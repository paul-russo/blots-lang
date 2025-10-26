#!/usr/bin/env python3
"""
Cross-check Blots unit conversions against Pint.

This script parses the unit catalogue from `blots-core/src/units.rs`,
derives each unit's conversion factor to the category base unit as
implemented by Blots, and compares it to the authoritative factor
obtained from Pint (a widely used unit conversion library).

Any mismatches, unmapped units, or identifier collisions are reported,
which makes it easier to spot discrepancies in the Blots data.
"""

from __future__ import annotations

import math
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Dict, Iterable, List, Optional, Tuple

try:
    from pint import UnitRegistry
    from pint.errors import DefinitionSyntaxError, DimensionalityError, UndefinedUnitError
except ImportError as exc:  # pragma: no cover - fails fast when dependency missing
    raise SystemExit(
        "The 'pint' package is required for scripts/check_unit_conversions.py.\n"
        "Install it with `python3 -m pip install pint` or create a virtualenv "
        "and install it there."
    ) from exc

UNITS_RS_PATH = Path("blots-core/src/units.rs")


@dataclass
class LinearUnit:
    category: str
    identifiers: List[str]
    coefficient: float
    mode: str
    pint_identifier: Optional[str] = None
    pint_unit: Optional[object] = None
    pint_scale: Optional[float] = None


@dataclass
class TemperatureUnit:
    identifiers: List[str]
    to_kelvin: Callable[[float], float]
    from_kelvin: Callable[[float], float]
    pint_identifier: Optional[str] = None
    pint_unit: Optional[object] = None
    pint_scale: Optional[float] = None


def parse_units() -> Tuple[List[LinearUnit], List[TemperatureUnit]]:
    text = UNITS_RS_PATH.read_text()

    linear_pattern = re.compile(
        r"Unit::new_linear\(\s*UnitCategory::(\w+),\s*&\[(.*?)\],\s*([^\)]+)\)",
        re.MULTILINE | re.DOTALL,
    )
    reciprocal_pattern = re.compile(
        r"Unit::new_reciprocal\(\s*UnitCategory::(\w+),\s*&\[(.*?)\],\s*([^\)]+)\)",
        re.MULTILINE | re.DOTALL,
    )
    temp_pattern = re.compile(
        r"Unit::new_temperature\(\s*&\[(.*?)\],\s*([^\s,]+),\s*([^\s\)]+)\)",
        re.MULTILINE,
    )

    def parse_identifier_block(block: str) -> List[str]:
        cleaned = []
        for line in block.splitlines():
            cleaned.append(line.split("//", 1)[0])
        literal = "[" + "".join(cleaned) + "]"
        # Evaluate as Python literal list.
        return eval(  # noqa: S307 - controlled literal from Rust array
            literal,
            {"__builtins__": {}},
        )

    def eval_coefficient(expr: str) -> float:
        expr = expr.split("//", 1)[0].strip()
        expr = expr.rstrip(",")
        expr = expr.replace("std::f64::consts::PI", "math.pi")
        expr = expr.replace("std::f64::consts::TAU", "2 * math.pi")
        expr = expr.replace("std::f64::consts::E", "math.e")
        return float(eval(expr, {"__builtins__": {}}, {"math": math}))

    linear_units: List[LinearUnit] = []
    for match in linear_pattern.finditer(text):
        category, ident_block, coeff_expr = match.groups()
        identifiers = parse_identifier_block(ident_block)
        coefficient = eval_coefficient(coeff_expr)
        linear_units.append(LinearUnit(category, identifiers, coefficient, "linear"))

    for match in reciprocal_pattern.finditer(text):
        category, ident_block, coeff_expr = match.groups()
        identifiers = parse_identifier_block(ident_block)
        coefficient = eval_coefficient(coeff_expr)
        linear_units.append(LinearUnit(category, identifiers, coefficient, "reciprocal"))

    temp_units: List[TemperatureUnit] = []
    temp_to_map: Dict[str, Callable[[float], float]] = {
        "kelvin_to_kelvin": lambda x: x,
        "celsius_to_kelvin": lambda c: c + 273.15,
        "fahrenheit_to_kelvin": lambda f: (f - 32.0) * 5.0 / 9.0 + 273.15,
    }
    temp_from_map: Dict[str, Callable[[float], float]] = {
        "kelvin_to_kelvin": lambda x: x,
        "kelvin_to_celsius": lambda k: k - 273.15,
        "kelvin_to_fahrenheit": lambda k: (k - 273.15) * 9.0 / 5.0 + 32.0,
    }
    for match in temp_pattern.finditer(text):
        ident_block, to_fn, from_fn = match.groups()
        identifiers = parse_identifier_block(ident_block)
        try:
            to_kelvin = temp_to_map[to_fn]
            from_kelvin = temp_from_map[from_fn]
        except KeyError as exc:
            raise ValueError(f"Unknown temperature conversion function {exc.args[0]!r}") from None
        temp_units.append(TemperatureUnit(identifiers, to_kelvin, from_kelvin))

    return linear_units, temp_units


BASE_UNITS: Dict[str, str] = {
    "Length": "meter",
    "Mass": "kilogram",
    "Area": "meter ** 2",
    "Volume": "liter",
    "Angle": "degree",
    "Duration": "second",
    "Speed": "meter / second",
    "Energy": "joule",
    "Power": "watt",
    "Pressure": "pascal",
    "Frequency": "hertz",
    "ElectricCharge": "coulomb",
    "ElectricCurrent": "ampere",
    "ElectricPotentialDifference": "volt",
    "ElectricResistance": "ohm",
    "ConcentrationMass": "gram / liter",
    "InformationStorage": "bit",
    "FuelEfficiency": "liter / kilometer",  # handled specially
}

CUSTOM_DEFINITIONS = [
    "scandinavian_mile = 10 * kilometer",
    "metric_cup = 250 * milliliter",
    "imperial_teaspoon = 5.91938802083 * milliliter",
    "imperial_tablespoon = 3 * imperial_teaspoon",
    "nibble = 4 * bit",
]

MANUAL_PINT_UNITS: Dict[Tuple[str, str], str] = {
    ("Length", "scandinavian miles"): "scandinavian_mile",
    ("Volume", "imperial teaspoons"): "imperial_teaspoon",
    ("Volume", "imperial tablespoons"): "imperial_tablespoon",
    ("Volume", "metric cups"): "metric_cup",
    ("Energy", "electronvolts"): "electron_volt",
    ("Pressure", "millimeters of mercury"): "mmHg",
    ("Pressure", "inches of mercury"): "inHg",
    ("Pressure", "pounds per square inch"): "psi",
    ("FuelEfficiency", "miles per imperial gallon"): "mile / imperial_gallon",
    ("FuelEfficiency", "liters per 100 kilometers"): "liter / (100 * kilometer)",
    ("InformationStorage", "nibbles"): "nibble",
}

BASE_ABS_TOL = 1e-6
BASE_REL_TOL = 5e-6


def identifier_variants(identifier: str) -> Iterable[str]:
    yield identifier
    if " per " in identifier:
        yield identifier.replace(" per ", " / ")
    yield identifier.replace(" ", "_")
    if identifier.endswith("s"):
        yield identifier[:-1]
    if identifier.endswith("es"):
        yield identifier[:-2]
    if identifier.endswith("ies"):
        yield identifier[:-3] + "y"
    trans = identifier.replace("²", "^2").replace("³", "^3")
    if trans != identifier:
        yield trans
    compressed = identifier.replace(" ", "")
    if compressed != identifier:
        yield compressed
    yield identifier.lower()


def normalize_power_notation(identifier: str) -> str:
    return identifier.replace("^2", "**2").replace("^3", "**3")


def blots_convert_linear(value: float, from_unit: LinearUnit, to_unit: LinearUnit) -> float:
    if from_unit.mode == "reciprocal":
        base = float("inf") if value == 0 else from_unit.coefficient / value
    else:
        base = value * from_unit.coefficient

    if to_unit.mode == "reciprocal":
        return float("inf") if base == 0 else to_unit.coefficient / base
    return base / to_unit.coefficient


def blots_convert_temperature(value: float, from_unit: TemperatureUnit, to_unit: TemperatureUnit) -> float:
    base = from_unit.to_kelvin(value)
    return to_unit.from_kelvin(base)


def pint_convert_linear(
    ureg: UnitRegistry,
    value: float,
    from_unit: LinearUnit,
    to_unit: LinearUnit,
) -> Optional[float]:
    if from_unit.pint_unit is None or to_unit.pint_unit is None:
        return None

    if from_unit.pint_scale is None or to_unit.pint_scale is None:
        return None

    if from_unit.category == "FuelEfficiency":
        return pint_convert_fuel_efficiency(ureg, value, from_unit, to_unit)

    qty = ureg.Quantity(value * from_unit.pint_scale, from_unit.pint_unit)
    converted = qty.to(to_unit.pint_unit)
    return float(converted.magnitude / to_unit.pint_scale)


def pint_convert_fuel_efficiency(
    ureg: UnitRegistry,
    value: float,
    from_unit: LinearUnit,
    to_unit: LinearUnit,
) -> Optional[float]:
    if from_unit.pint_unit is None or to_unit.pint_unit is None:
        return None
    if from_unit.pint_scale is None or to_unit.pint_scale is None:
        return None

    def is_consumption(unit: LinearUnit) -> bool:
        return unit.identifiers[0].lower().startswith("liters per 100")

    if is_consumption(from_unit):
        consumption = ureg.Quantity(value * from_unit.pint_scale, from_unit.pint_unit)
    else:
        efficiency = ureg.Quantity(value * from_unit.pint_scale, from_unit.pint_unit)
        consumption = (1 / efficiency).to("liter / kilometer")

    if is_consumption(to_unit):
        return float(consumption.magnitude / to_unit.pint_scale)

    efficiency = (1 / consumption).to(to_unit.pint_unit)
    return float(efficiency.magnitude / to_unit.pint_scale)


def pint_convert_temperature(
    ureg: UnitRegistry,
    value: float,
    from_unit: TemperatureUnit,
    to_unit: TemperatureUnit,
) -> Optional[float]:
    if from_unit.pint_unit is None or to_unit.pint_unit is None:
        return None

    qty = ureg.Quantity(value, from_unit.pint_unit)
    converted = qty.to(to_unit.pint_unit)
    return float(converted.magnitude)


def find_pint_quantity(ureg: UnitRegistry, identifiers: List[str]) -> Optional[Tuple[str, object]]:
    for identifier in identifiers:
        for variant in identifier_variants(identifier):
            candidate = normalize_power_notation(variant)
            try:
                qty = ureg(variant)
                return variant, qty
            except (UndefinedUnitError, DimensionalityError, ValueError):
                pass
            try:
                qty = ureg(candidate)
                return candidate, qty
            except (UndefinedUnitError, DimensionalityError, ValueError):
                continue
    return None


def find_temperature_quantity(
    ureg: UnitRegistry, identifiers: List[str]
) -> Optional[Tuple[str, object]]:
    mapping = {
        "kelvin": ureg.kelvin,
        "k": ureg.kelvin,
        "celsius": ureg.degC,
        "°c": ureg.degC,
        "c": ureg.degC,
        "fahrenheit": ureg.degF,
        "°f": ureg.degF,
        "f": ureg.degF,
    }
    for identifier in identifiers:
        key = identifier.lower()
        if key in mapping:
            return identifier, ureg.Quantity(1, mapping[key])
    return None


def compute_expected_coefficient(
    ureg: UnitRegistry,
    pint_qty,
    base: str,
    category: str,
    identifier: str,
) -> Optional[float]:
    if category == "FuelEfficiency":
        if "liter" in identifier and "100" in identifier:
            return 1.0
        # Efficiency units (e.g., miles per gallon) are reciprocal of consumption.
        consumption = (1 / pint_qty).to("liter / kilometer")
        return float(consumption.magnitude * 100.0)

    try:
        converted = pint_qty.to(base)
    except DimensionalityError:
        return None
    return float(converted.magnitude)


def main() -> None:
    linear_units, temp_units = parse_units()
    ureg = UnitRegistry()
    for definition in CUSTOM_DEFINITIONS:
        try:
            ureg.define(definition)
        except DefinitionSyntaxError:
            # Definition may already exist if the registry ships with it.
            pass

    from collections import defaultdict

    duplicate_entries: Dict[str, List[Tuple[str, str, str]]] = defaultdict(list)
    for unit in linear_units:
        for identifier in unit.identifiers:
            key = identifier.lower()
            duplicate_entries[key].append((identifier, unit.category, unit.identifiers[0]))
    for unit in temp_units:
        for identifier in unit.identifiers:
            key = identifier.lower()
            duplicate_entries[key].append((identifier, "Temperature", unit.identifiers[0]))

    exact_conflicts: Dict[str, List[Tuple[str, str, str]]] = {}
    case_conflicts: Dict[str, List[Tuple[str, str, str]]] = {}
    for key, entries in duplicate_entries.items():
        unique_units = {(category, canonical) for _, category, canonical in entries}
        if len(unique_units) <= 1:
            continue
        unique_aliases = {alias for alias, _, _ in entries}
        if len(unique_aliases) == 1:
            exact_conflicts[key] = entries
        else:
            case_conflicts[key] = entries

    results = []

    for unit in linear_units:
        base = BASE_UNITS.get(unit.category)
        if base is None:
            results.append(
                {
                    "category": unit.category,
                    "unit": unit.identifiers[0],
                    "status": "no-base-mapping",
                    "detail": "No Pint base unit mapping configured",
                }
            )
            continue

        manual_key = (unit.category, unit.identifiers[0])
        manual_expr = MANUAL_PINT_UNITS.get(manual_key)
        if manual_expr is not None:
            pint_identifier, pint_qty = manual_expr, ureg(manual_expr)
        else:
            pint_info = find_pint_quantity(ureg, unit.identifiers)
            if pint_info is None:
                results.append(
                    {
                        "category": unit.category,
                        "unit": unit.identifiers[0],
                        "status": "unmapped",
                        "detail": "Unable to locate Pint equivalent",
                    }
                )
                continue
            pint_identifier, pint_qty = pint_info

        unit.pint_identifier = pint_identifier
        unit.pint_unit = pint_qty.units
        unit.pint_scale = float(pint_qty.magnitude)

        expected = compute_expected_coefficient(ureg, pint_qty, base, unit.category, pint_identifier)
        if expected is None:
            results.append(
                {
                    "category": unit.category,
                    "unit": unit.identifiers[0],
                    "status": "dimension-mismatch",
                    "pint_identifier": pint_identifier,
                    "detail": "Dimensionality mismatch when converting with Pint",
                }
            )
            continue

        diff = abs(unit.coefficient - expected)
        rel = diff / expected if expected else float("inf")
        status = "ok" if diff <= BASE_ABS_TOL or rel <= BASE_REL_TOL else "mismatch"
        results.append(
            {
                "category": unit.category,
                "unit": unit.identifiers[0],
                "status": status,
                "blots": unit.coefficient,
                "pint": expected,
                "abs_diff": diff,
                "rel_diff": rel,
                "pint_identifier": pint_identifier,
            }
        )

    for unit in temp_units:
        base = "kelvin"
        pint_info = find_temperature_quantity(ureg, unit.identifiers)
        if pint_info is None:
            results.append(
                {
                    "category": "Temperature",
                    "unit": unit.identifiers[0],
                    "status": "unmapped",
                    "detail": "Unable to locate Pint temperature unit",
                }
            )
            continue

        pint_identifier, pint_qty = pint_info
        unit.pint_identifier = pint_identifier
        unit.pint_unit = pint_qty.units
        unit.pint_scale = float(pint_qty.magnitude)
        expected = float(pint_qty.to(base).magnitude)
        blots_value = unit.to_kelvin(1.0)
        diff = abs(blots_value - expected)
        rel = diff / expected if expected else float("inf")
        status = "ok" if diff <= 1e-9 else "mismatch"
        results.append(
            {
                "category": "Temperature",
                "unit": unit.identifiers[0],
                "status": status,
                "blots": blots_value,
                "pint": expected,
                "abs_diff": diff,
                "rel_diff": rel,
                "pint_identifier": pint_identifier,
            }
        )

    mismatches = [r for r in results if r["status"] == "mismatch"]
    unmapped = [r for r in results if r["status"] in {"unmapped", "dimension-mismatch", "no-base-mapping"}]

    print("=== Duplicate identifiers ===")
    if not exact_conflicts and not case_conflicts:
        print("None")
    else:
        if exact_conflicts:
            print("Exact conflicts (must be renamed):")
            for key, entries in sorted(exact_conflicts.items()):
                alias = entries[0][0]
                details = sorted({f"{category}:{canonical}" for _, category, canonical in entries})
                print(f"  {alias} -> {', '.join(details)}")
        if case_conflicts:
            print("Case-sensitive groups (match casing to disambiguate):")
            for key, entries in sorted(case_conflicts.items()):
                options = sorted(
                    {
                        f"'{alias}' ({category}:{canonical})"
                        for alias, category, canonical in entries
                    }
                )
                print(f"  {key} -> {', '.join(options)}")
    print()

    print("=== Mismatched units ===")
    if not mismatches:
        print("None")
    else:
        for item in sorted(mismatches, key=lambda x: (-abs(x["abs_diff"]), x["category"], x["unit"])):
            print(
                f"[{item['category']}] {item['unit']} "
                f"(Blots {item['blots']}, Pint {item['pint']}, "
                f"Δ={item['abs_diff']}, rel={item['rel_diff']}) via '{item['pint_identifier']}'"
            )
    print()

    print("=== Unmapped / problematic units ===")
    if not unmapped:
        print("None")
    else:
        for item in unmapped:
            print(f"[{item['category']}] {item['unit']}: {item['status']} ({item.get('detail','')})")
    print()

    # Pairwise conversion checks
    pair_failures = []
    test_values = [0.5, 1.0, 42.0]
    abs_tol = BASE_ABS_TOL
    rel_tol = BASE_REL_TOL

    from collections import defaultdict

    category_groups: Dict[str, List[LinearUnit]] = defaultdict(list)
    for unit in linear_units:
        if unit.pint_unit is not None:
            category_groups[unit.category].append(unit)

    for category, units in category_groups.items():
        for from_unit in units:
            for to_unit in units:
                for value in test_values:
                    blots_val = blots_convert_linear(value, from_unit, to_unit)
                    pint_val = pint_convert_linear(ureg, value, from_unit, to_unit)
                    if pint_val is None:
                        continue
                    diff = abs(blots_val - pint_val)
                    rel = diff / max(abs(pint_val), 1.0)
                    if diff > abs_tol and rel > rel_tol:
                        pair_failures.append(
                            {
                                "category": category,
                                "from": from_unit.identifiers[0],
                                "to": to_unit.identifiers[0],
                                "value": value,
                                "blots": blots_val,
                                "pint": pint_val,
                                "abs_diff": diff,
                                "rel_diff": rel,
                            }
                        )
                        break

    temp_failures = []
    if temp_units:
        for from_unit in temp_units:
            if from_unit.pint_unit is None:
                continue
            for to_unit in temp_units:
                if to_unit.pint_unit is None:
                    continue
                for value in [-40.0, 0.0, 100.0]:
                    blots_val = blots_convert_temperature(value, from_unit, to_unit)
                    pint_val = pint_convert_temperature(ureg, value, from_unit, to_unit)
                    if pint_val is None:
                        continue
                    diff = abs(blots_val - pint_val)
                    rel = diff / max(abs(pint_val), 1.0)
                    if diff > abs_tol and rel > rel_tol:
                        temp_failures.append(
                            {
                                "from": from_unit.identifiers[0],
                                "to": to_unit.identifiers[0],
                                "value": value,
                                "blots": blots_val,
                                "pint": pint_val,
                                "abs_diff": diff,
                                "rel_diff": rel,
                            }
                        )
                        break

    print("=== Conversion mismatches ===")
    if not pair_failures and not temp_failures:
        print("None")
    else:
        for item in pair_failures:
            print(
                f"[{item['category']}] {item['value']} {item['from']} -> {item['to']}: "
                f"Blots {item['blots']}, Pint {item['pint']} (Δ={item['abs_diff']}, rel={item['rel_diff']})"
            )
        for item in temp_failures:
            print(
                f"[Temperature] {item['value']} {item['from']} -> {item['to']}: "
                f"Blots {item['blots']}, Pint {item['pint']} (Δ={item['abs_diff']}, rel={item['rel_diff']})"
            )
    print()

    ok_count = sum(1 for r in results if r["status"] == "ok")
    total = len(results)
    print(f"Matched units: {ok_count}/{total}")

    has_failures = bool(
        exact_conflicts
        or mismatches
        or unmapped
        or pair_failures
        or temp_failures
    )

    if has_failures:
        sys.exit(1)


if __name__ == "__main__":
    main()
