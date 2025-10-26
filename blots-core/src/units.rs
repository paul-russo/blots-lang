use anyhow::Result;
use anyhow::anyhow;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnitCategory {
    Angle,
    Area,
    ConcentrationMass,
    Duration,
    ElectricCharge,
    ElectricCurrent,
    ElectricPotentialDifference,
    ElectricResistance,
    Energy,
    Frequency,
    FuelEfficiency,
    InformationStorage,
    Length,
    Mass,
    Power,
    Pressure,
    Speed,
    Temperature,
    Volume,
}

impl UnitCategory {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Angle => "angle",
            Self::Area => "area",
            Self::ConcentrationMass => "concentration of mass",
            Self::Duration => "duration",
            Self::ElectricCharge => "electric charge",
            Self::ElectricCurrent => "electric current",
            Self::ElectricPotentialDifference => "electric potential difference",
            Self::ElectricResistance => "electric resistance",
            Self::Energy => "energy",
            Self::Frequency => "frequency",
            Self::FuelEfficiency => "fuel efficiency",
            Self::InformationStorage => "information storage",
            Self::Length => "length",
            Self::Mass => "mass",
            Self::Power => "power",
            Self::Pressure => "pressure",
            Self::Speed => "speed",
            Self::Temperature => "temperature",
            Self::Volume => "volume",
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConversionType {
    /// Linear conversion: value * coefficient converts to base unit
    Linear { coefficient: f64 },
    /// Reciprocal conversion: coefficient / value converts to base unit
    Reciprocal { coefficient: f64 },
    /// Temperature conversion requires special formulas
    Temperature {
        to_kelvin: fn(f64) -> f64,
        from_kelvin: fn(f64) -> f64,
    },
}

#[derive(Debug, Clone)]
pub struct Unit {
    pub category: UnitCategory,
    pub identifiers: &'static [&'static str],
    pub conversion: ConversionType,
}

impl Unit {
    fn new_linear(
        category: UnitCategory,
        identifiers: &'static [&'static str],
        coefficient: f64,
    ) -> Self {
        Self {
            category,
            identifiers,
            conversion: ConversionType::Linear { coefficient },
        }
    }

    fn new_reciprocal(
        category: UnitCategory,
        identifiers: &'static [&'static str],
        coefficient: f64,
    ) -> Self {
        Self {
            category,
            identifiers,
            conversion: ConversionType::Reciprocal { coefficient },
        }
    }

    fn new_temperature(
        identifiers: &'static [&'static str],
        to_kelvin: fn(f64) -> f64,
        from_kelvin: fn(f64) -> f64,
    ) -> Self {
        Self {
            category: UnitCategory::Temperature,
            identifiers,
            conversion: ConversionType::Temperature {
                to_kelvin,
                from_kelvin,
            },
        }
    }

    pub fn matches(&self, identifier: &str) -> bool {
        if self.matches_exact(identifier) {
            return true;
        }
        let id_lower = identifier.to_lowercase();
        self.identifiers
            .iter()
            .any(|&i| i.to_lowercase() == id_lower)
    }

    pub fn matches_exact(&self, identifier: &str) -> bool {
        self.identifiers.iter().any(|&i| i == identifier)
    }

    pub fn convert_to_base(&self, value: f64) -> f64 {
        match &self.conversion {
            ConversionType::Linear { coefficient } => value * coefficient,
            ConversionType::Reciprocal { coefficient } => {
                if value == 0.0 {
                    f64::INFINITY
                } else {
                    coefficient / value
                }
            }
            ConversionType::Temperature { to_kelvin, .. } => to_kelvin(value),
        }
    }

    pub fn convert_from_base(&self, value: f64) -> f64 {
        match &self.conversion {
            ConversionType::Linear { coefficient } => value / coefficient,
            ConversionType::Reciprocal { coefficient } => {
                if value == 0.0 {
                    f64::INFINITY
                } else {
                    coefficient / value
                }
            }
            ConversionType::Temperature { from_kelvin, .. } => from_kelvin(value),
        }
    }
}

// Temperature conversion functions
fn celsius_to_kelvin(c: f64) -> f64 {
    c + 273.15
}

fn kelvin_to_celsius(k: f64) -> f64 {
    k - 273.15
}

fn fahrenheit_to_kelvin(f: f64) -> f64 {
    (f - 32.0) * 5.0 / 9.0 + 273.15
}

fn kelvin_to_fahrenheit(k: f64) -> f64 {
    (k - 273.15) * 9.0 / 5.0 + 32.0
}

fn kelvin_to_kelvin(k: f64) -> f64 {
    k
}

pub fn get_all_units() -> Vec<Unit> {
    vec![
        // Temperature units
        Unit::new_temperature(&["kelvin", "k"], kelvin_to_kelvin, kelvin_to_kelvin),
        Unit::new_temperature(
            &["celsius", "°c", "c"],
            celsius_to_kelvin,
            kelvin_to_celsius,
        ),
        Unit::new_temperature(
            &["fahrenheit", "°f", "f"],
            fahrenheit_to_kelvin,
            kelvin_to_fahrenheit,
        ),
        // Length units (base: meters)
        Unit::new_linear(
            UnitCategory::Length,
            &["meters", "meter", "m", "metres", "metre"],
            1.0,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &["kilometers", "kilometer", "km", "kilometres", "kilometre"],
            1000.0,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &[
                "centimeters",
                "centimeter",
                "cm",
                "centimetres",
                "centimetre",
            ],
            0.01,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &[
                "millimeters",
                "millimeter",
                "mm",
                "millimetres",
                "millimetre",
            ],
            0.001,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &[
                "micrometers",
                "micrometer",
                "μm",
                "micrometres",
                "micrometre",
                "um",
            ],
            1e-6,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &["nanometers", "nanometer", "nm", "nanometres", "nanometre"],
            1e-9,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &["picometers", "picometer", "pm", "picometres", "picometre"],
            1e-12,
        ),
        Unit::new_linear(UnitCategory::Length, &["inches", "in", "inch"], 0.0254),
        Unit::new_linear(UnitCategory::Length, &["feet", "ft", "foot"], 0.3048),
        Unit::new_linear(UnitCategory::Length, &["yards", "yd", "yard"], 0.9144),
        Unit::new_linear(UnitCategory::Length, &["miles", "mi", "mile"], 1609.344),
        Unit::new_linear(
            UnitCategory::Length,
            &["scandinavian miles", "scandinavian mile"],
            10000.0,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &["light years", "light year", "ly"],
            9.460_730_472_580_8e15,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &["nautical miles", "nautical mile", "nmi"],
            1852.0,
        ),
        Unit::new_linear(UnitCategory::Length, &["fathoms", "fathom"], 1.8288),
        Unit::new_linear(UnitCategory::Length, &["furlongs", "furlong"], 201.168),
        Unit::new_linear(
            UnitCategory::Length,
            &["astronomical units", "astronomical unit", "au"],
            149_597_870_700.0,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &["parsecs", "parsec", "pc"],
            3.085_677_581_467_191_6e16,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &["megameters", "megameter", "Mm"],
            1e6,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &["hectometers", "hectometer", "hm"],
            100.0,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &["decameters", "decameter", "dam"],
            10.0,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &["decimeters", "decimeter", "dm"],
            0.1,
        ),
        Unit::new_linear(
            UnitCategory::Length,
            &["thousandths of an inch", "thou", "mil"],
            0.0000254,
        ),
        // Mass units (base: kilograms)
        Unit::new_linear(UnitCategory::Mass, &["kilograms", "kg", "kilogram"], 1.0),
        Unit::new_linear(UnitCategory::Mass, &["grams", "g", "gram"], 0.001),
        Unit::new_linear(UnitCategory::Mass, &["milligrams", "mg", "milligram"], 1e-6),
        Unit::new_linear(
            UnitCategory::Mass,
            &["micrograms", "μg", "microgram", "ug"],
            1e-9,
        ),
        Unit::new_linear(UnitCategory::Mass, &["nanograms", "ng", "nanogram"], 1e-12),
        Unit::new_linear(UnitCategory::Mass, &["picograms", "pg", "picogram"], 1e-15),
        Unit::new_linear(
            UnitCategory::Mass,
            &["ounces", "oz", "ounce"],
            0.028349523125,
        ),
        Unit::new_linear(
            UnitCategory::Mass,
            &["pounds", "lb", "lbs", "pound"],
            0.45359237,
        ),
        Unit::new_linear(UnitCategory::Mass, &["stones", "st", "stone"], 6.35029318),
        Unit::new_linear(
            UnitCategory::Mass,
            &["metric tons", "metric ton", "tonnes", "tonne", "t"],
            1000.0,
        ),
        Unit::new_linear(
            UnitCategory::Mass,
            &["short tons", "short ton", "tons", "ton"],
            907.18474,
        ),
        Unit::new_linear(UnitCategory::Mass, &["carats", "ct", "carat"], 0.0002),
        Unit::new_linear(
            UnitCategory::Mass,
            &["troy ounces", "troy ounce", "oz t"],
            0.0311034768,
        ),
        Unit::new_linear(UnitCategory::Mass, &["slugs", "slug"], 14.593903),
        // Area units (base: square meters)
        Unit::new_linear(
            UnitCategory::Area,
            &[
                "square meters",
                "square meter",
                "m²",
                "m2",
                "square metres",
                "square metre",
            ],
            1.0,
        ),
        Unit::new_linear(
            UnitCategory::Area,
            &[
                "square kilometers",
                "square kilometer",
                "km²",
                "km2",
                "square kilometres",
                "square kilometre",
            ],
            1e6,
        ),
        Unit::new_linear(
            UnitCategory::Area,
            &[
                "square centimeters",
                "square centimeter",
                "cm²",
                "cm2",
                "square centimetres",
                "square centimetre",
            ],
            1e-4,
        ),
        Unit::new_linear(
            UnitCategory::Area,
            &[
                "square millimeters",
                "square millimeter",
                "mm²",
                "mm2",
                "square millimetres",
                "square millimetre",
            ],
            1e-6,
        ),
        Unit::new_linear(
            UnitCategory::Area,
            &[
                "square micrometers",
                "square micrometer",
                "μm²",
                "μm2",
                "square micrometres",
                "square micrometre",
                "um2",
            ],
            1e-12,
        ),
        Unit::new_linear(
            UnitCategory::Area,
            &[
                "square nanometers",
                "square nanometer",
                "nm²",
                "nm2",
                "square nanometres",
                "square nanometre",
            ],
            1e-18,
        ),
        Unit::new_linear(
            UnitCategory::Area,
            &["square inches", "in²", "in2", "square inch"],
            0.00064516,
        ),
        Unit::new_linear(
            UnitCategory::Area,
            &["square feet", "ft²", "ft2", "square foot"],
            0.09290304,
        ),
        Unit::new_linear(
            UnitCategory::Area,
            &["square yards", "yd²", "yd2", "square yard"],
            0.83612736,
        ),
        Unit::new_linear(
            UnitCategory::Area,
            &["square miles", "mi²", "mi2", "square mile"],
            2_589_988.110_336,
        ),
        Unit::new_linear(UnitCategory::Area, &["acres", "acre", "ac"], 4046.856_422_4),
        Unit::new_linear(UnitCategory::Area, &["ares", "are", "a"], 100.0),
        Unit::new_linear(UnitCategory::Area, &["hectares", "hectare", "ha"], 10000.0),
        Unit::new_linear(
            UnitCategory::Area,
            &["square megameters", "square megameter", "Mm²", "Mm2"],
            1e12,
        ),
        // Volume units (base: liters)
        Unit::new_linear(
            UnitCategory::Volume,
            &["liters", "l", "liter", "litre", "litres"],
            1.0,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &[
                "milliliters",
                "ml",
                "milliliter",
                "millilitre",
                "millilitres",
            ],
            0.001,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &[
                "centiliters",
                "cl",
                "centiliter",
                "centilitre",
                "centilitres",
            ],
            0.01,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["deciliters", "dl", "deciliter", "decilitre", "decilitres"],
            0.1,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["kiloliters", "kl", "kiloliter", "kilolitre", "kilolitres"],
            1000.0,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["megaliter", "megaliters", "ML"],
            1e6,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &[
                "cubic meters",
                "cubic meter",
                "m³",
                "m3",
                "cubic metres",
                "cubic metre",
            ],
            1000.0,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &[
                "cubic kilometers",
                "cubic kilometer",
                "km³",
                "km3",
                "cubic kilometres",
                "cubic kilometre",
            ],
            1e12,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &[
                "cubic centimeters",
                "cubic centimeter",
                "cm³",
                "cm3",
                "cc",
                "cubic centimetres",
                "cubic centimetre",
            ],
            0.001,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &[
                "cubic millimeters",
                "cubic millimeter",
                "mm³",
                "mm3",
                "cubic millimetres",
                "cubic millimetre",
            ],
            1e-6,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["cubic inches", "in³", "in3", "cubic inch"],
            0.016387064,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["cubic feet", "ft³", "ft3", "cubic foot"],
            28.316846592,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["cubic yards", "yd³", "yd3", "cubic yard"],
            764.554857984,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["cubic miles", "mi³", "mi3", "cubic mile"],
            4.168_181_825_440_58e12,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["acre feet", "acre foot", "acre ft"],
            1_233_481.837_547_52,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["bushels", "bushel", "bsh"],
            35.23907016688,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["teaspoons", "tsp", "teaspoon"],
            0.00492892159375,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["tablespoons", "tbsp", "tablespoon"],
            0.01478676478125,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["fluid ounces", "fl oz", "fluid ounce"],
            0.0295735295625,
        ),
        Unit::new_linear(UnitCategory::Volume, &["cups", "cup"], 0.236_588_236_5),
        Unit::new_linear(UnitCategory::Volume, &["pints", "pt", "pint"], 0.473176473),
        Unit::new_linear(
            UnitCategory::Volume,
            &["quarts", "qt", "quart"],
            0.946352946,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["gallons", "gal", "gallon"],
            3.785411784,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["imperial teaspoons", "imperial teaspoon", "imp tsp"],
            0.00591938802083,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["imperial tablespoons", "imperial tablespoon", "imp tbsp"],
            0.0177581640625,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["imperial fluid ounces", "imperial fluid ounce", "imp fl oz"],
            0.0284130625,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["imperial pints", "imperial pint", "imp pt"],
            0.56826125,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["imperial quarts", "imperial quart", "imp qt"],
            1.1365225,
        ),
        Unit::new_linear(
            UnitCategory::Volume,
            &["imperial gallons", "imperial gallon", "imp gal"],
            4.54609,
        ),
        Unit::new_linear(UnitCategory::Volume, &["metric cups", "metric cup"], 0.25),
        // Angle units (base: degrees)
        Unit::new_linear(UnitCategory::Angle, &["degrees", "deg", "°", "degree"], 1.0),
        Unit::new_linear(
            UnitCategory::Angle,
            &["arc minutes", "arcminutes", "arcmin", "′", "arc minute"],
            1.0 / 60.0,
        ),
        Unit::new_linear(
            UnitCategory::Angle,
            &["arc seconds", "arcseconds", "arcsec", "″", "arc second"],
            1.0 / 3600.0,
        ),
        Unit::new_linear(
            UnitCategory::Angle,
            &["radians", "rad", "radian"],
            180.0 / std::f64::consts::PI,
        ),
        Unit::new_linear(
            UnitCategory::Angle,
            &["gradians", "grad", "gradian", "gon"],
            0.9,
        ),
        Unit::new_linear(
            UnitCategory::Angle,
            &["revolutions", "rev", "revolution", "rotation"],
            360.0,
        ),
        // Duration units (base: seconds)
        Unit::new_linear(
            UnitCategory::Duration,
            &["seconds", "s", "sec", "second"],
            1.0,
        ),
        Unit::new_linear(
            UnitCategory::Duration,
            &["milliseconds", "ms", "millisecond"],
            0.001,
        ),
        Unit::new_linear(
            UnitCategory::Duration,
            &["microseconds", "μs", "microsecond", "us"],
            1e-6,
        ),
        Unit::new_linear(
            UnitCategory::Duration,
            &["nanoseconds", "ns", "nanosecond"],
            1e-9,
        ),
        Unit::new_linear(
            UnitCategory::Duration,
            &["picoseconds", "ps", "picosecond"],
            1e-12,
        ),
        Unit::new_linear(UnitCategory::Duration, &["minutes", "min", "minute"], 60.0),
        Unit::new_linear(
            UnitCategory::Duration,
            &["hours", "h", "hr", "hour"],
            3600.0,
        ),
        Unit::new_linear(UnitCategory::Duration, &["days", "d", "day"], 86400.0),
        Unit::new_linear(UnitCategory::Duration, &["weeks", "wk", "week"], 604800.0),
        // Speed units (base: meters per second)
        Unit::new_linear(
            UnitCategory::Speed,
            &["meters per second", "m/s", "mps"],
            1.0,
        ),
        Unit::new_linear(
            UnitCategory::Speed,
            &["kilometers per hour", "km/h", "kph", "kmph"],
            1.0 / 3.6,
        ),
        Unit::new_linear(
            UnitCategory::Speed,
            &["miles per hour", "mph", "mi/h"],
            0.44704,
        ),
        Unit::new_linear(
            UnitCategory::Speed,
            &["knots", "knot", "kn"],
            0.514_444_444_444_444_5,
        ),
        // Energy units (base: joules)
        Unit::new_linear(UnitCategory::Energy, &["joules", "j", "joule"], 1.0),
        Unit::new_linear(
            UnitCategory::Energy,
            &["kilojoules", "kj", "kilojoule"],
            1000.0,
        ),
        Unit::new_linear(UnitCategory::Energy, &["calories", "cal", "calorie"], 4.184),
        Unit::new_linear(
            UnitCategory::Energy,
            &["kilocalories", "kcal", "kilocalorie", "food calorie", "Cal"],
            4184.0,
        ),
        Unit::new_linear(
            UnitCategory::Energy,
            &["watt hours", "wh", "watt hour"],
            3600.0,
        ),
        Unit::new_linear(
            UnitCategory::Energy,
            &["kilowatt hours", "kwh", "kilowatt hour"],
            3.6e6,
        ),
        Unit::new_linear(
            UnitCategory::Energy,
            &["electronvolts", "ev", "electronvolt"],
            1.602176634e-19,
        ),
        // Power units (base: watts)
        Unit::new_linear(UnitCategory::Power, &["watts", "w", "watt"], 1.0),
        Unit::new_linear(
            UnitCategory::Power,
            &["milliwatts", "mw", "milliwatt"],
            0.001,
        ),
        Unit::new_linear(
            UnitCategory::Power,
            &["microwatts", "μw", "microwatt", "uw"],
            1e-6,
        ),
        Unit::new_linear(UnitCategory::Power, &["nanowatts", "nw", "nanowatt"], 1e-9),
        Unit::new_linear(UnitCategory::Power, &["picowatts", "pw", "picowatt"], 1e-12),
        Unit::new_linear(
            UnitCategory::Power,
            &["femtowatts", "fw", "femtowatt"],
            1e-15,
        ),
        Unit::new_linear(
            UnitCategory::Power,
            &["kilowatts", "kw", "kilowatt"],
            1000.0,
        ),
        Unit::new_linear(UnitCategory::Power, &["megawatts", "MW", "megawatt"], 1e6),
        Unit::new_linear(UnitCategory::Power, &["gigawatts", "GW", "gigawatt"], 1e9),
        Unit::new_linear(UnitCategory::Power, &["terawatts", "TW", "terawatt"], 1e12),
        Unit::new_linear(
            UnitCategory::Power,
            &["horsepower", "hp"],
            745.699_871_582_270_1,
        ),
        // Pressure units (base: pascals)
        Unit::new_linear(UnitCategory::Pressure, &["pascals", "pa", "pascal"], 1.0),
        Unit::new_linear(
            UnitCategory::Pressure,
            &["hectopascals", "hpa", "hectopascal"],
            100.0,
        ),
        Unit::new_linear(
            UnitCategory::Pressure,
            &["kilopascals", "kpa", "kilopascal"],
            1000.0,
        ),
        Unit::new_linear(
            UnitCategory::Pressure,
            &["megapascals", "mpa", "megapascal"],
            1e6,
        ),
        Unit::new_linear(
            UnitCategory::Pressure,
            &["gigapascals", "gpa", "gigapascal"],
            1e9,
        ),
        Unit::new_linear(UnitCategory::Pressure, &["bars", "bar"], 100000.0),
        Unit::new_linear(
            UnitCategory::Pressure,
            &["millibars", "mbar", "millibar"],
            100.0,
        ),
        Unit::new_linear(
            UnitCategory::Pressure,
            &["millimeters of mercury", "mmhg", "mm hg"],
            133.322387415,
        ),
        Unit::new_linear(
            UnitCategory::Pressure,
            &["inches of mercury", "inhg", "in hg"],
            3386.389,
        ),
        Unit::new_linear(
            UnitCategory::Pressure,
            &["pounds per square inch", "psi", "lbf/in²"],
            6894.757293168,
        ),
        Unit::new_linear(
            UnitCategory::Pressure,
            &["atmospheres", "atm", "atmosphere"],
            101325.0,
        ),
        // Frequency units (base: hertz)
        Unit::new_linear(UnitCategory::Frequency, &["hertz", "hz", "Hz"], 1.0),
        Unit::new_linear(UnitCategory::Frequency, &["millihertz", "mHz"], 0.001),
        Unit::new_linear(UnitCategory::Frequency, &["microhertz", "µHz", "uHz"], 1e-6),
        Unit::new_linear(UnitCategory::Frequency, &["nanohertz", "nHz"], 1e-9),
        Unit::new_linear(UnitCategory::Frequency, &["kilohertz", "kHz"], 1000.0),
        Unit::new_linear(UnitCategory::Frequency, &["megahertz", "MHz"], 1e6),
        Unit::new_linear(UnitCategory::Frequency, &["gigahertz", "GHz"], 1e9),
        Unit::new_linear(UnitCategory::Frequency, &["terahertz", "THz"], 1e12),
        // Electric Charge units (base: coulombs)
        Unit::new_linear(
            UnitCategory::ElectricCharge,
            &["coulombs", "c", "coulomb"],
            1.0,
        ),
        Unit::new_linear(
            UnitCategory::ElectricCharge,
            &["megaampere hours", "MAh"],
            3.6e9,
        ),
        Unit::new_linear(
            UnitCategory::ElectricCharge,
            &["kiloampere hours", "kAh"],
            3.6e6,
        ),
        Unit::new_linear(
            UnitCategory::ElectricCharge,
            &["ampere hours", "Ah"],
            3600.0,
        ),
        Unit::new_linear(
            UnitCategory::ElectricCharge,
            &["milliampere hours", "mAh"],
            3.6,
        ),
        Unit::new_linear(
            UnitCategory::ElectricCharge,
            &["microampere hours", "µAh", "uAh"],
            0.0036,
        ),
        // Electric Current units (base: amperes)
        Unit::new_linear(
            UnitCategory::ElectricCurrent,
            &["amperes", "A", "amp", "amps", "ampere"],
            1.0,
        ),
        Unit::new_linear(UnitCategory::ElectricCurrent, &["megaamperes", "MA"], 1e6),
        Unit::new_linear(
            UnitCategory::ElectricCurrent,
            &["kiloamperes", "kA"],
            1000.0,
        ),
        Unit::new_linear(
            UnitCategory::ElectricCurrent,
            &["milliamperes", "mA"],
            0.001,
        ),
        Unit::new_linear(
            UnitCategory::ElectricCurrent,
            &["microamperes", "µA", "uA"],
            1e-6,
        ),
        // Electric Potential Difference units (base: volts)
        Unit::new_linear(
            UnitCategory::ElectricPotentialDifference,
            &["volts", "V", "volt", "v"],
            1.0,
        ),
        Unit::new_linear(
            UnitCategory::ElectricPotentialDifference,
            &["megavolts", "MV"],
            1e6,
        ),
        Unit::new_linear(
            UnitCategory::ElectricPotentialDifference,
            &["kilovolts", "kV"],
            1000.0,
        ),
        Unit::new_linear(
            UnitCategory::ElectricPotentialDifference,
            &["millivolts", "mV"],
            0.001,
        ),
        Unit::new_linear(
            UnitCategory::ElectricPotentialDifference,
            &["microvolts", "µV", "uV"],
            1e-6,
        ),
        // Electric Resistance units (base: ohms)
        Unit::new_linear(
            UnitCategory::ElectricResistance,
            &["ohms", "Ω", "ohm", "ω"],
            1.0,
        ),
        Unit::new_linear(
            UnitCategory::ElectricResistance,
            &["megaohms", "MΩ", "megohm"],
            1e6,
        ),
        Unit::new_linear(
            UnitCategory::ElectricResistance,
            &["kiloohms", "kΩ", "kilohm"],
            1000.0,
        ),
        Unit::new_linear(
            UnitCategory::ElectricResistance,
            &["milliohms", "mΩ", "milliohm"],
            0.001,
        ),
        Unit::new_linear(
            UnitCategory::ElectricResistance,
            &["microohms", "µΩ", "microhm", "uΩ"],
            1e-6,
        ),
        // Concentration of Mass units (base: grams per liter)
        Unit::new_linear(
            UnitCategory::ConcentrationMass,
            &["grams per liter", "g/l"],
            1.0,
        ),
        Unit::new_linear(
            UnitCategory::ConcentrationMass,
            &["milligrams per deciliter", "mg/dl"],
            0.01,
        ),
        // Fuel Efficiency units (base: liters per 100 kilometers)
        Unit::new_linear(
            UnitCategory::FuelEfficiency,
            &["liters per 100 kilometers", "l/100km"],
            1.0,
        ),
        Unit::new_reciprocal(
            UnitCategory::FuelEfficiency,
            &["miles per gallon", "mpg"],
            235.214_583_333_333_34,
        ),
        Unit::new_reciprocal(
            UnitCategory::FuelEfficiency,
            &["miles per imperial gallon", "imp mpg"],
            282.480_936_331_822_2,
        ),
        // Information Storage units (base: bits)
        Unit::new_linear(UnitCategory::InformationStorage, &["bits", "bit", "b"], 1.0),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["nibbles", "nibble"],
            4.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["bytes", "byte", "B"],
            8.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["kilobits", "kbit", "kb"],
            1000.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["megabits", "mbit", "mb"],
            1e6,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["gigabits", "gbit", "gb"],
            1e9,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["terabits", "tbit", "tb"],
            1e12,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["petabits", "pbit", "pb"],
            1e15,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["exabits", "ebit", "eb"],
            1e18,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["zettabits", "zbit", "zb"],
            1e21,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["yottabits", "ybit", "yb"],
            1e24,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["kilobytes", "kbyte", "kB"],
            8000.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["megabytes", "mbyte", "MB"],
            8e6,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["gigabytes", "gbyte", "GB"],
            8e9,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["terabytes", "tbyte", "TB"],
            8e12,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["petabytes", "pbyte", "PB"],
            8e15,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["exabytes", "ebyte", "EB"],
            8e18,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["zettabytes", "zbyte", "ZB"],
            8e21,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["yottabytes", "ybyte", "YB"],
            8e24,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["kibibits", "kibit", "kib"],
            1024.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["mebibits", "mibit", "mib"],
            1048576.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["gibibits", "gibit", "gib"],
            1073741824.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["tebibits", "tibit", "tib"],
            1099511627776.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["pebibits", "pibit", "pib"],
            1125899906842624.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["exbibits", "eibit", "eib"],
            1152921504606846976.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["zebibits", "zibit", "zib"],
            1180591620717411303424.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["yobibits", "yibit", "yib"],
            1208925819614629174706176.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["kibibytes", "kibyte", "KiB"],
            8192.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["mebibytes", "mibyte", "MiB"],
            8388608.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["gibibytes", "gibyte", "GiB"],
            8589934592.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["tebibytes", "tibyte", "TiB"],
            8796093022208.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["pebibytes", "pibyte", "PiB"],
            9007199254740992.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["exbibytes", "eibyte", "EiB"],
            9223372036854775808.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["zebibytes", "zibyte", "ZiB"],
            9444732965739290427392.0,
        ),
        Unit::new_linear(
            UnitCategory::InformationStorage,
            &["yobibytes", "yibyte", "YiB"],
            9671406556917033397649408.0,
        ),
    ]
}

pub fn resolve_unit(identifier: &str) -> Result<Unit> {
    let units = get_all_units();
    let identifier_lower = identifier.to_lowercase();

    let mut exact_matches: Vec<Unit> = Vec::new();
    let mut case_matches: Vec<Unit> = Vec::new();

    for unit in units {
        let is_exact = unit.matches_exact(identifier);
        let is_case = unit
            .identifiers
            .iter()
            .any(|alias| alias.to_lowercase() == identifier_lower);

        if is_exact {
            exact_matches.push(unit.clone());
        }

        if is_case {
            case_matches.push(unit);
        }
    }

    if exact_matches.len() == 1 {
        return Ok(exact_matches.remove(0));
    }

    if exact_matches.len() > 1 {
        let mut suggestions = exact_matches
            .iter()
            .map(|unit| format!("{} ({})", identifier, unit.category.name()))
            .collect::<Vec<_>>();
        suggestions.sort();
        suggestions.dedup();
        return Err(anyhow!(
            "Ambiguous unit '{}'; try a more specific name such as {}",
            identifier,
            suggestions.join(", ")
        ));
    }

    if case_matches.is_empty() {
        return Err(anyhow!("Unknown unit: {}", identifier));
    }

    if case_matches.len() == 1 {
        return Ok(case_matches.remove(0));
    }

    let mut suggestions = case_matches
        .iter()
        .map(|unit| {
            let display = unit
                .identifiers
                .iter()
                .find(|alias| alias.to_lowercase() == identifier_lower)
                .copied()
                .unwrap_or(unit.identifiers[0]);
            format!("{} ({})", display, unit.category.name())
        })
        .collect::<Vec<_>>();
    suggestions.sort();
    suggestions.dedup();

    Err(anyhow!(
        "Ambiguous unit '{}'; try using specific casing such as {}",
        identifier,
        suggestions.join(", ")
    ))
}

pub fn find_unit(identifier: &str) -> Option<Unit> {
    resolve_unit(identifier).ok()
}

pub fn convert(value: f64, from_unit: &str, to_unit: &str) -> Result<f64> {
    let from = resolve_unit(from_unit)?;
    let to = resolve_unit(to_unit)?;

    if from.category != to.category {
        return Err(anyhow!(
            "Cannot convert {} to {}",
            from.category.name(),
            to.category.name()
        ));
    }

    // Convert to base unit, then to target unit
    let base_value = from.convert_to_base(value);
    let result = to.convert_from_base(base_value);

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_length_conversion() {
        let result = convert(1.0, "km", "m").unwrap();
        assert!((result - 1000.0).abs() < 1e-10);
    }

    #[test]
    fn test_temperature_conversion() {
        let result = convert(0.0, "celsius", "fahrenheit").unwrap();
        assert!((result - 32.0).abs() < 1e-10);

        let result = convert(100.0, "celsius", "fahrenheit").unwrap();
        assert!((result - 212.0).abs() < 1e-10);
    }

    #[test]
    fn test_same_unit_conversion() {
        let result = convert(42.0, "meters", "m").unwrap();
        assert!((result - 42.0).abs() < 1e-10);
    }

    #[test]
    fn test_incompatible_units() {
        let result = convert(1.0, "kg", "meters");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Cannot convert"));
    }

    #[test]
    fn test_unknown_unit() {
        let result = convert(1.0, "foobar", "meters");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unknown unit"));
    }

    #[test]
    fn test_ambiguous_unit_identifier() {
        let result = convert(1.0, "ma", "amperes");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Ambiguous unit"));
    }

    #[test]
    fn test_information_storage() {
        let result = convert(1.0, "kibibytes", "bytes").unwrap();
        assert!((result - 1024.0).abs() < 1e-10);
    }

    #[test]
    fn test_case_insensitive() {
        let result = convert(1.0, "KM", "M").unwrap();
        assert!((result - 1000.0).abs() < 1e-10);
    }

    #[test]
    fn test_angle_conversion() {
        let result = convert(180.0, "degrees", "radians").unwrap();
        assert!((result - std::f64::consts::PI).abs() < 1e-10);
    }

    #[test]
    fn test_area_conversion() {
        let result = convert(1.0, "square kilometers", "square meters").unwrap();
        assert!((result - 1e6).abs() < 1e-6);
    }

    #[test]
    fn test_concentration_mass_conversion() {
        let result = convert(1.0, "grams per liter", "milligrams per deciliter").unwrap();
        assert!((result - 100.0).abs() < 1e-10);
    }

    #[test]
    fn test_duration_conversion() {
        let result = convert(1.0, "hours", "seconds").unwrap();
        assert!((result - 3600.0).abs() < 1e-10);
    }

    #[test]
    fn test_electric_charge_conversion() {
        let result = convert(1.0, "ampere hours", "coulombs").unwrap();
        assert!((result - 3600.0).abs() < 1e-10);
    }

    #[test]
    fn test_electric_current_conversion() {
        let result = convert(1.0, "kiloamperes", "amperes").unwrap();
        assert!((result - 1000.0).abs() < 1e-10);
    }

    #[test]
    fn test_electric_potential_difference_conversion() {
        let result = convert(1.0, "kilovolts", "volts").unwrap();
        assert!((result - 1000.0).abs() < 1e-10);
    }

    #[test]
    fn test_electric_resistance_conversion() {
        let result = convert(1.0, "kiloohms", "ohms").unwrap();
        assert!((result - 1000.0).abs() < 1e-10);
    }

    #[test]
    fn test_energy_conversion() {
        let result = convert(1.0, "kilocalories", "calories").unwrap();
        assert!((result - 1000.0).abs() < 1e-10);
    }

    #[test]
    fn test_frequency_conversion() {
        let result = convert(1.0, "megahertz", "hertz").unwrap();
        assert!((result - 1e6).abs() < 1e-6);
    }

    #[test]
    fn test_fuel_efficiency_conversion() {
        let result = convert(10.0, "liters per 100 kilometers", "miles per gallon").unwrap();
        assert!((result - 23.521_458_333_333_332).abs() < 1e-9);

        let result = convert(7.5, "miles per gallon", "liters per 100 kilometers").unwrap();
        assert!((result - (235.214_583_333_333_34 / 7.5)).abs() < 1e-9);

        let result = convert(40.0, "miles per gallon", "miles per imperial gallon").unwrap();
        assert!((result - 48.037_997_020_194_2).abs() < 1e-9);

        let result = convert(
            50.0,
            "miles per imperial gallon",
            "liters per 100 kilometers",
        )
        .unwrap();
        assert!((result - 5.649_618_726_636_444).abs() < 1e-9);
    }

    #[test]
    fn test_power_conversion() {
        let result = convert(1.0, "kilowatts", "watts").unwrap();
        assert!((result - 1000.0).abs() < 1e-10);
    }

    #[test]
    fn test_pressure_conversion() {
        let result = convert(1.0, "bars", "pascals").unwrap();
        assert!((result - 100000.0).abs() < 1e-6);
    }

    #[test]
    fn test_speed_conversion() {
        let result = convert(1.0, "kilometers per hour", "meters per second").unwrap();
        assert!((result - (1.0 / 3.6)).abs() < 1e-10);
    }

    #[test]
    fn test_volume_conversion() {
        let result = convert(1.0, "liters", "milliliters").unwrap();
        assert!((result - 1000.0).abs() < 1e-10);
    }
}
