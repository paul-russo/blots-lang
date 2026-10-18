#!/usr/bin/env python3
"""Compare Blots against other dynamic language implementations.

Each benchmark exists as `<name>.blots` plus an equivalent `<name>.lua` and `<name>.py`,
written the way a programmer in that language would naturally write it (loops where the
language has loops). Where that takes something Lua 5.1 and LuaJIT do not have (integer
division), `<name>.lua51.lua` is what those two run instead. Every program prints a single
checksum, which is compared across implementations so a benchmark can never silently
measure different work.

Usage:
    benches/compare/run.py [--runs N] [--min-time S] [--blots PATH] [--baseline PATH]
                           [--only a,b] [--skip a,b] [--json FILE]

Timing is best-of-N wall clock for the whole process (so it includes startup, parsing and
teardown), measured with a monotonic clock around a plain fork/exec. A program that takes
less than a millisecond is at the mercy of the scheduler in five runs, so each one is also
run again until `--min-time` seconds have gone into it.
"""

import argparse
import json
import math
import os
import shutil
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))

BENCHMARKS = [
    "startup",
    "fib",
    "tak",
    "collatz",
    "mandelbrot",
    "reduce_sum",
    "hof_pipeline",
    "closures",
    "matmul",
    "records",
    "binary_trees",
    "strings",
    "sort",
    "broadcast",
]


# The most runs that a program is given, however short it is.
MAX_RUNS = 1000


def implementations(args):
    """(label, argv prefix, file extensions by preference, stream carrying the checksum)."""
    impls = []

    if args.baseline:
        impls.append(("blots-base", [args.baseline], ["blots"], "stderr"))

    impls.append(("blots", [args.blots], ["blots"], "stderr"))

    for label, binary, extra, exts in [
        ("lua5.4", "lua5.4", [], ["lua"]),
        ("lua5.1", "lua5.1", [], ["lua51.lua", "lua"]),
        ("luajit-int", "luajit", ["-joff"], ["lua51.lua", "lua"]),
        ("luajit", "luajit", [], ["lua51.lua", "lua"]),
        ("python3", "python3", [], ["py"]),
    ]:
        if label in args.skip:
            continue

        path = shutil.which(binary)
        if path:
            impls.append((label, [path, *extra], exts, "stdout"))

    return impls


def program(name, exts):
    """The path of the first of the benchmark's files that exists, or None."""
    for ext in exts:
        path = os.path.join(HERE, f"{name}.{ext}")
        if os.path.exists(path):
            return path

    return None


def run_once(argv):
    start = time.perf_counter()
    proc = subprocess.run(argv, capture_output=True, text=True)
    elapsed = time.perf_counter() - start

    return elapsed, proc


def checksum(text):
    """The last non-empty output line as a float, or None when there isn't one."""
    lines = [line.strip() for line in text.splitlines() if line.strip()]
    if not lines:
        return None

    try:
        return float(lines[-1])
    except ValueError:
        return lines[-1]


def same(a, b):
    if isinstance(a, float) and isinstance(b, float):
        return math.isclose(a, b, rel_tol=1e-9, abs_tol=1e-9)

    return a == b


def fmt_ms(seconds):
    if seconds is None:
        return "-"

    ms = seconds * 1000.0
    if ms >= 100:
        return f"{ms:.0f}"
    if ms >= 10:
        return f"{ms:.1f}"

    return f"{ms:.2f}"


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("--runs", type=int, default=5)
    parser.add_argument("--min-time", type=float, default=0.25, help="seconds to keep rerunning a short program for")
    parser.add_argument("--blots", default=os.path.join(ROOT, "target", "release", "blots"))
    parser.add_argument("--baseline", default=None, help="an older blots binary to compare against")
    parser.add_argument("--only", default=None, help="comma-separated benchmark names")
    parser.add_argument("--skip", default="", help="comma-separated implementation labels to skip")
    parser.add_argument("--json", default=None, help="write raw results to this file")
    args = parser.parse_args()
    args.skip = set(filter(None, args.skip.split(",")))

    names = args.only.split(",") if args.only else BENCHMARKS
    impls = implementations(args)
    labels = [label for label, *_ in impls]
    results = {}
    failed = False

    for name in names:
        results[name] = {}
        reference = None

        for label, prefix, exts, stream in impls:
            path = program(name, exts)
            if path is None:
                continue

            best = None
            value = None
            runs = 0
            spent = 0.0

            while runs < args.runs or (spent < args.min_time and runs < MAX_RUNS):
                elapsed, proc = run_once([*prefix, path])

                if proc.returncode != 0:
                    print(f"!! {name}/{label} exited {proc.returncode}: {proc.stderr[-400:]}", file=sys.stderr)
                    failed = True
                    best = None
                    break

                value = checksum(getattr(proc, stream))
                best = elapsed if best is None else min(best, elapsed)
                runs += 1
                spent += elapsed

            if best is None:
                continue

            results[name][label] = best

            if name == "startup":
                continue

            if reference is None:
                reference = (label, value)
            elif not same(reference[1], value):
                print(
                    f"!! {name}: checksum mismatch, {reference[0]}={reference[1]!r} vs {label}={value!r}",
                    file=sys.stderr,
                )
                failed = True

    width = max(len(name) for name in names) + 2
    header = "benchmark".ljust(width) + "".join(label.rjust(12) for label in labels)
    header += "  blots/lua5.4".rjust(14)
    if args.baseline:
        header += "  speedup".rjust(10)

    print(header)
    print("-" * len(header))

    ratios = []
    speedups = []

    for name in names:
        row = name.ljust(width)
        for label in labels:
            row += fmt_ms(results[name].get(label)).rjust(12)

        blots = results[name].get("blots")
        lua = results[name].get("lua5.4")
        if blots and lua:
            ratio = blots / lua
            ratios.append(ratio)
            row += f"{ratio:.2f}x".rjust(14)
        else:
            row += "-".rjust(14)

        base = results[name].get("blots-base")
        if args.baseline:
            if blots and base:
                speedups.append(base / blots)
                row += f"{base / blots:.1f}x".rjust(10)
            else:
                row += "-".rjust(10)

        print(row)

    print("-" * len(header))
    print(f"times in milliseconds, best of {args.runs} or of as many as fit in {args.min_time:g} s")

    if ratios:
        geomean = math.exp(sum(math.log(r) for r in ratios) / len(ratios))
        print(f"geometric mean blots/lua5.4: {geomean:.2f}x  (below 1.0 means blots is faster)")

    if speedups:
        geomean = math.exp(sum(math.log(s) for s in speedups) / len(speedups))
        print(f"geometric mean speedup over baseline: {geomean:.1f}x")

    if args.json:
        with open(args.json, "w") as handle:
            json.dump(results, handle, indent=2)

    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
