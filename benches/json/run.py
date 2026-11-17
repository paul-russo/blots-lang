#!/usr/bin/env python3
"""Compare the JSON input and output of Blots against jq.

Every document is read and thrown away (`parse`) and read and written again (`roundtrip`)
by both programs, which is all that either does with JSON apart from what a program asks
for. Each run is timed as a whole process and its peak memory is taken from the kernel, so
the numbers include startup, reading stdin and teardown.

The documents are made up here, the same ones every time, and kept under `target/`. What
Blots writes for a round trip is compared with the document itself, so a benchmark can
never silently measure different work.

Usage:
    benches/json/run.py [--runs N] [--scale N] [--blots PATH] [--baseline PATH]
                        [--only a,b] [--file PATH ...]

`--scale 3` makes the documents three times as long (about 70 MB of records). A `--file` is
any other document to compare on, which has to be an object.
"""

import argparse
import json
import math
import os
import random
import shutil
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))

WORDS = (
    "alpha bravo charlie delta echo foxtrot golf hotel india juliet kilo lima mike november "
    "oscar papa quebec romeo sierra tango uniform victor whiskey xray yankee zulu"
).split()

# What each program is asked to do with a document on stdin: (jq arguments, Blots source).
OPERATIONS = {
    "parse": (["length"], "output count = len(keys(inputs))"),
    "roundtrip": (["-c", "."], "output out = inputs"),
}


def sentence(rng, words):
    return " ".join(rng.choice(WORDS) for _ in range(words))


def record(rng, index):
    """An entry of the kind an export of an API or a log is made of."""
    return {
        "id": index,
        "uuid": "%032x" % rng.getrandbits(128),
        "name": sentence(rng, 2).title(),
        "email": "%s.%s@example.com" % (rng.choice(WORDS), rng.choice(WORDS)),
        "active": rng.random() < 0.7,
        "score": round(rng.uniform(0, 100), 3),
        "balance": round(rng.uniform(-1e5, 1e5), 2),
        "created_at": "2026-%02d-%02dT%02d:%02d:%02dZ"
        % (rng.randint(1, 12), rng.randint(1, 28), rng.randint(0, 23), rng.randint(0, 59), rng.randint(0, 59)),
        "tags": rng.sample(WORDS, rng.randint(0, 5)),
        "address": {
            "street": "%d %s St" % (rng.randint(1, 9999), rng.choice(WORDS).title()),
            "city": rng.choice(WORDS).title(),
            "zip": "%05d" % rng.randint(0, 99999),
            "geo": {"lat": rng.uniform(-90, 90), "lng": rng.uniform(-180, 180)},
        },
        "bio": sentence(rng, rng.randint(5, 30)) + (' "quoted"\n\tline — ünïcödé ✓' if rng.random() < 0.2 else ""),
        "parent": None if rng.random() < 0.5 else rng.randint(0, index),
    }


def documents(scale):
    """(name, what to make it from, how to write it), for every document that is made up."""
    compact = {"ensure_ascii": False, "separators": (",", ":")}

    return [
        ("records", lambda rng: {"records": [record(rng, i) for i in range(50_000 * scale)]}, compact),
        # The same kind of entries the way many tools print them: indented, in ASCII only.
        ("records_pretty", lambda rng: {"records": [record(rng, i) for i in range(16_000 * scale)]}, {"indent": 2}),
        (
            "numbers",
            lambda rng: {
                "ints": [rng.randint(-(10**9), 10**9) for _ in range(330_000 * scale)],
                "floats": [rng.uniform(-1e6, 1e6) for _ in range(330_000 * scale)],
            },
            compact,
        ),
        (
            "strings",
            lambda rng: {
                "lines": [
                    sentence(rng, rng.randint(3, 40)) + ('\n"x"\\' if rng.random() < 0.3 else "")
                    for _ in range(66_000 * scale)
                ]
            },
            compact,
        ),
    ]


def make_documents(scale, directory):
    """The paths of the documents by name, which are made where they are not there yet."""
    os.makedirs(directory, exist_ok=True)
    paths = {}

    for name, make, options in documents(scale):
        path = os.path.join(directory, f"{name}.x{scale}.json")
        paths[name] = path

        if not os.path.exists(path):
            with open(path, "w", encoding="utf-8") as handle:
                json.dump(make(random.Random(7)), handle, **options)

    return paths


def run_once(argv, path, keep_output=False):
    """(seconds, peak bytes of memory, output or None) of one run with the document on stdin."""
    with open(path, "rb") as document:
        start = time.perf_counter()
        process = subprocess.Popen(
            argv,
            stdin=document,
            stdout=subprocess.PIPE if keep_output else subprocess.DEVNULL,
            stderr=subprocess.PIPE,
        )
        output = process.stdout.read() if keep_output else None
        errors = process.stderr.read()
        _, status, usage = os.wait4(process.pid, 0)
        elapsed = time.perf_counter() - start

    if status != 0:
        sys.exit(f"!! {' '.join(argv[:2])} failed on {path}: {errors.decode(errors='replace')[-400:]}")

    # The kernel counts in bytes on macOS and in kilobytes elsewhere.
    peak = usage.ru_maxrss if sys.platform == "darwin" else usage.ru_maxrss * 1024

    return elapsed, peak, output


def measure(argv, path, runs):
    """The best time and the lowest peak of memory of `runs` runs."""
    results = [run_once(argv, path) for _ in range(runs)]

    return min(seconds for seconds, _, _ in results), min(peak for _, peak, _ in results)


def says_the_same(written, read):
    """Whether what was written says what was read, to the last place of a number.

    Blots reads a number the way serde_json does by default, which can be one unit in the
    last place away from the nearest double, and it keeps every number as a double.
    """
    if isinstance(read, bool) or read is None or isinstance(read, str):
        return type(written) is type(read) and written == read
    if isinstance(read, (int, float)):
        return isinstance(written, float) and math.isclose(written, read, rel_tol=1e-15, abs_tol=0.0)
    if isinstance(read, list):
        return isinstance(written, list) and len(written) == len(read) and all(map(says_the_same, written, read))

    return (
        isinstance(written, dict)
        and written.keys() == read.keys()
        and all(says_the_same(written[key], value) for key, value in read.items())
    )


def is_faithful(blots, path):
    """Whether a round trip through Blots writes what the document says."""
    _, _, output = run_once([blots, OPERATIONS["roundtrip"][1]], path, keep_output=True)

    with open(path, "rb") as document:
        return says_the_same(json.loads(output)["out"], json.load(document))


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("--runs", type=int, default=5)
    parser.add_argument("--scale", type=int, default=1, help="how many times as long the documents are")
    parser.add_argument("--blots", default=os.path.join(ROOT, "target", "release", "blots"))
    parser.add_argument("--baseline", default=None, help="an older blots binary to compare against")
    parser.add_argument("--only", default=None, help="comma-separated document names")
    parser.add_argument("--file", action="append", default=[], help="another document to compare on")
    args = parser.parse_args()

    jq = shutil.which("jq")
    if jq is None:
        sys.exit("!! jq is not installed")

    paths = make_documents(args.scale, os.path.join(ROOT, "target", "json-bench"))
    paths.update({os.path.basename(path): path for path in args.file})
    if args.only:
        paths = {name: paths[name] for name in args.only.split(",")}

    programs = [("jq", lambda operation: [jq, *OPERATIONS[operation][0]])]
    if args.baseline:
        programs.append(("blots-base", lambda operation: [args.baseline, OPERATIONS[operation][1]]))
    programs.append(("blots", lambda operation: [args.blots, OPERATIONS[operation][1]]))

    width = max(len(name) for name in paths) + 2
    header = "document".ljust(width) + "MB".rjust(7) + "  " + "operation".ljust(10)
    header += "".join(f"{label} ms".rjust(15) for label, _ in programs) + "jq/blots".rjust(10)
    header += "".join(f"{label} MB".rjust(15) for label, _ in programs) + "blots/jq".rjust(10)

    print(header)
    print("-" * len(header))

    failed = False
    behind = []

    for name, path in paths.items():
        if not is_faithful(args.blots, path):
            print(f"!! {name}: a round trip through blots does not write what the document says", file=sys.stderr)
            failed = True

        for operation in OPERATIONS:
            results = {label: measure(argv(operation), path, args.runs) for label, argv in programs}
            (jq_seconds, jq_peak), (blots_seconds, blots_peak) = results["jq"], results["blots"]

            row = name.ljust(width) + f"{os.path.getsize(path) / 1e6:.1f}".rjust(7) + "  " + operation.ljust(10)
            row += "".join(f"{seconds * 1000:.1f}".rjust(15) for seconds, _ in results.values())
            row += f"{jq_seconds / blots_seconds:.1f}x".rjust(10)
            row += "".join(f"{peak / 1e6:.1f}".rjust(15) for _, peak in results.values())
            row += f"{blots_peak / jq_peak:.2f}x".rjust(10)
            print(row)

            if blots_seconds > jq_seconds:
                behind.append(f"{name} {operation}: time")
            if blots_peak > jq_peak:
                behind.append(f"{name} {operation}: memory")

    print("-" * len(header))
    print(f"best time and lowest peak of memory of {args.runs} runs of the whole process")
    print("jq/blots above 1.0 means blots is faster; blots/jq below 1.0 means blots takes less memory")

    if behind:
        print("blots is behind jq on: " + ", ".join(behind))

    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
