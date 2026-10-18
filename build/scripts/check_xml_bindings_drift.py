#!/usr/bin/env python3
"""Fail when the committed codec tables are not what the schemas produce.

The generated files are an artefact of the schemas and the generator, and
nothing else may write them. The check regenerates twice into temporary roots,
compares the two runs to prove the generator is deterministic, then compares
that output with what is committed.

The fresh output is the authority, not the committed tree: a table the schemas
produce but nobody committed is the failure this check exists to catch, and
walking the committed directories would never see it.
"""

import os
import subprocess
import sys
import tempfile

GENERATED = (
    "projects/ores.ore/core/include/ores.ore.core/xml/generated",
    "projects/ores.ore/core/src/xml/generated",
)


def git_root():
    return subprocess.run(
        ("git", "rev-parse", "--show-toplevel"), capture_output=True, text=True, check=False
    ).stdout.strip()


def generate(root, out, seed):
    env = dict(os.environ, XML_BINDINGS_OUT=out, PYTHONHASHSEED=str(seed))
    return subprocess.run(
        ("./projects/ores.ore/scripts/regenerate_xml_bindings.sh",),
        capture_output=True,
        text=True,
        env=env,
        cwd=root,
        check=False,
    )


def files_under(where):
    found = {}
    for directory in GENERATED:
        full = os.path.join(where, directory)
        if not os.path.isdir(full):
            continue
        for name in sorted(os.listdir(full)):
            with open(os.path.join(full, name), "rb") as handle:
                found[os.path.join(directory, name)] = handle.read()
    return found


def main():
    root = git_root()
    if not root:
        print("not a git checkout; nothing to check")
        return 0

    with tempfile.TemporaryDirectory() as first, tempfile.TemporaryDirectory() as second:
        for out, seed in ((first, 1), (second, 999)):
            done = generate(root, out, seed)
            if done.returncode != 0:
                print("the generator itself failed:")
                print(done.stderr.strip())
                return 1

        a, b = files_under(first), files_under(second)
        if a != b:
            print("The generator is not deterministic: two runs differ.")
            only_a = sorted(set(a) - set(b))
            only_b = sorted(set(b) - set(a))
            differing = sorted(k for k in set(a) & set(b) if a[k] != b[k])
            for name in only_a:
                print(f"  only in the first run:  {name}")
            for name in only_b:
                print(f"  only in the second run: {name}")
            for name in differing:
                print(f"  differs between runs:   {name}")
            return 1

        committed = files_under(root)
        problems = []
        for name in sorted(a):
            if name not in committed:
                problems.append(f"{name} is produced by the schemas but not committed")
            elif committed[name] != a[name]:
                problems.append(f"{name} differs from what the schemas produce")
        for name in sorted(set(committed) - set(a)):
            problems.append(f"{name} is committed but no longer produced")

    if problems:
        print("The committed codec tables are not what the schemas produce:")
        for line in problems:
            print(f"  {line}")
        print("")
        print("Run ./projects/ores.ore/scripts/regenerate_xml_bindings.sh and commit.")
        return 1

    print("the generated codec tables match the schemas, and the generator is deterministic")
    return 0


if __name__ == "__main__":
    sys.exit(main())
