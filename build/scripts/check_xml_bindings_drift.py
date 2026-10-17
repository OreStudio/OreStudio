#!/usr/bin/env python3
"""Fail when a generated codec table and the schema it came from disagree.

The generator is the only thing that may write these files, so the check is
simple: regenerate into a temporary root and compare. A difference means either
someone edited a generated file by hand or a schema changed without the tables
following it, and both are the same failure from here.
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


def main():
    root = git_root()
    if not root:
        print("not a git checkout; nothing to check")
        return 0
    os.chdir(root)

    with tempfile.TemporaryDirectory() as out:
        env = dict(os.environ, XML_BINDINGS_OUT=out)
        done = subprocess.run(
            ("./projects/ores.ore/scripts/regenerate_xml_bindings.sh",),
            capture_output=True,
            text=True,
            env=env,
            check=False,
        )
        if done.returncode != 0:
            print("the generator itself failed:")
            print(done.stderr.strip())
            return 1

        stale = []
        for directory in GENERATED:
            for name in sorted(os.listdir(os.path.join(root, directory))):
                committed = os.path.join(root, directory, name)
                fresh = os.path.join(out, directory, name)
                if not os.path.exists(fresh):
                    stale.append(f"{directory}/{name} is committed but no longer generated")
                    continue
                with open(committed, "rb") as handle:
                    before = handle.read()
                with open(fresh, "rb") as handle:
                    after = handle.read()
                if before != after:
                    stale.append(f"{directory}/{name} differs from what the schema produces")

    if stale:
        print("The generated codec tables do not match the schemas:")
        for line in stale:
            print(f"  {line}")
        print("")
        print("Run ./projects/ores.ore/scripts/regenerate_xml_bindings.sh and commit.")
        return 1

    print("the generated codec tables match the schemas")
    return 0


if __name__ == "__main__":
    sys.exit(main())
