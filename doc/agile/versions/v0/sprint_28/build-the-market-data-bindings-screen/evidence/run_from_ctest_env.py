#!/usr/bin/env python3
"""Run one Catch2 test binary with the environment CTest would give it.

Reads the ENVIRONMENT property out of a CTestTestfile.cmake for the named test,
then runs the binary with the arguments after `--`. A wrapper is needed because
the build lock is held by other worktrees, so `compass test run` is unavailable.
"""

import os
import re
import subprocess
import sys
from pathlib import Path


def environment_of(testfile: Path, test_name: str) -> dict[str, str]:
    text = testfile.read_text()
    pattern = (
        r"set_tests_properties\(\[=\[" + re.escape(test_name) + r"\]=\] PROPERTIES.*?"
        r'ENVIRONMENT "([^"]*)"'
    )
    match = re.search(pattern, text, re.DOTALL)
    if not match:
        raise SystemExit(f"no ENVIRONMENT property for {test_name} in {testfile}")
    env: dict[str, str] = {}
    for entry in match.group(1).split(";"):
        key, _, value = entry.partition("=")
        env[key] = value
    return env


def main() -> int:
    testfile = Path(sys.argv[1])
    test_name = sys.argv[2]
    command = sys.argv[3:]
    env = {**os.environ, **environment_of(testfile, test_name)}
    print(f"passing the environment of {test_name} from {testfile}", flush=True)
    completed = subprocess.run(command, env=env)
    return completed.returncode


if __name__ == "__main__":
    sys.exit(main())
