#!/usr/bin/env python3
"""Run one Catch2 test binary with the environment CTest would give it.

Reads the ENVIRONMENT property out of a CTestTestfile.cmake for the named test,
then runs the binary with the arguments after it. A wrapper is needed because
the build lock is held by other worktrees, so `compass test run` is unavailable.

Usage: run_from_ctest_env.py <CTestTestfile.cmake> <test name> <command>...
"""

import os
import re
import subprocess
import sys
from pathlib import Path

USAGE = "usage: run_from_ctest_env.py <CTestTestfile.cmake> <test name> <command>..."


def blocks_of(text: str):
    """Yield the text of each set_tests_properties() call, one at a time.

    A call ends at the first close parenthesis that is not inside a quoted
    string, so a property value holding a parenthesis stays whole.
    """
    opener = "set_tests_properties("
    start = text.find(opener)
    while start != -1:
        index = start + len(opener)
        quoted = False
        while index < len(text):
            char = text[index]
            if char == '"':
                quoted = not quoted
            elif char == ")" and not quoted:
                break
            index += 1
        yield text[start:index + 1]
        start = text.find(opener, index)


def environment_of(testfile: Path, test_name: str) -> dict[str, str]:
    marker = "[=[" + test_name + "]=" + "]"
    for block in blocks_of(testfile.read_text()):
        if not block.startswith("set_tests_properties(" + marker):
            continue
        match = re.search(r'ENVIRONMENT "([^"]*)"', block)
        if not match:
            continue
        env: dict[str, str] = {}
        for entry in match.group(1).split(";"):
            if not entry:
                continue
            key, _, value = entry.partition("=")
            env[key] = value
        return env
    raise SystemExit(f"no ENVIRONMENT property for {test_name} in {testfile}")


def main() -> int:
    if len(sys.argv) < 4:
        raise SystemExit(USAGE)
    testfile = Path(sys.argv[1])
    test_name = sys.argv[2]
    command = sys.argv[3:]
    env = {**os.environ, **environment_of(testfile, test_name)}
    print(f"passing the environment of {test_name} from {testfile}", flush=True)
    completed = subprocess.run(command, env=env)
    return completed.returncode


if __name__ == "__main__":
    sys.exit(main())
