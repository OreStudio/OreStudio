"""
Tests for `compass test run` command construction.

Run with:  python -m pytest projects/ores.compass/tests/test_test_run.py -v
No live database or file system access required.
"""

import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from compass import _test_run_command  # noqa: E402


def test_plain_form():
    cmd = _test_run_command("clang-debug", "", [])
    assert cmd == ["ctest", "--preset", "clang-debug"]


def test_pass_through_args():
    cmd = _test_run_command("clang-debug", "", ["-VV", "-R", "foo"])
    assert cmd == ["ctest", "--preset", "clang-debug", "-VV", "-R", "foo"]


def test_cdash_script_form():
    cmd = _test_run_command("clang-debug", "Experimental", [])
    assert cmd == [
        "ctest", "--preset", "clang-debug", "--script",
        "CTest.cmake,build_group=Experimental,preset=clang-debug",
    ]


def test_cdash_rejects_unknown_group():
    import pytest

    with pytest.raises(ValueError):
        _test_run_command("clang-debug", "Staging", [])
