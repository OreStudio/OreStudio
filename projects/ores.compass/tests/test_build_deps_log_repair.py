"""
Tests for the ninja deps log repair that `compass build` runs first.

Run with:  python -m pytest projects/ores.compass/tests/test_build_deps_log_repair.py -v
"""

import shutil
import struct
import subprocess
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from compass import _ninja_version, deps_log_repair_command  # noqa: E402


def _build_dir(tmp_path, program="/usr/bin/ninja", deps=True):
    (tmp_path / "CMakeCache.txt").write_text(
        f"CMAKE_GENERATOR:INTERNAL=Ninja\nCMAKE_MAKE_PROGRAM:FILEPATH={program}\n")
    if deps:
        (tmp_path / ".ninja_deps").write_bytes(b"# ninjadeps\n")
    return tmp_path


def _version(v):
    return lambda program: v


def test_ninja_1_13_recompacts_first(tmp_path):
    d = _build_dir(tmp_path)
    assert deps_log_repair_command(d, _version((1, 13, 2))) == [
        "/usr/bin/ninja", "-C", str(d), "-t", "recompact"]


@pytest.mark.parametrize("version", [(1, 12, 1), (1, 14, 0), None])
def test_other_ninja_versions_build_directly(tmp_path, version):
    assert deps_log_repair_command(_build_dir(tmp_path), _version(version)) is None


def test_a_directory_without_a_deps_log_builds_directly(tmp_path):
    d = _build_dir(tmp_path, deps=False)
    assert deps_log_repair_command(d, _version((1, 13, 2))) is None


def test_a_directory_without_a_cache_builds_directly(tmp_path):
    (tmp_path / ".ninja_deps").write_bytes(b"# ninjadeps\n")
    assert deps_log_repair_command(tmp_path, _version((1, 13, 2))) is None


def test_a_program_that_is_not_ninja_has_no_version(tmp_path):
    assert _ninja_version(str(tmp_path / "missing")) is None
    assert _ninja_version("/bin/true") is None


NINJA = shutil.which("ninja")
MANIFEST = """\
rule cc
  command = printf '%s: %s %s\\n' $out $in $hdr > $out.d && touch $out
  depfile = $out.d
  deps = gcc
build a.o: cc a.c
  hdr = a.h
build b.o: cc b.c
  hdr = b.h
"""


def _ninja(d):
    done = subprocess.run([NINJA, "-C", str(d)], capture_output=True,
                          text=True, check=True)
    return done.stdout + done.stderr


@pytest.mark.skipif(NINJA is None, reason="ninja is not installed")
def test_the_repair_stops_a_bad_record_forcing_full_builds(tmp_path):
    """A path record with a stale id is what two writers leave behind."""
    for name in ("a.c", "a.h", "b.c", "b.h"):
        (tmp_path / name).write_text("x\n")
    (tmp_path / "build.ninja").write_text(MANIFEST)
    _build_dir(tmp_path, program=NINJA, deps=False)
    _ninja(tmp_path)
    name = b"stale.h\0"
    record = name + struct.pack("<I", ~0 & 0xFFFFFFFF)
    with open(tmp_path / ".ninja_deps", "ab") as f:
        f.write(struct.pack("<I", len(record)) + record)

    repair = deps_log_repair_command(tmp_path, _ninja_version)
    if repair is None:
        pytest.skip(f"ninja {_ninja_version(NINJA)} does not need the repair")
    subprocess.run(repair, capture_output=True, check=True)

    first = _ninja(tmp_path)
    assert "premature end of file" not in first
    (tmp_path / "a.h").write_text("y\n")
    rebuilt = _ninja(tmp_path)
    assert "a.o" in rebuilt and "b.o" not in rebuilt
    assert "no work to do" in _ninja(tmp_path)
