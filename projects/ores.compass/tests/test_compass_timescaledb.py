"""
Tests for the timescaledb line in `compass db status`.

No live database and no real postgres: the library directory is a temporary
one and the verdict is a pure function, so these cases describe what the
report says rather than the machine it runs on.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_timescaledb.py -v
"""

import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass_db


def test_a_recorded_version_with_its_library_reads_as_healthy():
    label, level = compass_db.timescaledb_verdict("2.30.2", ["2.30.1", "2.30.2"])

    assert level == "ok"
    assert "2.30.2" in label


def test_a_recorded_version_without_its_library_names_the_fix():
    """The apt-upgrade case: the package moved on, the database did not."""
    label, level = compass_db.timescaledb_verdict("2.30.1", ["2.30.2"])

    assert level == "critical"
    assert "2.30.1" in label
    assert "2.30.2" in label


def test_no_library_at_all_is_critical():
    label, level = compass_db.timescaledb_verdict("2.30.1", [])

    assert level == "critical"
    assert "no timescaledb library is on disk" in label


def test_a_database_without_the_extension_is_not_a_problem():
    label, level = compass_db.timescaledb_verdict(None, ["2.30.2"])

    assert level == "none"
    assert "not installed in this database" in label


def test_the_libraries_on_disk_are_read_from_the_directory(tmp_path):
    (tmp_path / "timescaledb-2.30.1.so").write_text("")
    (tmp_path / "timescaledb-2.30.2.so").write_text("")
    (tmp_path / "timescaledb.so").write_text("")

    assert compass_db.timescaledb_libraries(str(tmp_path)) == ["2.30.1", "2.30.2"]


def test_the_fix_statement_is_the_documented_one():
    assert compass_db.TIMESCALEDB_FIX == (
        'compass sql -- -c "alter extension timescaledb update;"')
