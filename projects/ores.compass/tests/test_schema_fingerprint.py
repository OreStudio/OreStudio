"""The schema fingerprint compass stamps agrees with the one the build compiles.

compass_db.schema_fingerprint and projects/ores.sql/schema_fingerprint.cmake
are two implementations of one definition. If they disagree, every service
refuses every freshly recreated database, so these tests run both on the same
trees and require the same answer.
"""

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass_db

PROJECT_ROOT = Path(__file__).resolve().parents[3]
SCRIPT = PROJECT_ROOT / "projects" / "ores.sql" / "schema_fingerprint.cmake"


def cmake_fingerprint(sql_dir):
    if shutil.which("cmake") is None:
        pytest.skip("cmake is not on the path")
    out = subprocess.run(
        ["cmake", f"-DSQL_DIR={sql_dir}", "-DPRINT=ON", "-P", str(SCRIPT)],
        capture_output=True, text=True, check=True)
    return (out.stderr + out.stdout).strip()


def make_tree(root):
    sql = root / "projects" / "ores.sql"
    for rel, text in {
        "setup_schema.sql": "select 1;\n",
        "create/create.sql": "\\ir ./refdata/a.sql\n",
        "create/refdata/a.sql": "create table a (x int);\n",
        "populate/populate.sql": "insert into a values (1);\n",
        "instance/init_instance.sql": "select 2;\n",
        "drop/drop.sql": "drop table a;\n",
        "test/a_test.sql": "select 3;\n",
    }.items():
        path = sql / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
    return sql


def test_compass_and_the_build_agree_on_the_real_tree():
    sql_dir = PROJECT_ROOT / "projects" / "ores.sql"
    assert compass_db.schema_fingerprint(PROJECT_ROOT) == cmake_fingerprint(sql_dir)


def test_compass_and_the_build_agree_on_a_small_tree(tmp_path):
    sql_dir = make_tree(tmp_path)
    fingerprint = compass_db.schema_fingerprint(tmp_path)
    assert len(fingerprint) == 16
    assert fingerprint == cmake_fingerprint(sql_dir)


def test_editing_an_input_changes_the_fingerprint(tmp_path):
    sql_dir = make_tree(tmp_path)
    before = compass_db.schema_fingerprint(tmp_path)
    (sql_dir / "create" / "refdata" / "a.sql").write_text("create table a (y int);\n")
    assert compass_db.schema_fingerprint(tmp_path) != before


def test_files_recreate_does_not_read_leave_it_alone(tmp_path):
    sql_dir = make_tree(tmp_path)
    before = compass_db.schema_fingerprint(tmp_path)
    (sql_dir / "drop" / "drop.sql").write_text("drop table b;\n")
    (sql_dir / "test" / "a_test.sql").write_text("select 4;\n")
    assert compass_db.schema_fingerprint(tmp_path) == before


def test_a_matching_database_passes(monkeypatch, tmp_path, capsys):
    make_tree(tmp_path)
    expected = compass_db.schema_fingerprint(tmp_path)
    monkeypatch.setattr(compass_db, "database_info", lambda env: {
        "schema_fingerprint": expected, "git_commit": "abc", "git_date": "d"})
    assert compass_db.check_schema_in_sync(tmp_path, {}) is True
    assert expected in capsys.readouterr().out


def test_a_mismatch_warns_with_both_fingerprints_and_continues(
        monkeypatch, tmp_path, capsys):
    """A developer's checkout that has moved past its database still starts."""
    monkeypatch.delenv("ORES_REQUIRE_SCHEMA_MATCH", raising=False)
    make_tree(tmp_path)
    expected = compass_db.schema_fingerprint(tmp_path)
    monkeypatch.setattr(compass_db, "database_info", lambda env: {
        "schema_fingerprint": "0000000000000000", "git_commit": "abc123",
        "git_date": "2026/10/01 10:00:00"})
    assert compass_db.check_schema_in_sync(
        tmp_path, {"ORES_TEST_DB_DATABASE": "ores_dev_x"}) is True
    err = capsys.readouterr().err
    assert "WARNING" in err
    assert "REFUSING TO START" not in err
    assert expected in err
    assert "0000000000000000" in err
    assert "abc123" in err
    assert "compass db recreate" in err
    assert "ORES_REQUIRE_SCHEMA_MATCH" in err


def test_a_mismatch_is_refused_when_the_strict_answer_is_asked_for(
        monkeypatch, tmp_path, capsys):
    """A deployment sets the switch and gets the refusal it needs."""
    make_tree(tmp_path)
    monkeypatch.setattr(compass_db, "database_info", lambda env: {
        "schema_fingerprint": "0000000000000000", "git_commit": "abc123",
        "git_date": "2026/10/01 10:00:00"})
    assert compass_db.check_schema_in_sync(
        tmp_path, {"ORES_TEST_DB_DATABASE": "ores_dev_x",
                   "ORES_REQUIRE_SCHEMA_MATCH": "1"}) is False
    assert "REFUSING TO START" in capsys.readouterr().err


def test_an_unreachable_database_warns_and_continues(monkeypatch, tmp_path, capsys):
    monkeypatch.delenv("ORES_REQUIRE_SCHEMA_MATCH", raising=False)
    make_tree(tmp_path)
    monkeypatch.setattr(compass_db, "database_info", lambda env: None)
    assert compass_db.check_schema_in_sync(tmp_path, {}) is True
    err = capsys.readouterr().err
    assert "(none)" in err
    assert "WARNING" in err
