"""Tests that the single-writer check finds a table a populate script and a
publish function both write, and passes a table each writes alone.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_check_single_writer.py
"""
import importlib.util
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SCRIPT = REPO_ROOT / "projects/ores.codegen/scripts/check_single_writer.py"

spec = importlib.util.spec_from_file_location("check_single_writer", SCRIPT)
check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check)

SEED_BOTH = """
insert into ores_refdata_calendars_tbl (code) values ('TARGET');
-- insert into ores_refdata_countries_tbl (code) values ('ZZ');
"""

PUBLISH_BOTH = """
create or replace function ores_refdata_publish_calendars_from_dq_fn() returns void as $$
begin
    insert into ores_refdata_calendars_tbl (code) values ('USD');
end;
$$ language plpgsql;
"""

SEED_ONE = """
insert into ores_refdata_currencies_tbl (iso_code) values ('EUR');
"""


def write_tree(root, seed_sql, publish_sql):
    (root / "populate").mkdir()
    (root / "create").mkdir()
    (root / "populate" / "seed_populate.sql").write_text(seed_sql, encoding="utf-8")
    (root / "create" / "refdata_publish_from_dq_create.sql").write_text(
        publish_sql, encoding="utf-8")


def test_a_table_both_writers_reach_is_found(tmp_path, monkeypatch):
    monkeypatch.setattr(check, "REPO_ROOT", tmp_path)
    write_tree(tmp_path, SEED_BOTH, PUBLISH_BOTH)
    found = check.writers(tmp_path / "populate", tmp_path / "create")
    assert sorted(found) == ["ores_refdata_calendars_tbl"]


def test_a_commented_out_insert_is_not_a_writer(tmp_path, monkeypatch):
    monkeypatch.setattr(check, "REPO_ROOT", tmp_path)
    write_tree(tmp_path, SEED_BOTH, PUBLISH_BOTH)
    assert "ores_refdata_countries_tbl" not in check.inserted(
        (tmp_path / "populate" / "seed_populate.sql").read_text(encoding="utf-8"))


def test_a_table_only_the_seed_writes_is_not_found(tmp_path, monkeypatch):
    monkeypatch.setattr(check, "REPO_ROOT", tmp_path)
    write_tree(tmp_path, SEED_ONE, PUBLISH_BOTH)
    assert check.writers(tmp_path / "populate", tmp_path / "create") == {}


def test_every_exception_states_a_reason():
    for table, reason in check.EXCEPTIONS.items():
        assert len(reason) > 30, table


def test_the_tree_has_no_unlisted_second_writer():
    assert check.main() == 0
