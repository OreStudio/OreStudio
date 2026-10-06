"""Tests that the widening check finds a policy that lets the system tenant
see every tenant, and passes a policy scoped to the current tenant alone.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_check_rls_system_tenant_widenings.py
"""
import importlib.util
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SCRIPT = REPO_ROOT / "projects/ores.codegen/scripts/check_rls_system_tenant_widenings.py"

spec = importlib.util.spec_from_file_location("check_widenings", SCRIPT)
check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check)

WIDENED = """
-- The system tenant reads parties too.
create policy parties_tenant_isolation_policy on ores_refdata_parties_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
);
"""

STRICT = """
-- or ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
create policy parties_tenant_isolation_policy on ores_refdata_parties_tbl
for all using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());
"""


def write(tmp_path, sql):
    (tmp_path / "policies_create.sql").write_text(sql, encoding="utf-8")
    return tmp_path


def test_a_policy_that_admits_the_system_tenant_is_found(tmp_path, monkeypatch):
    monkeypatch.setattr(check, "REPO_ROOT", tmp_path)
    found = check.widenings(write(tmp_path, WIDENED))
    assert [(name, table) for name, table, _ in found] == [
        ("parties_tenant_isolation_policy", "ores_refdata_parties_tbl")]


def test_a_policy_scoped_to_the_current_tenant_is_not_found(tmp_path, monkeypatch):
    monkeypatch.setattr(check, "REPO_ROOT", tmp_path)
    assert check.widenings(write(tmp_path, STRICT)) == []


def test_the_parties_policy_is_not_an_exception():
    assert "parties_tenant_isolation_policy" not in check.EXCEPTIONS


def test_the_tree_has_no_unlisted_widening():
    assert check.main() == 0
