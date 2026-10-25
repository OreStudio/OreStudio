"""Tests for the writer_role sql feature.

The feature grants INSERT on the table to one database role, after the policies,
for a table whose only writer is an ingest. The role is named as a psql
variable, because the create file is a psql script and the role name is
deployment state. Everything the role is allowed beyond INSERT -- BYPASSRLS, the
ownership of the table, the SELECT it does not have -- is stated where the roles
are created, so the feature adds no second statement.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_writer_role.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import validate_rls_isolation  # noqa: E402

TEMPLATE = (REPO_ROOT / "projects/ores.codegen/library/templates"
            / "sql_schema_domain_entity_create.mustache").read_text()


def model(**sql):
    return {
        "entity_singular": "tick",
        "has_tenant_id": True,
        "columns": [{"name": "party_id", "type": "uuid"}],
        "sql": {"rls_tenant_isolation": True, "rls_party_isolation": True, **sql},
    }


def test_a_writer_role_is_accepted():
    validate_rls_isolation(model(writer_role="marketdata_writer_user"))


@pytest.mark.parametrize("name", ["Marketdata", "a b", "a;drop", "1abc", ":x"])
def test_a_name_that_is_not_a_psql_variable_is_refused(name):
    with pytest.raises(ValueError, match="writer_role must name a psql variable"):
        validate_rls_isolation(model(writer_role=name))


def test_the_grant_is_one_insert_statement_that_names_the_role():
    block = TEMPLATE[TEMPLATE.index("{{#domain_entity.sql.writer_role}}"):
                     TEMPLATE.index("{{/domain_entity.sql.writer_role}}")]
    statements = [line for line in block.splitlines()
                  if line.startswith(("grant ", "alter ", "create ", "drop "))]
    assert statements == [
        "grant insert on {{domain_entity.sql_name_base}}_tbl "
        "to :{{domain_entity.sql.writer_role}};"]
    assert "bypassrls" not in block.lower()


def test_a_junction_is_refused():
    bad = model(writer_role="marketdata_writer_user")
    del bad["entity_singular"]
    bad["name_singular"] = "link"
    with pytest.raises(ValueError, match="domain entities only"):
        validate_rls_isolation(bad)
