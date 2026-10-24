"""Tests for the rls_service_read_role sql feature.

The feature emits a select-only policy for one database role, so a platform
service reads the rows of every tenant without a system-tenant session gaining
the same reach. It must not be emitted without tenant isolation, on a junction,
or with a name that is not a psql variable.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_rls_service_read_role.py
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
        "entity_singular": "route",
        "has_tenant_id": True,
        "sql": {"rls_tenant_isolation": True, **sql},
    }


def test_a_service_role_with_tenant_isolation_is_accepted():
    validate_rls_isolation(model(rls_service_read_role="marketdata_service_user"))


def test_a_service_role_without_tenant_isolation_is_refused():
    bad = model(rls_service_read_role="marketdata_service_user")
    bad["sql"]["rls_tenant_isolation"] = False
    with pytest.raises(ValueError, match="requires rls_tenant_isolation"):
        validate_rls_isolation(bad)


def test_a_junction_is_refused():
    bad = model(rls_service_read_role="marketdata_service_user")
    del bad["entity_singular"]
    bad["name_singular"] = "link"
    with pytest.raises(ValueError, match="domain entities only"):
        validate_rls_isolation(bad)


@pytest.mark.parametrize("name", ["Marketdata", "a b", "a;drop", "1abc"])
def test_a_name_that_is_not_a_psql_variable_is_refused(name):
    with pytest.raises(ValueError, match="psql variable"):
        validate_rls_isolation(model(rls_service_read_role=name))


def test_the_policy_is_select_only_and_names_the_role():
    block = TEMPLATE[TEMPLATE.index("{{#domain_entity.sql.rls_service_read_role}}"):
                     TEMPLATE.index("{{/domain_entity.sql.rls_service_read_role}}")]
    assert "for select" in block
    assert "to :{{domain_entity.sql.rls_service_read_role}}" in block
    assert "for all" not in block
