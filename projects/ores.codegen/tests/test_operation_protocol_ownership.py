"""Tests for the operation-model ownership of an entity's protocol header.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_operation_protocol_ownership.py

One model owns ``<entity>_protocol``. An operation model that declares an
entity's messages (same component, same ``entity_singular``) renders the
header, and the entity or junction model must not render a derived CRUD
header at the same path, where the last writer would silently discard the
other. Without an operation model the derived header stays.

``resolve_targets`` applies the rule, so these cases pin both sides: an owned
entity drops the C++ and TypeScript protocol facets and keeps every other
facet, and deleting the operation model restores the two facets. The
ownership set is read from the directory that holds the entity model, so a
temporary directory with a copied model is a complete component for this
rule.
"""
import shutil
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import (  # noqa: E402
    _operation_protocol_owners,
    resolve_targets,
)

CODEGEN = REPO_ROOT / "projects/ores.codegen"
IAM_MODELING = REPO_ROOT / "projects/ores.iam/modeling"
TENANT_TYPE = IAM_MODELING / "ores.iam.tenant_type.org"
ACCOUNT_PARTY_JUNCTION = IAM_MODELING / "ores.iam.account_party_junction.org"
ACCOUNT_PARTY_MESSAGES = IAM_MODELING / "ores.iam.account_party_messages.org"

PROTOCOL_TEMPLATES = frozenset({
    "cpp_protocol.hpp.mustache",
    "ts_protocol.ts.mustache",
})

# A minimal but complete operation model for tenant_type: the ownership
# rule reads only the frontmatter (component, entity_singular), so one
# message with one mapped field is enough to make it load.
TENANT_TYPE_MESSAGES = """\
:PROPERTIES:
:END:
#+title: ores.iam.tenant_type_messages
#+type: ores.codegen.operation
#+component: iam
#+subcomponent: api
#+entity_singular: tenant_type
#+namespace: ores::iam::messaging
#+brief: Test messages for the ownership rule.
#+filetags: :model:operation:

* Messages

** get_tenant_types_request
:PROPERTIES:
:subject: iam.v1.tenant_types.list
:END:

*** limit
:PROPERTIES:
:cpp_type: int
:END:
"""


def _templates(model_path: Path) -> set:
    units, _, _ = resolve_targets(model_path, CODEGEN)
    return {unit["template"] for unit in units}


def _reload_owners() -> None:
    """Drop the per-directory cache after the fixture changes the disk."""
    _operation_protocol_owners.cache_clear()


def _copy_tenant_type(directory: Path) -> Path:
    entity = directory / "ores.iam.tenant_type.org"
    shutil.copy(TENANT_TYPE, entity)
    return entity


def test_an_entity_with_an_operation_model_renders_no_derived_protocol(tmp_path):
    entity = _copy_tenant_type(tmp_path)
    (tmp_path / "ores.iam.tenant_type_messages.org").write_text(
        TENANT_TYPE_MESSAGES, encoding="utf-8")
    _reload_owners()

    templates = _templates(entity)
    assert not (PROTOCOL_TEMPLATES & templates)
    # The rule drops the two protocol facets only: the entity keeps its
    # remaining C++, SQL, and TypeScript stack.
    assert "cpp_domain_type_class.hpp.mustache" in templates
    assert "sql_schema_domain_entity_create.mustache" in templates
    assert "domain_types.ts.mustache" in templates
    assert "ts_ui.ts.mustache" in templates


def test_deleting_the_operation_model_restores_the_derived_protocol(tmp_path):
    entity = _copy_tenant_type(tmp_path)
    operation = tmp_path / "ores.iam.tenant_type_messages.org"
    operation.write_text(TENANT_TYPE_MESSAGES, encoding="utf-8")
    _reload_owners()
    assert not (PROTOCOL_TEMPLATES & _templates(entity))

    operation.unlink()
    _reload_owners()
    assert PROTOCOL_TEMPLATES <= _templates(entity)


def test_the_operation_model_owns_the_real_account_party_protocol(tmp_path):
    """The pair the rule exists for: the real junction and its real
    operation model. The committed junction declares no list read, so the
    messaging gate already drops its protocol; the fixture adds one, which
    makes the junction's own derived protocol renderable and lets the
    ownership rule decide instead."""
    text = ACCOUNT_PARTY_JUNCTION.read_text(encoding="utf-8")
    marker = ":column:        account_id"
    assert marker in text
    junction = tmp_path / "ores.iam.account_party_junction.org"
    junction.write_text(
        text.replace(marker, marker + "\n:list_by:       account_id", 1),
        encoding="utf-8")
    operation = tmp_path / "ores.iam.account_party_messages.org"
    shutil.copy(ACCOUNT_PARTY_MESSAGES, operation)
    _reload_owners()

    owned = _templates(junction)
    assert "cpp_protocol.hpp.mustache" not in owned
    # The junction keeps the rest of the messaging stack the list read admits.
    assert "cpp_nats_handler.hpp.mustache" in owned

    operation.unlink()
    _reload_owners()
    assert "cpp_protocol.hpp.mustache" in _templates(junction)


def test_the_iam_owner_map_names_the_real_operation_model():
    owners = _operation_protocol_owners(str(IAM_MODELING))
    assert owners[("iam", "account_party")] == "ores.iam.account_party_messages.org"
    # An entity with no operation model is absent, so its derived header
    # stays: tenant_type, and the four neighbours the defect named.
    for unowned in ("tenant_type", "tenant", "tenant_status",
                    "account_type", "account_contact_information"):
        assert ("iam", unowned) not in owners
