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

PROTOCOL_TEMPLATES = frozenset({
    "cpp_protocol.hpp.mustache",
    "ts_protocol.ts.mustache",
})

# The BFF route descriptor imports the subjects of the entity's generated
# protocol. The operation-owned module need not export the derived names, so
# the descriptor is dropped with the protocol facets while the declaration,
# which names no subject, stays.
BFF_ROUTE_TEMPLATE = "ts_bff_route.ts.mustache"

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


def test_an_owned_protocol_drops_the_bff_route_descriptor(tmp_path):
    """The descriptor would import subjects the owned module may not export."""
    entity = _copy_tenant_type(tmp_path)
    (tmp_path / "ores.iam.tenant_type_messages.org").write_text(
        TENANT_TYPE_MESSAGES, encoding="utf-8")
    _reload_owners()

    templates = _templates(entity)
    assert BFF_ROUTE_TEMPLATE not in templates
    # The declaration names no subject, so the operation's ownership does
    # not touch it; only the descriptor is dropped.
    assert "ts_web_declaration.ts.mustache" in templates


def test_deleting_the_operation_model_restores_the_bff_route(tmp_path):
    entity = _copy_tenant_type(tmp_path)
    operation = tmp_path / "ores.iam.tenant_type_messages.org"
    operation.write_text(TENANT_TYPE_MESSAGES, encoding="utf-8")
    _reload_owners()
    assert BFF_ROUTE_TEMPLATE not in _templates(entity)

    operation.unlink()
    _reload_owners()
    assert BFF_ROUTE_TEMPLATE in _templates(entity)


def test_the_real_account_party_junction_now_owns_its_protocol(tmp_path):
    """The pair the rule existed for is retired. The real junction declares
    its list read and has no operation model, so it renders its own protocol
    -- and it still keeps its hand-written messaging stack, whose facets the
    model disables."""
    junction = tmp_path / "ores.iam.account_party_junction.org"
    junction.write_text(ACCOUNT_PARTY_JUNCTION.read_text(encoding="utf-8"),
                        encoding="utf-8")
    _reload_owners()

    templates = _templates(junction)
    assert PROTOCOL_TEMPLATES <= templates
    # The hand-written service, handler and registrar stay: their generated
    # facets are disabled in the model.
    assert "cpp_service.hpp.mustache" not in templates
    assert "cpp_nats_handler.hpp.mustache" not in templates
    assert "cpp_nats_registrar.cpp.mustache" not in templates


def test_the_iam_owner_map_no_longer_names_account_party():
    owners = _operation_protocol_owners(str(IAM_MODELING))
    # The junction renders the generic protocol, so no operation model owns
    # it, and an entity with no operation model is absent too.
    for unowned in ("account_party", "tenant_type", "tenant", "tenant_status",
                    "account_type", "account_contact_information"):
        assert ("iam", unowned) not in owners
