"""Tests for the composite family's service, handler and sub-registrar.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_family_service.py

A family is the protocol of a composite entity: one central row plus its parts.
The protocol twins are pinned by ``test_family_model.py``; these cases pin the
service and the NATS handler that turn that protocol into a working round trip:
the four operations, the central row read once and used as the instant for the
parts, the one transaction a write opens, the closing of a part row the request
leaves out, and the permission the handler checks -- the central entity's own
read and write codes rather than a resource nobody was granted.

Nothing here compiles the generated C++. No family model exists in the
repository, so no component builds one; these cases read the rendered text.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))
sys.path.insert(0, str(Path(__file__).resolve().parent))

import test_family_model as family_model  # noqa: E402

from codegen import core  # noqa: E402
from codegen.generate import resolve_targets  # noqa: E402

BASE_DIR = REPO_ROOT / "projects/ores.codegen"

SERVICE = "cpp_service.hpp.mustache"
SERVICE_IMPL = "cpp_service.cpp.mustache"
HANDLER = "cpp_nats_handler.hpp.mustache"
REGISTRAR = "cpp_nats_registrar.hpp.mustache"
REGISTRAR_IMPL = "cpp_nats_registrar.cpp.mustache"


def _render(tmp_path: Path, out_name: str, template: str, **model_kwargs) -> str:
    path = family_model._modeling_dir(tmp_path, **model_kwargs)
    out = tmp_path / "out"
    out.mkdir(exist_ok=True)
    core.generate_from_model(
        str(path), BASE_DIR / "library/data", BASE_DIR / "library/templates",
        out, target_template=template, target_output=out_name)
    return (out / out_name).read_text(encoding="utf-8")


def test_resolve_targets_reaches_the_service_handler_and_registrar(tmp_path):
    """A family that renders no service would generate messages nobody serves."""
    path = family_model._modeling_dir(tmp_path)
    units, model_type, _ = resolve_targets(path, BASE_DIR)
    assert model_type == "family"
    assert sorted(unit["template"] for unit in units) == [
        "cpp_nats_handler.hpp.mustache",
        "cpp_nats_registrar.cpp.mustache",
        "cpp_nats_registrar.hpp.mustache",
        "cpp_protocol.hpp.mustache",
        "cpp_service.cpp.mustache",
        "cpp_service.hpp.mustache",
        "ts_protocol.ts.mustache",
    ]
    outputs = {unit["template"]: unit["output"] for unit in units}
    assert outputs[SERVICE_IMPL].endswith(
        "/src/service/tenor_basis_swap_convention_service.cpp")
    assert outputs[HANDLER].endswith(
        "/messaging/tenor_basis_swap_convention_handler.hpp")
    assert outputs[REGISTRAR_IMPL].endswith(
        "/src/messaging/tenor_basis_swap_convention_registrar.cpp")


def test_the_service_declares_the_four_protocol_operations(tmp_path):
    header = _render(tmp_path, "svc.hpp", SERVICE)
    assert "class ORES_REFDATA_CORE_EXPORT tenor_basis_swap_convention_service {" in header
    assert ("messaging::get_tenor_basis_swap_convention_response "
            "get_tenor_basis_swap_convention(" in header)
    assert ("messaging::get_many_tenor_basis_swap_conventions_response "
            "get_many_tenor_basis_swap_conventions(" in header)
    assert ("messaging::put_tenor_basis_swap_convention_response "
            "put_tenor_basis_swap_convention(" in header)
    assert ("messaging::put_many_tenor_basis_swap_conventions_response "
            "put_many_tenor_basis_swap_conventions(" in header)
    # One repository per table the family writes: the central row and each part.
    assert "repository::convention_repository central_repo_;" in header
    assert "repository::tenor_basis_swap_pay_receive_repository pay_receive_repo_;" in header
    assert "repository::tenor_basis_swap_long_short_repository long_short_repo_;" in header


def test_the_read_takes_the_central_row_and_the_parts_at_the_same_instant(tmp_path):
    body = _render(tmp_path, "svc.cpp", SERVICE_IMPL)
    # The central row is read by the key the family addresses it by.
    assert "return repo.read_latest(ctx, boost::uuids::to_string(key.id));" in body
    # Each part is read for that central row, in one query, and assembled.
    assert ("ctx, boost::uuids::to_string(central.id), 0, "
            "std::numeric_limits<std::uint32_t>::max(), {}, std::nullopt"
            in body)
    assert "item.convention = central;" in body
    assert "item.pay_receive = read_pay_receive(pay_receive_repo_, ctx, *central);" in body
    assert "item.long_short = read_long_short(long_short_repo_, ctx, *central);" in body


def test_the_put_opens_one_transaction_and_reconciles_every_part(tmp_path):
    body = _render(tmp_path, "svc.cpp", SERVICE_IMPL)
    assert "ores::database::repository::unit_of_work uow(ctx_);" in body
    assert ("central_repo_.write(ctx, central, request.item.change.precondition);"
            in body)
    assert ("response.result = reconcile_pay_receive(ctx, central, "
            "request.item.pay_receive, request.intent);" in body)
    assert ("response.result = reconcile_long_short(ctx, central, "
            "request.item.long_short, request.intent);" in body)
    assert body.count("uow.commit();") == 2
    # The central row is read back after the parts land, so the item carries
    # the version the part writes bumped.
    assert (body.index("reconcile_pay_receive(ctx, central")
            < body.rindex("auto stored = read_convention"))


def test_the_part_is_written_and_the_rows_the_request_leaves_out_are_closed(tmp_path):
    body = _render(tmp_path, "svc.cpp", SERVICE_IMPL)
    # The requested rows carry the parent key and land as one statement.
    assert "row.convention_id = central.id;" in body
    assert "pay_receive_repo_.write(ctx, wanted, claims);" in body
    assert "long_short_repo_.write(ctx, wanted, claims);" in body
    # A live row absent from the request is closed, so the part's rows for the
    # central row become exactly the rows the request carries.
    assert ("const bool present = std::ranges::any_of(\n"
            "            wanted, [&](const auto& w) { return w.id == row.id; });"
            in body)
    assert "pay_receive_repo_.remove(ctx, boost::uuids::to_string(row.id));" in body
    assert "long_short_repo_.remove(ctx, boost::uuids::to_string(row.id));" in body


def test_the_body_does_not_duplicate_the_parent_version_bump(tmp_path):
    """The part table's insert trigger bumps the central row, not the C++."""
    body = _render(tmp_path, "svc.cpp", SERVICE_IMPL)
    assert "touch_version" not in body
    assert "central.version =" not in body


def test_the_part_takes_the_central_rows_party_when_both_carry_one(tmp_path):
    """A part table has its own party isolation policy, which reads the part's
    own column, so the write copies the central row's party."""
    body = _render(tmp_path, "svc.cpp", SERVICE_IMPL, party_id=True)
    assert body.count("row.party_id = central.party_id;") == 2


def test_a_part_without_its_own_party_column_is_not_stamped_from_the_central_row(tmp_path):
    """The rule is the column's presence, not a hard-coded name: a part with no
    party_id of its own has nothing to copy into."""
    body = _render(tmp_path, "svc.cpp", SERVICE_IMPL)
    assert "row.party_id = central.party_id;" not in body


def test_only_a_part_that_carries_a_party_copies_it(tmp_path):
    """A central row with no party column leaves a part that has one alone."""
    part_with_party = family_model._part_model(
        "tenor_basis_swap_pay_receive",
        "tenor_basis_swap_pay_receives", party_id=True)
    body = _render(tmp_path, "svc.cpp", SERVICE_IMPL, part_overrides={
        "tenor_basis_swap_pay_receive": part_with_party})
    assert "row.party_id = central.party_id;" not in body


def test_the_put_many_is_one_transaction_for_the_whole_batch(tmp_path):
    body = _render(tmp_path, "svc.cpp", SERVICE_IMPL)
    assert body.count("ores::database::repository::unit_of_work uow(ctx_);") == 2
    assert "for (const auto& write : request.items) {" in body
    # Each element states its own claim, and the claim is carried into the
    # write rather than restated from the batch.
    assert "central_repo_.write(ctx, central, write.change.precondition);" in body


def test_the_handler_checks_the_central_entitys_permissions(tmp_path):
    handler = _render(tmp_path, "h.hpp", HANDLER)
    assert handler.count('has_permission(req_ctx, "refdata::conventions:read")') == 2
    assert handler.count('has_permission(req_ctx, "refdata::conventions:write")') == 2
    # The family has no codes of its own; a family read is the central row's
    # read and a family write its write.
    assert "refdata::tenor_basis_swap_conventions:read" not in handler
    assert "refdata::tenor_basis_swap_conventions:write" not in handler


def test_the_handler_dispatches_every_operation(tmp_path):
    handler = _render(tmp_path, "h.hpp", HANDLER)
    for method in ("get_tenor_basis_swap_convention",
                   "get_many_tenor_basis_swap_conventions",
                   "put_tenor_basis_swap_convention",
                   "put_many_tenor_basis_swap_conventions"):
        assert f"void {method}(ores::nats::message msg) {{" in handler
        assert f"auto response = svc.{method}(*req);" in handler
    assert "service::tenor_basis_swap_convention_service svc(req_ctx);" in handler


def test_the_registrar_wires_every_subject_to_the_handler(tmp_path):
    implementation = _render(tmp_path, "r.cpp", REGISTRAR_IMPL)
    for request in ("get_tenor_basis_swap_convention_request",
                    "get_many_tenor_basis_swap_conventions_request",
                    "put_tenor_basis_swap_convention_request",
                    "put_many_tenor_basis_swap_conventions_request"):
        assert f"{request}::nats_subject" in implementation
    assert "std::make_shared<tenor_basis_swap_convention_handler>" in implementation
    header = _render(tmp_path, "r.hpp", REGISTRAR)
    assert "register_tenor_basis_swap_convention_handlers(" in header


def test_a_declared_key_that_is_not_the_storage_key_reads_by_its_own_method():
    """The family key is copied from the central entity, so the read it needs
    is the same declared-key read the central entity's own service uses."""
    model = {
        "component": "refdata",
        "entity_singular": "convention",
        "entity_plural": "conventions",
        "primary_key": {"columns": [
            {"type": "uuid", "cpp_type": "boost::uuids::uuid", "column": "id"}]},
        "columns": [{"name": "code", "type": "text", "cpp_type": "std::string"}],
        "key_field": "code",
    }
    view = core._family_entity_view(model, "central row", "test")
    assert view["key_is_primary"] is False
    assert view["read_method"] == "read_latest_by_code"
    assert view["read_key_expr"] == "key.code"
    assert view["key_member"] == "code"
    # The storage key is still what a part joins to.
    assert view["pk_member"] == "id"


def test_a_uuid_key_is_converted_to_the_text_the_repository_takes():
    model = {
        "component": "refdata",
        "entity_singular": "convention",
        "entity_plural": "conventions",
        "primary_key": {"columns": [
            {"type": "uuid", "cpp_type": "boost::uuids::uuid", "column": "id"}]},
        "columns": [],
    }
    view = core._family_entity_view(model, "central row", "test")
    assert view["read_method"] == "read_latest"
    assert view["read_key_expr"] == "boost::uuids::to_string(key.id)"
    assert view["pk_text_expr"] == "boost::uuids::to_string(row.id)"



GROUPED_PART = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000011
:END:
#+title: ores.refdata.tenor_basis_swap_pay_receive
#+type: ores.codegen.entity
#+component: refdata
#+entity_singular: tenor_basis_swap_pay_receive
#+entity_plural: tenor_basis_swap_pay_receives
#+entity_title: Tenor Basis Swap Pay Receive
#+brief: A grouped family part.

* Flags
:PROPERTIES:
:schema: public
:product: ores
:component: refdata
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

Key.

** convention_id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:END:

The central row this part belongs to.

** party_id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:END:

The party that owns the central row this part belongs to.

* Foreign keys

** convention_id
:PROPERTIES:
:table:               ores_refdata_conventions_tbl
:parent_entity:       convention
:nullable:            false
:error_message:       Invalid convention_id: %.
:list_by:             true
:list_by_uuid:        true
:bump_parent_version: true
:list_by_as_of:       true
:END:

* C++

** Domain groups

| member   | field_group                 |
|----------+-----------------------------|
| identity | ores.refdata.probe_identity |
"""

IDENTITY_GROUP = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000012
:END:
#+title: ores.refdata.probe_identity
#+type: ores.codegen.field_group
#+component: refdata
#+subcomponent: api
#+product: ores
#+entity_singular: probe_identity
#+namespace: ores::refdata::domain
#+brief: The identity of a probe part.

* Fields

** id
:PROPERTIES:
:cpp_type: boost::uuids::uuid
:END:

Key.

** convention_id
:PROPERTIES:
:cpp_type: boost::uuids::uuid
:END:

The central row this part belongs to.

** party_id
:PROPERTIES:
:cpp_type: boost::uuids::uuid
:END:

The party that owns the central row this part belongs to.
"""


def test_a_grouped_part_names_its_columns_through_the_group(tmp_path):
    """A part may be a grouped entity.

    The family service builds a domain object from the flat write record and
    names the key and the party on both sides of an assignment, so on a grouped
    part every one of those paths has to reach through the member the column
    lives in. Rendering a flat path here would not compile.
    """
    path = family_model._modeling_dir(
        tmp_path,
        part_overrides={"tenor_basis_swap_pay_receive": GROUPED_PART},
        party_id=True)
    (tmp_path / "ores.refdata.probe_identity_field_group.org").write_text(
        IDENTITY_GROUP, encoding="utf-8")

    out = tmp_path / "out"
    out.mkdir(exist_ok=True)
    core.generate_from_model(
        str(path), BASE_DIR / "library/data", BASE_DIR / "library/templates",
        out, target_template=SERVICE_IMPL, target_output="service.cpp")
    text = (out / "service.cpp").read_text(encoding="utf-8")

    assert "v.identity.id = write.id;" in text
    assert "row.identity.party_id = central.party_id;" in text
    assert "w.identity.id == row.identity.id" in text
