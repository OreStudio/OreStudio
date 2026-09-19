"""Tests for the TypeScript projection of an operation model.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_operation_typescript.py

One operation model renders both the C++ header and the TypeScript
module, so the TypeScript spelling of every field comes from the model's
``:cpp_type:``. Two properties make that safe, and both are silent when
they break: a type the table does not know must not project to a
plausible-looking wrong name, and a field the table cannot project must
not merely vanish from the interface.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import (  # noqa: E402
    _ts_type,
    load_org_operation_model,
)

MODEL = """\
{drawer}
#+title: ores.iam.thing_messages
#+type: ores.codegen.operation
#+component: iam
#+subcomponent: api
#+entity_singular: thing
#+namespace: ores::iam::messaging

* Messages

** thing_request

*** name
:PROPERTIES:
:cpp_type: std::string
:END:

*** count
:PROPERTIES:
:cpp_type: std::vector<thing_item>
:END:

** thing_response

*** ok
:PROPERTIES:
:cpp_type: bool
:END:
"""

DRAWER_ENABLED = ":PROPERTIES:\n:ID: 1\n:END:"
DRAWER_DISABLED = (
    ":PROPERTIES:\n:ID: 1\n:ores.ts.protocol.enabled: nil\n:END:"
)


def _write(tmp_path, drawer=DRAWER_ENABLED, body=MODEL):
    path = tmp_path / "ores.iam.thing_messages.org"
    path.write_text(body.format(drawer=drawer), encoding="utf-8")
    return path


def test_scalars_and_vectors_project():
    assert _ts_type("std::string") == "string"
    assert _ts_type("bool") == "boolean"
    assert _ts_type("std::uint64_t") == "number"
    assert _ts_type("std::vector<std::string>") == "string[]"


def test_a_local_message_name_becomes_its_interface_name():
    assert _ts_type("thing_item") == "ThingItem"
    assert _ts_type("std::vector<thing_item>") == "ThingItem[]"


def test_a_domain_type_projects_onto_its_interface():
    assert _ts_type("ores::iam::domain::role") == "Role"
    assert _ts_type("std::vector<ores::iam::domain::session>") == "Session[]"


def test_a_timestamp_projects_onto_a_string():
    assert _ts_type("std::chrono::system_clock::time_point") == "string"


def test_a_type_with_no_projection_stays_none():
    # A component domain type projects to the interface its entity emits,
    # through an optional as well; a utility domain type has no facet output to
    # name, so it stays a gap the guard refuses.
    assert _ts_type("std::optional<ores::iam::domain::role>") == "Role | null"
    assert _ts_type("ores::utility::domain::hierarchy_node") is None
    assert _ts_type("std::vector<ores::utility::domain::hierarchy_node>") is None
    assert _ts_type("std::map<std::string, int>") is None


def test_message_names_are_pascal_cased(tmp_path):
    op = load_org_operation_model(_write(tmp_path))["operation"]
    assert [m["name_pascal"] for m in op["messages"]] == [
        "ThingRequest", "ThingResponse"]


def test_fields_carry_their_typescript_type(tmp_path):
    op = load_org_operation_model(_write(tmp_path))["operation"]
    fields = {f["name"]: f for f in op["messages"][0]["fields"]}
    assert fields["name"]["ts_type"] == "string"
    assert fields["count"]["ts_type"] == "ThingItem[]"


def test_an_unprojectable_type_stops_the_model_loading(tmp_path):
    body = MODEL.replace(
        "std::string", "ores::utility::domain::hierarchy_node")
    with pytest.raises(ValueError, match="ores::utility::domain::hierarchy_node"):
        load_org_operation_model(_write(tmp_path, body=body))


def test_the_drawer_flag_lets_an_unprojectable_type_through(tmp_path):
    body = MODEL.replace(
        "std::string", "ores::utility::domain::hierarchy_node")
    op = load_org_operation_model(
        _write(tmp_path, drawer=DRAWER_DISABLED, body=body))["operation"]
    assert "ts_type" not in op["messages"][0]["fields"][0]


def test_a_fully_projectable_model_needs_no_flag(tmp_path):
    load_org_operation_model(_write(tmp_path))
