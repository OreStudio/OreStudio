"""Tests for the protocol-dependent C++ facet gate in ``resolve_targets``.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_protocol_dependent_facets_gate.py

The shell's entity unit includes the entity's protocol header, an event
carries that protocol's key record, and the eventing integration test
drives the same types. A model that opts out of ``:ores.cpp.protocol:``
therefore has no types for those facets to name, and the gate in
``resolve_targets`` drops them together instead of leaving units that
cannot compile.

The negative case is a fixture rather than a live model. It was pinned to
the dq ``badge_mapping`` junction, which was then the only model in the
tree that disabled its protocol; that junction now generates its protocol
like any other, and pinning a gate's negative case to a live model makes
the test a statement about the tree instead of about the gate. The fixture
is that junction's model text with the opt-out restored, so it carries a
declared ``:list_by:`` and the junction gate stays out of the way -- the
protocol-dependent gate is the one under test.

The positive case pins the other direction: a junction that keeps its
protocol keeps its shell unit. Without it the gate could pass by dropping
the shell facet everywhere.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"

# The model text the fixture is cut from: a junction with a declared
# :list_by:, so the junction gate does not also fire.
JUNCTION_FIXTURE = (
    REPO_ROOT / "projects/ores.dq/modeling/ores.dq.badge_mapping_junction.org"
)

# A junction on the standard stack, used as the positive control.
PROTOCOL_OPTED_IN = (
    REPO_ROOT / "projects/ores.dq/modeling/ores.dq.dataset_bundle_member_junction.org"
)

# The opt-out, as a model's :PROPERTIES: drawer states it.
OPT_OUT = (
    ":ores.cpp.nats-handler.enabled: false\n"
    ":ores.cpp.nats-sub-registrar.enabled: false\n"
    ":ores.cpp.protocol.enabled: false\n"
    ":ores.cpp.service.enabled: false\n"
)

# The generated shell command unit. Every one of the three templates names
# the entity's derived request types, and the header includes the protocol.
SHELL_COMMAND_TEMPLATES = frozenset({
    "cpp_shell_command_header.hpp.mustache",
    "cpp_shell_command_impl.cpp.mustache",
    "cpp_shell_command_tests.cpp.mustache",
})

# The eventing facets that carry the protocol's key record, and the protocol
# header itself. All of them are dropped by the same gate.
PROTOCOL_NAMING_TEMPLATES = SHELL_COMMAND_TEMPLATES | frozenset({
    "cpp_protocol.hpp.mustache",
    "cpp_nats_changed_event.hpp.mustache",
    "cpp_nats_event_registrar.hpp.mustache",
    "cpp_nats_event_registrar.cpp.mustache",
})

# The stack the gate must not touch: the junction still generates its SQL.
SQL_TEMPLATES = frozenset({
    "sql_schema_junction_artefact_create.mustache",
    "sql_schema_junction_create.mustache",
})


def _opted_out_junction(tmp_path: Path) -> Path:
    """The junction fixture with its protocol, handler and service disabled."""
    text = JUNCTION_FIXTURE.read_text(encoding="utf-8")
    head, sep, tail = text.partition(":END:\n")
    assert sep, f"{JUNCTION_FIXTURE.name} has no property drawer"
    path = tmp_path / JUNCTION_FIXTURE.name
    path.write_text(head + OPT_OUT + sep + tail, encoding="utf-8")
    return path


def test_a_junction_that_suppresses_its_protocol_resolves_no_unit_that_names_it(
        tmp_path):
    units, model_type, _ = resolve_targets(_opted_out_junction(tmp_path), CODEGEN_BASE)
    assert model_type == "junction"
    templates = {u["template"] for u in units}
    assert not (PROTOCOL_NAMING_TEMPLATES & templates)
    # The gate must not over-drop: the junction still resolves its SQL.
    assert SQL_TEMPLATES <= templates


def test_a_junction_that_keeps_its_protocol_keeps_its_shell_unit():
    units, model_type, _ = resolve_targets(PROTOCOL_OPTED_IN, CODEGEN_BASE)
    assert model_type == "junction"
    templates = {u["template"] for u in units}
    assert SHELL_COMMAND_TEMPLATES <= templates
    assert "cpp_protocol.hpp.mustache" in templates
