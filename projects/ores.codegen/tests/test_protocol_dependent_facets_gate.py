"""Tests for the protocol-dependent C++ facet gate in ``resolve_targets``.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_protocol_dependent_facets_gate.py

The shell's entity unit includes the entity's protocol header, an event
carries that protocol's key record, and the eventing integration test
drives the same types. A model that opts out of ``:ores.cpp.protocol:``
therefore has no types for those facets to name, and the gate in
``resolve_targets`` drops them together instead of leaving units that
cannot compile.

The live case is the dq ``badge_mapping`` junction, which suppresses its
protocol, handler and service because a hand-written read-only handler
serves the badge lookup. Its shell unit was emitted anyway, including a
``badge_mapping_protocol.hpp`` nothing generates, and that is what failed
to build. It is the only model in the tree that disables the protocol, so
the negative case is pinned to it rather than to a fixture.

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

# The junction that suppresses its protocol, handler and service.
PROTOCOL_OPTED_OUT = (
    REPO_ROOT / "projects/ores.dq/modeling/ores.dq.badge_mapping_junction.org"
)

# A junction on the standard stack, used as the positive control.
PROTOCOL_OPTED_IN = (
    REPO_ROOT / "projects/ores.dq/modeling/ores.dq.dataset_bundle_member_junction.org"
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


def test_a_junction_that_suppresses_its_protocol_resolves_no_unit_that_names_it():
    units, model_type, _ = resolve_targets(PROTOCOL_OPTED_OUT, CODEGEN_BASE)
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
