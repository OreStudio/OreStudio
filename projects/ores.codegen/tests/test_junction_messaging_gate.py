"""Tests for the junction messaging-facet gate in ``resolve_targets``.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_junction_messaging_gate.py

A junction's messaging layer exists to serve parent-scoped list reads
(``:list_by:`` on a junction side). The gate in ``resolve_targets`` drops
the four messaging facets for a junction whose left and right sides both
lack ``:list_by:``, so no regenerated stack sits without a subscriber.
The two live orgs below are the pair the gate must keep apart: the dq
``dataset_bundle_member`` junction declares ``:list_by:`` on its left
side, and the refdata ``tenor_convention_resolution`` junction declares
none.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DECLARING_JUNCTION = (
    REPO_ROOT / "projects/ores.dq/modeling/ores.dq.dataset_bundle_member_junction.org"
)
BARE_JUNCTION = (
    REPO_ROOT / "projects/ores.refdata/modeling/ores.refdata.tenor_convention_resolution.org"
)

# The six archetypes the four messaging facets resolve for a junction:
# handler, registrar header/impl, protocol header, service header/impl.
MESSAGING_TEMPLATES = frozenset({
    "cpp_nats_handler.hpp.mustache",
    "cpp_nats_registrar.hpp.mustache",
    "cpp_nats_registrar.cpp.mustache",
    "cpp_protocol.hpp.mustache",
    "cpp_service.hpp.mustache",
    "cpp_service.cpp.mustache",
})


def test_list_by_declaring_junction_keeps_full_messaging_stack():
    units, model_type, _ = resolve_targets(DECLARING_JUNCTION, CODEGEN_BASE)
    assert model_type == "junction"
    templates = {u["template"] for u in units}
    assert MESSAGING_TEMPLATES <= templates


def test_bare_junction_drops_only_the_messaging_units():
    bare_units, _, _ = resolve_targets(BARE_JUNCTION, CODEGEN_BASE)
    declaring_units, _, _ = resolve_targets(DECLARING_JUNCTION, CODEGEN_BASE)
    bare_templates = {u["template"] for u in bare_units}
    declaring_templates = {u["template"] for u in declaring_units}
    assert not (MESSAGING_TEMPLATES & bare_templates)
    # The gate removes exactly the six messaging units: everything else
    # the declaring junction resolves (SQL create, Qt, domain, repository)
    # resolves unchanged for a junction with no :list_by: at all.
    assert bare_templates == declaring_templates - MESSAGING_TEMPLATES
