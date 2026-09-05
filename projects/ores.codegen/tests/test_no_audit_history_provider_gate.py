"""Tests for the no-audit history-provider gate in ``resolve_targets``.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_no_audit_history_provider_gate.py

The per-entity history-provider registrar renders every version's actor
through ``build_entity_history_versions()``, which requires the
entity's domain type to carry ``modified_by``. A ``:no_audit_columns:``
entity (hypertable time-series rows that deliberately carry no actor
stamps) has no such member, so the generated registrar cannot compile
for it. The gate in ``resolve_targets`` drops the registrar facet for
such entities. The two live orgs below are the pair the gate must keep
apart: the marketdata ``market_fixing`` entity declares
``:no_audit_columns: true``, and its ``feed_binding`` entity declares
none.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
NO_AUDIT_ENTITY = (
    REPO_ROOT / "projects/ores.marketdata/modeling/ores.marketdata.market_fixing.org"
)
AUDITED_ENTITY = (
    REPO_ROOT / "projects/ores.marketdata/modeling/ores.marketdata.feed_binding.org"
)

# The two templates the history-provider-registrar facet resolves for a
# domain_entity: the registrar header and its implementation.
REGISTRAR_TEMPLATES = frozenset({
    "cpp_history_provider_registrar.hpp.mustache",
    "cpp_history_provider_registrar.cpp.mustache",
})


def test_no_audit_entity_resolves_no_history_provider_registrar():
    units, model_type, _ = resolve_targets(NO_AUDIT_ENTITY, CODEGEN_BASE)
    assert model_type == "domain_entity"
    templates = {u["template"] for u in units}
    assert not (REGISTRAR_TEMPLATES & templates)
    # The gate must not over-drop: a no-audit entity still resolves its
    # non-registrar stack, e.g. its SQL create and its domain type.
    assert "sql_schema_domain_entity_create.mustache" in templates
    assert "cpp_domain_type_entity.hpp.mustache" in templates


def test_audited_entity_keeps_history_provider_registrar():
    units, model_type, _ = resolve_targets(AUDITED_ENTITY, CODEGEN_BASE)
    assert model_type == "domain_entity"
    templates = {u["template"] for u in units}
    assert REGISTRAR_TEMPLATES <= templates
