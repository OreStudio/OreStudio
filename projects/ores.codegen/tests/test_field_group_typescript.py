"""Tests for the TypeScript projection of field groups.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_field_group_typescript.py

ores.trading was the first field-grouped component projected to
TypeScript. Two things had to work and neither had a test. A field-group
model must render its own interface: the field-group branch once lacked
the ``_to_pascal_case`` and ``_ts_domain_type`` helpers the entity branch
imports. An entity must import each group it composes, from beside it
when its own component declares the group, and from the owner's domain
directory when another component does. Until these cases, only the web
typecheck in CI caught a wrong import path.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
TRADING = REPO_ROOT / "projects/ores.trading/modeling"


def _render(model, tmp_path):
    generate_from_model(
        str(TRADING / model), CODEGEN / "library" / "data",
        CODEGEN / "library" / "templates", tmp_path,
        target_template="domain_types.ts.mustache", target_output="out.ts")
    return (tmp_path / "out.ts").read_text(encoding="utf-8")


def test_field_group_renders_its_own_interface(tmp_path):
    rendered = _render("ores.trading.instrument_identity_field_group.org", tmp_path)

    assert "export interface InstrumentIdentity {" in rendered
    assert "    trade_type_code: string;" in rendered
    assert ": ;" not in rendered


def test_entity_imports_own_and_cross_component_groups(tmp_path):
    rendered = _render("ores.trading.bond_instrument.org", tmp_path)

    assert ("import type { InstrumentIdentity } from "
            "'./instrument_identity.js';") in rendered
    assert ("import type { AuditRecord } from "
            "'../../dq/domain/audit_record.js';") in rendered
