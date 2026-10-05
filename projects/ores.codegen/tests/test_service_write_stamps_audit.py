"""Regression test: a write stamps the member its audit columns live on.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_service_write_stamps_audit.py

The service template's ``prepare_change`` stamped the domain object whole.
That is right for a flat entity, whose audit columns sit on the struct, and
wrong for every grouped one: a field-grouped entity reaches its audit through
``audit`` and an identity-grouped entity through ``identity`` and ``audit``, so
stamping the struct found no ``modified_by`` to set and left the column empty.
The database's insert trigger refuses an empty name after bootstrap, so every
trade and every bond instrument written through a ``put`` failed with
``modified_by cannot be null or empty``, and the ORE round trip could not run.

The two models are live ones -- one of each shape -- so the test stays about
the shapes rather than about whichever names an org happens to carry today.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"
TRADING_MODELING = REPO_ROOT / "projects/ores.trading/modeling"

# One of each shape: an identity slot beside a flat body, and a flat entity
# end to end.
IDENTITY_GROUPED = ("ores.trading.bond_instrument.org", "bond_instrument_service.cpp")
FLAT = ("ores.trading.bond_issue.org", "bond_issue_service.cpp")


def _render(model_name, output_name, tmp_path):
    output_dir = tmp_path / output_name
    output_dir.mkdir()
    generate_from_model(
        str(TRADING_MODELING / model_name),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_service.cpp.mustache",
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def _prepare_change(rendered):
    """The body of the put path's prepare_change, up to its closing brace."""
    start = rendered.index("::prepare_change(")
    end = rendered.index("\n}\n", start)
    return rendered[start:end]


def test_an_identity_grouped_entity_stamps_its_identity_and_audit(tmp_path):
    body = _prepare_change(_render(*IDENTITY_GROUPED, tmp_path=tmp_path))
    assert "stamp(out.identity," in body
    assert "stamp(out.audit," in body
    assert "stamp(out, ctx_," not in body


def test_a_flat_entity_is_still_stamped_whole(tmp_path):
    body = _prepare_change(_render(*FLAT, tmp_path=tmp_path))
    assert "stamp(out, ctx_," in body
    assert "stamp(out.audit," not in body
