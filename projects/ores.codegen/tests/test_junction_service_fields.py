"""Regression test: a junction service writes the response field its protocol declares.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_junction_service_fields.py

A junction repository carries an abbreviated plural (``name_short``) that the
service template used to write as the response collection, while the protocol
names the same field after the resource. A junction with no presentation
drawer gets no name aliasing, so the protocol fell back to the resource plural
and the two disagreed: ``currency_currency_group_service.cpp`` wrote
``response.currency_groups`` where the header declares
``currency_currency_groups``, and the component stopped compiling.

The base model is a live junction, so it stays structurally valid; only the
names are rewritten, which forces the short form to differ from the resource
plural and keeps the test about the disagreement rather than about whichever
names an org happens to carry today.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

# A live junction, used as a valid base rather than copied and frozen.
BASE_JUNCTION = (
    REPO_ROOT
    / "projects/ores.refdata/modeling/ores.refdata.currency_currency_group_junction.org"
)

PLURAL = "widget_thing_links"
SINGULAR = "widget_thing_link"
SHORT = "links"


def _renamed_junction(tmp_path):
    """The base junction with a plural and an abbreviated plural that differ."""
    body = BASE_JUNCTION.read_text(encoding="utf-8")
    body = body.replace("#+name: currency_currency_groups", f"#+name: {PLURAL}")
    body = body.replace("#+name_singular: currency_currency_group",
                        f"#+name_singular: {SINGULAR}")
    body = body.replace(":name_singular_short: currency_group",
                        f":name_singular_short: {SINGULAR}")
    body = body.replace(":name_short:          currency_groups",
                        f":name_short:          {SHORT}")
    assert SHORT in body and PLURAL in body
    model = tmp_path / f"ores.testjunc.{PLURAL}.org"
    model.write_text(body, encoding="utf-8")
    return model


def _render_service(tmp_path):
    model = _renamed_junction(tmp_path)
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    output_name = "widget_thing_link_service.cpp"
    generate_from_model(
        str(model),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_service.cpp.mustache",
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def test_service_uses_the_resource_plural_not_the_abbreviated_one(tmp_path):
    rendered = _render_service(tmp_path)
    assert f"response.{PLURAL} " in rendered
    assert f"response.{SHORT} " not in rendered


def test_the_collection_is_written_and_read_on_the_same_field(tmp_path):
    """Every response field the service writes must be one the header declares."""
    rendered = _render_service(tmp_path)
    written = {
        line.split("response.", 1)[1].split(" =", 1)[0].split(".", 1)[0]
        for line in rendered.splitlines()
        if "response." in line and "response.result" not in line
    }
    assert PLURAL in written
    assert SHORT not in written
