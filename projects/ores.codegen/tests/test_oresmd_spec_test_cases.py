"""Every oresmd test case the models declare reaches a generated test.

An oresmd spec declares its cases in a ``* Test cases`` table, and the
generated test templates read those tables by name: ``{{#fx_round_trip}}``,
``{{#power_rejection}}``, ``{{#inflation_projections}}``. A class whose table
has no matching block in the template generates nothing, and nothing says so:
the spec still reads as a list of promised cases, the suite still compiles, and
ctest reports green over a declaration that no compiler ever saw.

The measured instance was power. The class landed in the identity work with
four Round-trip rows, six Rejection rows and a Projections row, and none of
them became a test, because the templates had no ``{{#power_*}}`` block; the
gap was invisible until the generic class was added beside it and both were
wired. This check is the part that does not depend on a reader noticing.

It reads the manifest through the same loader the generator uses, so a table
the generator cannot parse is a table this cannot check -- and it pins a floor
on the number of cases it looked at, so a loader that silently returns nothing
fails here instead of passing vacuously.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_oresmd_spec_test_cases.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import load_org_oresmd_quote_type_model  # noqa: E402

MODEL = REPO_ROOT / "projects/ores.marketdata/modeling/oresmd/model.org"

# The two files the oresmd test templates write. A case declared by a model is
# rendered into one of them, and which one depends on the table it sits in.
GENERATED_TEST_SOURCES = (
    REPO_ROOT / "projects/ores.marketdata/core/tests/oresmd_oresmd_parser_tests.cpp",
    REPO_ROOT / "projects/ores.marketdata/core/tests/oresmd_oresmd_projections_tests.cpp",
)


def _declared_cases():
    """(asset class, table, description) for every case the models declare."""
    model = load_org_oresmd_quote_type_model(MODEL)
    for spec in model["oresmd_quote_types"]:
        for table, rows in sorted((spec.get("test_cases") or {}).items()):
            for row in rows:
                if row.get("description"):
                    yield spec["asset_class"], table, row["description"]


def _generated_text():
    return "\n".join(p.read_text(encoding="utf-8") for p in GENERATED_TEST_SOURCES)


def test_the_models_declare_cases_to_check():
    # Without this, a change to the loader or the tables would empty the check
    # below and leave it passing over nothing.
    declared = list(_declared_cases())
    assert len(declared) > 50
    assert {asset_class for asset_class, _, _ in declared} >= {
        "ir",
        "fx",
        "equity",
        "credit",
        "commodity",
        "correlation",
        "inflation",
        "security",
        "shape_profile",
        "rating",
        "power",
        "generic",
    }


def test_every_declared_case_reaches_a_generated_test():
    generated = _generated_text()
    missing = [
        f"{asset_class} {table}: {description}"
        for asset_class, table, description in _declared_cases()
        if f'TEST_CASE("{description}"' not in generated
    ]
    assert missing == []
