"""Tests for the rules the CSA models ask the database to enforce.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_csa_rules.py

A netting set has at most one active credit support annex, and a CSA lists
each eligible currency at its own position, so an export keeps ORE's
order. Both rules live in the models as unique indexes; these tests render
the real models and fail if either index is lost.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"
MODELING = REPO_ROOT / "projects/ores.refdata/modeling"


def _create_sql(tmp_path, model_name):
    output_dir = tmp_path / model_name
    output_dir.mkdir()
    generate_from_model(
        str(MODELING / f"ores.refdata.{model_name}.org"), DATA_DIR,
        TEMPLATES_DIR, output_dir, is_processing_batch=True,
        target_template="sql_schema_domain_entity_create.mustache",
        target_output="create.sql")
    return " ".join((output_dir / "create.sql").read_text(encoding="utf-8").split())


def test_a_netting_set_has_at_most_one_active_csa(tmp_path):
    sql = _create_sql(tmp_path, "csa")

    assert ('create unique index if not exists csas_active_set_idx '
            'on "ores_refdata_csas_tbl" (netting_set_id) '
            'where valid_to = ores_utility_infinity_timestamp_fn() '
            'and is_active;') in sql


def test_a_csa_lists_each_position_once(tmp_path):
    sql = _create_sql(tmp_path, "csa_eligible_currency")

    assert ('create unique index if not exists '
            'csa_eligible_currencies_csa_position_idx '
            'on "ores_refdata_csa_eligible_currencies_tbl" (csa_id, position) '
            'where valid_to = ores_utility_infinity_timestamp_fn();') in sql
