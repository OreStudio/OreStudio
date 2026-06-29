"""Tests for data-scope (populate/seed) generation wiring.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_data_scope.py

Covers the three seams the data-scope path adds on top of the physical-space
graph: the dataset model loader, the resolve_output_path ``dataset`` branch,
and resolve_targets threading each archetype's ``#+data_source:`` through to
its unit. The end-to-end byte-identical reproduction of the live solvaris
output is verified out-of-band by ores.seeder/seeder.sh.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import resolve_output_path  # noqa: E402
from codegen.org_loader import load_org_dataset_model  # noqa: E402
from codegen.generate import resolve_targets  # noqa: E402

DATASET_MODEL = (
    REPO_ROOT / "projects/ores.seeder/datasets/slovaris/dataset_overview.org"
)
CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"


def test_load_org_dataset_model_reads_name_and_prefix():
    model = load_org_dataset_model(DATASET_MODEL)
    assert model == {"dataset": {"name": "solvaris", "prefix": "solvaris"}}


def test_load_org_dataset_model_prefix_defaults_to_name(tmp_path):
    doc = tmp_path / "d.org"
    doc.write_text("#+type: ores.codegen.dataset\n#+name: foo\n", encoding="utf-8")
    assert load_org_dataset_model(doc) == {"dataset": {"name": "foo", "prefix": "foo"}}


def test_resolve_output_path_dataset_branch():
    model_data = {"dataset": {"name": "solvaris", "prefix": "solvaris"}}
    out = resolve_output_path(
        "projects/ores.sql/populate/{dataset}/{prefix}_catalog_populate.sql",
        model_data, "dataset")
    assert out == "projects/ores.sql/populate/solvaris/solvaris_catalog_populate.sql"


def test_resolve_targets_threads_data_source_and_master_name():
    units, model_type, model_data = resolve_targets(
        DATASET_MODEL, CODEGEN_BASE, address="ores.sql.populate")
    assert model_type == "dataset"
    assert model_data["dataset"]["prefix"] == "solvaris"

    by_output = {Path(u["output"]).name: u for u in units}
    # The facet opt-in (drawer :ores.sql.populate.enabled:) enables all 9
    # archetypes; each carries its dataset-relative payload.
    assert by_output["solvaris_catalog_populate.sql"]["data_source"] == "catalogs.json"
    assert by_output["solvaris_flag_populate.sql"]["data_source"] == "country_currency.json"
    # The master include is the standardised {prefix}_populate.sql, sourced
    # from the batch manifest.
    assert "solvaris_populate.sql" in by_output
    assert by_output["solvaris_populate.sql"]["data_source"] == "model.json"
