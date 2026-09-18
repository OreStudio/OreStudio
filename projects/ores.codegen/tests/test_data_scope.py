"""Tests for data-scope (populate/seed) generation wiring.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_data_scope.py

Covers the three seams the data-scope path adds on top of the physical-space
graph: the dataset model loader, the resolve_output_path ``dataset`` branch,
and resolve_targets threading each archetype's ``#+data_source:`` through to
its unit. The fixtures are written to a tmp directory, so the tests do not
depend on any dataset shipped in the tree.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import resolve_output_path  # noqa: E402
from codegen.org_loader import load_org_dataset_model  # noqa: E402
from codegen.generate import resolve_targets  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"


def make_dataset(tmp_path, name="demo", prefix="demo", payloads=()):
    """Write a minimal dataset model plus its payload files."""
    doc = tmp_path / "dataset_overview.org"
    doc.write_text(
        "#+title: Demo dataset\n"
        "#+type: ores.codegen.dataset\n"
        f"#+name: {name}\n"
        f"#+prefix: {prefix}\n"
        ":PROPERTIES:\n"
        ":ores.sql.populate.enabled: true\n"
        ":END:\n",
        encoding="utf-8")
    for payload in payloads:
        (tmp_path / payload).write_text("[]\n", encoding="utf-8")
    return doc


def test_load_org_dataset_model_reads_name_and_prefix(tmp_path):
    model = load_org_dataset_model(make_dataset(tmp_path))
    assert model == {"dataset": {"name": "demo", "prefix": "demo"}}


def test_load_org_dataset_model_prefix_defaults_to_name(tmp_path):
    doc = tmp_path / "d.org"
    doc.write_text("#+type: ores.codegen.dataset\n#+name: foo\n", encoding="utf-8")
    assert load_org_dataset_model(doc) == {"dataset": {"name": "foo", "prefix": "foo"}}


def test_resolve_output_path_dataset_branch():
    model_data = {"dataset": {"name": "demo", "prefix": "demo"}}
    out = resolve_output_path(
        "projects/ores.sql/populate/{dataset}/{prefix}_catalog_populate.sql",
        model_data, "dataset")
    assert out == "projects/ores.sql/populate/demo/demo_catalog_populate.sql"


def test_resolve_targets_threads_data_source_and_master_name(tmp_path):
    doc = make_dataset(tmp_path, payloads=(
        "catalogs.json", "country_currency.json", "datasets.json",
        "tags.json", "model.json", "manifest.json"))
    units, model_type, model_data = resolve_targets(
        doc, CODEGEN_BASE, address="ores.sql.populate")
    assert model_type == "dataset"
    assert model_data["dataset"]["prefix"] == "demo"

    by_output = {Path(u["output"]).name: u for u in units}
    # The facet opt-in (drawer :ores.sql.populate.enabled:) enables all 9
    # archetypes; each carries its dataset-relative payload.
    assert by_output["demo_catalog_populate.sql"]["data_source"] == "catalogs.json"
    assert by_output["demo_flag_populate.sql"]["data_source"] == "country_currency.json"
    # The master include is the standardised {prefix}_populate.sql, sourced
    # from the batch manifest.
    assert "demo_populate.sql" in by_output
    assert by_output["demo_populate.sql"]["data_source"] == "model.json"
