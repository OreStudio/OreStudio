"""Tests for data-scope (populate/seed) generation wiring.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_data_scope.py

Covers the seams the data-scope path adds on top of the physical-space graph:
the dataset model loader, the resolve_output_path ``dataset`` branch,
resolve_targets threading each archetype's ``#+data_source:`` through to its
unit, and the image-artefact enrichment that reads a manifest's SVGs. The
fixtures are written to a tmp directory, so the tests do not depend on any
dataset shipped in the tree.
"""
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import pytest  # noqa: E402

from codegen.core import (  # noqa: E402
    _build_image_artefact,
    _build_ip2country_artefact,
    resolve_output_path,
)
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
    # The facet opt-in (drawer :ores.sql.populate.enabled:) enables every
    # archetype in the facet; each carries its dataset-relative payload.
    assert by_output["demo_catalog_populate.sql"]["data_source"] == "catalogs.json"
    assert by_output["demo_currency_populate.sql"]["data_source"] == "country_currency.json"
    # The master include is the standardised {prefix}_populate.sql, sourced
    # from the batch manifest.
    assert "demo_populate.sql" in by_output
    assert by_output["demo_populate.sql"]["data_source"] == "model.json"


def make_image_manifest(tmp_path, svgs, source_dir="external/icons"):
    """Write a repo-shaped manifest plus the SVG files its dataset points at."""
    (tmp_path / ".git").mkdir(exist_ok=True)
    dataset_dir = tmp_path / "projects" / "demo"
    dataset_dir.mkdir(parents=True)
    svg_dir = tmp_path / source_dir
    svg_dir.mkdir(parents=True)
    for key, body in svgs.items():
        (svg_dir / f"{key}.svg").write_text(body, encoding="utf-8")

    manifest = {
        "name": "Demo",
        "datasets": [{
            "name": "Demo Images",
            "subject_area": "Demo Subject Area",
            "domain": "Reference Data",
            "artefact_type": "images",
            "source_dir": source_dir,
            "description_template": "Icon for {key}",
        }],
    }
    manifest_path = dataset_dir / "manifest.json"
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    return manifest_path


def test_image_artefact_reads_svgs_in_key_order_and_translates_names(tmp_path):
    manifest_path = make_image_manifest(tmp_path, {
        "gb": "  <svg id='gb'/>\n",
        "ad": "<svg id='ad'/>",
    })
    artefact = _build_image_artefact(
        json.loads(manifest_path.read_text()), manifest_path)

    # subject_area/domain are the manifest's names; the archetype reads the
    # _name forms, so the enrichment translates them.
    assert artefact["dataset"] == {
        "name": "Demo Images",
        "subject_area_name": "Demo Subject Area",
        "domain_name": "Reference Data",
    }
    assert artefact["count"] == 2
    assert [i["key"] for i in artefact["items"]] == ["ad", "gb"]
    assert [i["description"] for i in artefact["items"]] == [
        "Icon for ad", "Icon for gb"]
    # The whitespace around an SVG document is stripped, matching the SQL the
    # legacy generator emitted.
    assert artefact["items"][1]["svg"] == "<svg id='gb'/>"


def test_image_artefact_is_absent_when_no_dataset_declares_images(tmp_path):
    manifest_path = make_image_manifest(tmp_path, {"ad": "<svg/>"})
    manifest = json.loads(manifest_path.read_text())
    manifest["datasets"][0]["artefact_type"] = "coding_schemes"
    assert _build_image_artefact(manifest, manifest_path) is None


def test_image_artefact_rejects_a_missing_source_dir(tmp_path):
    manifest_path = make_image_manifest(tmp_path, {"ad": "<svg/>"})
    manifest = json.loads(manifest_path.read_text())
    manifest["datasets"][0]["source_dir"] = "external/absent"
    with pytest.raises(FileNotFoundError, match="external/absent"):
        _build_image_artefact(manifest, manifest_path)


def test_resolve_targets_wires_the_image_artefact_to_the_manifest(tmp_path):
    doc = make_dataset(tmp_path, payloads=("manifest.json",))
    units, _, _ = resolve_targets(doc, CODEGEN_BASE, address="ores.sql.populate")
    by_output = {Path(u["output"]).name: u for u in units}
    assert by_output["demo_images_artefact_populate.sql"]["data_source"] == \
        "manifest.json"


def make_ip2country_manifest(tmp_path, data_file="ip2country-v4-u32.tsv"):
    """Write a repo-shaped manifest whose dataset declares an ip2country artefact."""
    (tmp_path / ".git").mkdir(exist_ok=True)
    manifest_dir = tmp_path / "external" / "ip2country"
    manifest_dir.mkdir(parents=True)
    (manifest_dir / data_file).write_text("0\t1\tNone\n", encoding="utf-8")

    manifest = {
        "name": "IP to Country Mapping",
        "sources": [{"name": "iptoasn.com", "data_file": data_file}],
        "datasets": [{
            "name": "IP to Country IPv4 Ranges",
            "subject_area": "IP Address to Country maps",
            "domain": "Reference Data",
            "artefact_type": "ip2country",
        }],
    }
    manifest_path = manifest_dir / "manifest.json"
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    return manifest_path


def test_ip2country_artefact_carries_dataset_names_and_source_path(tmp_path):
    manifest_path = make_ip2country_manifest(tmp_path)
    artefact = _build_ip2country_artefact(
        json.loads(manifest_path.read_text()), manifest_path)

    assert artefact["dataset"] == {
        "name": "IP to Country IPv4 Ranges",
        "subject_area_name": "IP Address to Country maps",
        "domain_name": "Reference Data",
    }
    # The path is repository-relative, because the generated script's own
    # \copy resolves it against the psql client's working directory.
    assert artefact["data_file"] == "external/ip2country/ip2country-v4-u32.tsv"


def test_ip2country_artefact_is_absent_when_no_dataset_declares_it(tmp_path):
    manifest_path = make_ip2country_manifest(tmp_path)
    manifest = json.loads(manifest_path.read_text())
    manifest["datasets"][0]["artefact_type"] = "images"
    assert _build_ip2country_artefact(manifest, manifest_path) is None


def test_ip2country_artefact_rejects_a_manifest_without_a_data_file(tmp_path):
    manifest_path = make_ip2country_manifest(tmp_path)
    manifest = json.loads(manifest_path.read_text())
    manifest["sources"] = [{"name": "iptoasn.com"}]
    with pytest.raises(ValueError, match="no source with a data_file"):
        _build_ip2country_artefact(manifest, manifest_path)


def test_resolve_targets_wires_the_ip2country_artefact_to_the_manifest(tmp_path):
    doc = make_dataset(tmp_path, payloads=("manifest.json",))
    units, _, _ = resolve_targets(doc, CODEGEN_BASE, address="ores.sql.populate")
    by_output = {Path(u["output"]).name: u for u in units}
    assert by_output["demo_artefact_populate.sql"]["data_source"] == "manifest.json"
