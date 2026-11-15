"""Tests for group diagrams that no package of the model draws any more.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_er_generate_orphans.py

A group's diagram is named after its package, so a renamed or removed package
leaves its old file beside the index. The check fails on such a file, and a
regeneration removes it with its drawing, so the check can be cleared by
regenerating. Every path is under tmp_path.
"""
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import plantuml_er_generate as peg  # noqa: E402

TEMPLATE = "{{#packages}}{{name}}{{/packages}}\n"
INDEX_TEMPLATE = "{{#packages}}{{name}}\n{{/packages}}"
MODEL = {
    "packages": [{"name": "alpha", "description": "First", "tables": []}],
    "relationships": [],
}


def _main(tmp_path: Path, *extra: str) -> int:
    model_path = tmp_path / "model.json"
    model_path.write_text(json.dumps(MODEL), encoding="utf-8")
    template_path = tmp_path / "er.mustache"
    template_path.write_text(TEMPLATE, encoding="utf-8")
    index_path = tmp_path / "er_index.mustache"
    index_path.write_text(INDEX_TEMPLATE, encoding="utf-8")
    return peg.main(["--model", str(model_path), "--template", str(template_path),
                     "--index-template", str(index_path),
                     "--output", str(tmp_path / "out" / "ores_schema.puml"), *extra])


def _regenerated(tmp_path: Path) -> Path:
    out = tmp_path / "out"
    out.mkdir()
    assert _main(tmp_path) == 0
    return out


def test_the_check_fails_on_a_group_no_package_draws(tmp_path, capsys):
    out = _regenerated(tmp_path)
    (out / "ores_schema.removed.puml").write_text("old\n", encoding="utf-8")

    assert _main(tmp_path, "--check") == 1
    assert "ores_schema.removed.puml (no group of the model draws this file)" in (
        capsys.readouterr().err)


def test_the_check_passes_when_every_group_file_is_drawn(tmp_path):
    _regenerated(tmp_path)

    assert _main(tmp_path, "--check") == 0


def test_a_regeneration_removes_the_orphan_and_its_drawing(tmp_path):
    out = _regenerated(tmp_path)
    (out / "ores_schema.removed.puml").write_text("old\n", encoding="utf-8")
    (out / "ores_schema.removed.svg").write_text("<svg/>\n", encoding="utf-8")

    assert _main(tmp_path) == 0

    assert not (out / "ores_schema.removed.puml").exists()
    assert not (out / "ores_schema.removed.svg").exists()
    assert (out / "ores_schema.alpha.puml").exists()
    assert _main(tmp_path, "--check") == 0


def test_a_file_that_is_not_a_group_diagram_is_left_alone(tmp_path):
    out = _regenerated(tmp_path)
    other = out / "other_schema.puml"
    other.write_text("not a group\n", encoding="utf-8")

    assert _main(tmp_path) == 0
    assert _main(tmp_path, "--check") == 0
    assert other.exists()


def test_the_check_names_an_orphan_and_a_stale_group_together(tmp_path, capsys):
    out = _regenerated(tmp_path)
    (out / "ores_schema.alpha.puml").write_text("drifted\n", encoding="utf-8")
    (out / "ores_schema.removed.puml").write_text("old\n", encoding="utf-8")

    assert _main(tmp_path, "--check") == 1
    err = capsys.readouterr().err
    assert "ores_schema.alpha.puml (1 line differs)" in err
    assert "ores_schema.removed.puml (no group of the model draws this file)" in err
