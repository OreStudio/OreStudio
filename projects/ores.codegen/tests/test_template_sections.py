"""Tests for projects/ores.codegen/scripts/check_template_sections.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_template_sections.py

The rule is about the renderer's standalone-line stripping: a line holding only
a section tag leaves nothing behind when the section is empty, and a line
sharing the tag with anything else keeps its newline. The cases below pin the
detector, and pin the baseline to failing in both directions, because a baseline
that only fails on growth leaves room for the next leak.
"""
import importlib.util
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "check_template_sections",
    REPO_ROOT / "projects" / "ores.codegen" / "scripts" / "check_template_sections.py")
check_template_sections = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = check_template_sections
SPEC.loader.exec_module(check_template_sections)


def test_a_section_on_its_own_lines_is_clean():
    text = "a\n{{#s}}\nx\n{{/s}}\nb\n"
    assert check_template_sections.hazards(text) == []


def test_a_section_that_shares_its_lines_is_a_hazard():
    text = "a\n{{#s}}x\n{{/s}}\nb\n"
    assert check_template_sections.hazards(text) == [2]


def test_nested_sections_on_one_line_are_a_hazard():
    text = "a\n{{^x}}{{^y}}\nz\n{{/y}}{{/x}}\nb\n"
    # Both pairs span lines and neither tag stands alone, so both are counted.
    assert check_template_sections.hazards(text) == [2, 2]


def test_a_section_within_one_line_is_clean():
    text = "a\n{{#s}}x{{/s}}\nb\n"
    assert check_template_sections.hazards(text) == []


def write(tmp_path, name, text):
    (tmp_path / name).write_text(text, encoding="utf-8")


def run(tmp_path, baseline, capsys):
    templates = tmp_path / "templates"
    templates.mkdir(exist_ok=True)
    spec = tmp_path / "baseline.json"
    spec.write_text(json.dumps({"templates": baseline}), encoding="utf-8")
    sys.argv = ["check_template_sections.py", "--templates", str(templates),
                "--baseline", str(spec)]
    rc = check_template_sections.main()
    capsys.readouterr()
    return rc


def test_a_new_hazard_fails_the_check(tmp_path, capsys):
    templates = tmp_path / "templates"
    templates.mkdir()
    write(templates, "a.mustache", "a\n{{#s}}x\n{{/s}}\nb\n")
    assert run(tmp_path, {}, capsys) == 1


def test_a_hazard_at_the_baseline_passes(tmp_path, capsys):
    templates = tmp_path / "templates"
    templates.mkdir()
    write(templates, "a.mustache", "a\n{{#s}}x\n{{/s}}\nb\n")
    assert run(tmp_path, {"a.mustache": 1}, capsys) == 0


def test_a_baseline_above_the_tree_fails_the_check(tmp_path, capsys):
    templates = tmp_path / "templates"
    templates.mkdir()
    write(templates, "a.mustache", "a\n{{#s}}\nx\n{{/s}}\nb\n")
    assert run(tmp_path, {"a.mustache": 1}, capsys) == 1
