"""Tests for the service architecture pattern document type.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_doc_service_architecture_pattern_type.py

A pattern page names one established pattern of service interaction, cites the
sources that define it, and states its forces, its failure handling, its
pitfalls and its use in ORE Studio. The pattern hub orders the pages by group,
so a pattern with no group, or with an unknown one, has no place in it.

These cases call the generator's real entry point, as the report type's cases
do, so they test the scaffold an author actually receives.
"""
import re
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import doc_generate  # noqa: E402

TEMPLATE_DIR = REPO_ROOT / "projects" / "ores.codegen" / "library" / "templates"
GLOSSARY = REPO_ROOT / "doc" / "meta" / "glossary.org"
CONTRACT = REPO_ROOT / "doc" / "meta" / "document_type_service_architecture_pattern.org"
HUB = REPO_ROOT / "doc" / "knowledge" / "service_architecture_patterns" / "service_architecture_patterns_hub.org"

# The contract's required sections, in the order the contract states them,
# taken from doc/meta/document_type_service_architecture_pattern.org.
REQUIRED_SECTIONS = [
    "Also known as",
    "Context",
    "Problem",
    "Forces",
    "Solution",
    "Failure handling",
    "Pitfalls",
    "Security",
    "Resulting context",
    "In ORE Studio",
    "Related patterns",
    "Sources",
    "See also",
]


def _run(parent, slug, group="identity"):
    argv = [
        "--type", "service_architecture_pattern",
        "--slug", slug,
        "--parent-dir", str(parent),
        "--title", "Probe pattern",
        "--description", "A probe of the pattern scaffold.",
    ]
    if group is not None:
        argv += ["--pattern-group", group]
    return doc_generate.main(argv)


def _scaffold(parent, slug, group="identity"):
    _run(parent, slug, group)
    expected = parent / f"pattern_{slug}.org"
    written = sorted(f.name for f in parent.glob("*.org"))
    assert written == [expected.name], written
    return expected.read_text(encoding="utf-8")


def _id_of(path, heading=None):
    text = path.read_text(encoding="utf-8")
    if heading:
        assert f"\n{heading}\n" in text, f"{path.name} has no {heading}"
        text = text.split(f"\n{heading}\n", 1)[1]
    found = re.search(r"^:ID:\s*([0-9A-Fa-f-]{36})\s*$", text, re.M)
    assert found, f"no :ID: in {path.name} {heading or ''}"
    return found.group(1)


def test_the_pattern_template_is_registered():
    assert doc_generate.TYPE_TO_TEMPLATE["service_architecture_pattern"] == "doc_service_architecture_pattern.org.mustache"
    assert (TEMPLATE_DIR / "doc_service_architecture_pattern.org.mustache").exists()


def test_the_scaffold_is_named_for_its_type(tmp_path):
    _run(tmp_path, "probe")
    _run(tmp_path, "pattern_second_probe")
    written = sorted(f.name for f in tmp_path.glob("*.org"))
    assert written == ["pattern_probe.org", "pattern_second_probe.org"]


def test_the_frontmatter_states_the_type_and_the_group(tmp_path):
    text = _scaffold(tmp_path, "probe", "resilience")
    assert "#+type: service_architecture_pattern" in text
    assert "#+level: cross" in text
    assert "#+pattern_group: resilience" in text


def test_a_pattern_without_a_group_is_refused(tmp_path):
    with pytest.raises(SystemExit) as refused:
        _run(tmp_path, "probe", group=None)
    assert "--pattern-group is required" in str(refused.value)
    assert list(tmp_path.glob("*.org")) == []


def test_an_unknown_group_is_refused(tmp_path):
    with pytest.raises(SystemExit):
        _run(tmp_path, "probe", group="security")
    assert list(tmp_path.glob("*.org")) == []


def test_the_required_sections_are_present_in_order(tmp_path):
    text = _scaffold(tmp_path, "probe")
    headings = re.findall(r"^\* (.+)$", text, re.M)
    assert headings == REQUIRED_SECTIONS


def test_the_template_links_the_glossary_entry_and_the_hub(tmp_path):
    text = _scaffold(tmp_path, "probe")
    assert f"id:{_id_of(GLOSSARY, '* Service architecture pattern')}" in text
    assert f"id:{_id_of(HUB)}" in text


def test_the_hub_reads_the_groups_in_order():
    text = HUB.read_text(encoding="utf-8")
    positions = [text.find(f"\n** The {g} group") for g in doc_generate.PATTERN_GROUPS]
    assert -1 not in positions, positions
    assert positions == sorted(positions)


def test_the_contract_names_every_group():
    text = CONTRACT.read_text(encoding="utf-8")
    for group in doc_generate.PATTERN_GROUPS:
        assert f"={group}=" in text, group
