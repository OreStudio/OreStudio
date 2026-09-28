"""Tests for the report document type.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_doc_report_type.py

A report page is the durable description of one report: the decision it feeds,
who reads it, how often it runs, the ORE XML that configures it, and the test
that exercises it. Nothing enforced the shape before this type existed, and the
one report catalogue in the tree carries no audience in a role's terms and no
configuration at all, which is what a reader about to run or change a report
needs.

These cases call the generator's real entry point, as the investigation
placement cases do, so they test the scaffold an author actually receives
rather than a helper.
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
REPORT_INVENTORY = REPO_ROOT / "doc" / "knowledge" / "reports" / "reports.org"

# The contract's required sections, in the order the contract states them,
# taken from doc/meta/document_type_report.org. The blurb is deliberately not
# here: it is prose before the first heading, not a section.
REQUIRED_SECTIONS = [
    "Purpose",
    "Audience",
    "Frequency",
    "Inputs",
    "Outputs",
    "Measures",
    "ORE configuration",
    "Parameters",
    "Test coverage",
    "Related",
    "See also",
]


def _run(parent, slug, extra=()):
    doc_generate.main([
        "--type", "report",
        "--slug", slug,
        "--parent-dir", str(parent),
        "--title", "Probe report",
        "--description", "A probe of the report scaffold.",
        *extra,
    ])


def _scaffold(parent, slug, extra=()):
    """Scaffold one report and return it, asserting nothing else was written."""
    _run(parent, slug, extra)
    expected = parent / f"report_{slug}.org"
    written = sorted(f.name for f in parent.glob("*.org"))
    assert written == [expected.name], written
    return expected


def _glossary_report_id():
    """The id of the glossary's report entry, which the template links."""
    text = GLOSSARY.read_text(encoding="utf-8")
    assert "\n* Report\n" in text, "the glossary has no * Report entry"
    block = text.split("\n* Report\n", 1)[1]
    found = re.search(r"^:ID:\s*([0-9A-Fa-f-]{36})\s*$", block, re.M)
    assert found, "the glossary's * Report entry carries no :ID:"
    return found.group(1)


def test_the_report_template_is_registered():
    assert doc_generate.TYPE_TO_TEMPLATE["report"] == "doc_report.org.mustache"
    assert (TEMPLATE_DIR / "doc_report.org.mustache").exists()


def test_the_scaffold_is_named_for_its_type(tmp_path):
    # Discovered rather than predicted: the point here is the naming rule, so
    # this case must not restate it through the helper.
    _run(tmp_path, "probe_report")
    written = [f.name for f in tmp_path.glob("*.org")]
    assert written == ["report_probe_report.org"]
    # A caller who passes the prefix must not get it twice.
    _run(tmp_path, "report_second_probe")
    written = sorted(f.name for f in tmp_path.glob("*.org"))
    assert written == ["report_probe_report.org", "report_second_probe.org"]


def test_the_frontmatter_states_the_type_and_the_code(tmp_path):
    written = _scaffold(tmp_path, "probe_report")
    text = written.read_text(encoding="utf-8")
    assert "#+type: report" in text
    assert "#+level: cross" in text
    # The code defaults to the slug, so a definition can refer to the report
    # before an author thinks about the code.
    assert "#+report_code: probe_report" in text


def test_an_explicit_code_overrides_the_slug(tmp_path):
    written = _scaffold(tmp_path, "probe_report", ("--report-code", "probe"))
    text = written.read_text(encoding="utf-8")
    assert "#+report_code: probe" in text


@pytest.mark.parametrize("code", ["Probe", "probe-report", "probe report", "_probe", "1probe"])
def test_a_code_that_is_not_a_key_is_refused(tmp_path, code):
    # The code goes into a seed and into a definition, so a malformed one must
    # fail at scaffold time rather than becoming a lookup that never matches.
    with pytest.raises(SystemExit):
        _run(tmp_path, "probe_report", ("--report-code", code))


def test_every_required_section_is_present_and_in_order(tmp_path):
    written = _scaffold(tmp_path, "probe_report")
    text = written.read_text(encoding="utf-8")
    headings = re.findall(r"^\* (.+?)\s*$", text, re.M)
    assert headings == REQUIRED_SECTIONS, headings


def test_the_blurb_precedes_the_first_heading(tmp_path):
    written = _scaffold(tmp_path, "probe_report")
    text = written.read_text(encoding="utf-8")
    body = text.split("#+startup: inlineimages", 1)[1]
    blurb = body.split("\n* ", 1)[0]
    assert "Probe report" in blurb
    assert not blurb.lstrip().startswith("*")


def test_the_page_links_the_glossary_entry_and_the_inventory(tmp_path):
    written = _scaffold(tmp_path, "probe_report")
    text = written.read_text(encoding="utf-8")
    # Read from the glossary rather than pinned, so a renamed or regenerated
    # entry fails here as a template that needs updating rather than as a test
    # that remembers an old id.
    assert f"[[id:{_glossary_report_id()}][report]]" in text
    # The inventory the page must be listed in, named relative to its folder.
    assert "[[file:./reports.org][Reports]]" in text
    assert REPORT_INVENTORY.exists()
