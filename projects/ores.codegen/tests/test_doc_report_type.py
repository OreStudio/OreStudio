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

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import doc_generate  # noqa: E402

TEMPLATE_DIR = REPO_ROOT / "projects" / "ores.codegen" / "library" / "templates"

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


def _scaffold(parent, slug, extra=()):
    """Scaffold one report and return the file it wrote."""
    doc_generate.main([
        "--type", "report",
        "--slug", slug,
        "--parent-dir", str(parent),
        "--title", "Probe report",
        "--description", "A probe of the report scaffold.",
        *extra,
    ])
    written = [f for f in parent.glob("*.org")]
    assert len(written) == 1, written
    return written[0]


def test_the_report_template_is_registered():
    assert doc_generate.TYPE_TO_TEMPLATE["report"] == "doc_report.org.mustache"
    assert (TEMPLATE_DIR / "doc_report.org.mustache").exists()


def test_the_scaffold_is_named_for_its_type(tmp_path):
    written = _scaffold(tmp_path, "probe_report")
    assert written.name == "report_probe_report.org"


def test_the_prefix_is_not_doubled(tmp_path):
    written = _scaffold(tmp_path, "report_probe_report")
    assert written.name == "report_probe_report.org"


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


def test_the_page_links_its_inventory_and_the_glossary(tmp_path):
    written = _scaffold(tmp_path, "probe_report")
    text = written.read_text(encoding="utf-8")
    # The report glossary entry, and the inventory the page must be listed in.
    assert "[[id:A7FCC423-1CA3-4345-BF5B-18D54207B00B][report]]" in text
    assert "[[file:./reports.org][Reports]]" in text
