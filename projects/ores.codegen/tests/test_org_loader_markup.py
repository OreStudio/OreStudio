"""Regression tests for _strip_org_markup's verbatim-marker boundaries.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_org_loader_markup.py

The verbatim regex must not pair bare equals signs across words: prose
like "0=Sunday..6=Saturday" (a code-value legend in a model doc) is not
markup, and the word-boundary guards on both sides keep it intact while
real =verbatim= spans still strip. Regression for the calendar_rule doc
sign-off in the refdata/compute drift task.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import _strip_org_markup  # noqa: E402


def test_prose_equals_signs_survive_stripping():
    assert _strip_org_markup("0=Sunday..6=Saturday") == "0=Sunday..6=Saturday"


def test_prose_state_legend_survives_stripping():
    assert _strip_org_markup("1=Inactive, 2=Unsent") == "1=Inactive, 2=Unsent"


def test_real_verbatim_span_strips():
    assert _strip_org_markup("use =foo= here") == "use foo here"
