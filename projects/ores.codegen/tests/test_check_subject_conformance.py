"""Tests for projects/ores.codegen/scripts/check_subject_conformance.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_check_subject_conformance.py

The specification closes the verb set to eight and fixes the subject grammar
at four snake_case segments. The gate reads the subjects the *models* declare
by hand, because the generator already refuses a bad verb when it derives one,
so these cases are the contract: a good subject passes, a bad one is named with
its reason, and the committed tree is clean against its baseline.
"""
import importlib.util
import json
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "check_subject_conformance",
    REPO_ROOT / "projects" / "ores.codegen" / "scripts" / "check_subject_conformance.py")
check_subject_conformance = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = check_subject_conformance
SPEC.loader.exec_module(check_subject_conformance)


# -- a subject that is inside the protocol ----------------------------------


@pytest.mark.parametrize("subject", [
    "refdata.v1.currencies.get",
    "refdata.v1.currencies.get_many",
    "refdata.v1.currencies.list",
    "refdata.v1.currencies.put",
    "refdata.v1.currencies.put_many",
    "refdata.v1.currencies.delete",
    "refdata.v1.currencies.delete_many",
    "refdata.v1.currencies.list_by_portfolio",
    "iam.v1.roles_permissions.put",
    "iam.v1.tenants_events.created",
    "iam.v1.tenants_events.updated",
    "iam.v1.tenants_events.deleted",
    "iam.v1.ops.switch_party",
])
def test_a_conforming_subject_passes(subject):
    ok, reason = check_subject_conformance.classify(subject)
    assert ok, reason


# -- a subject that is not ------------------------------------------------


@pytest.mark.parametrize("subject,expected", [
    ("workflow.v1.start", "3 segments"),
    ("marketdata.v1.tick", "3 segments"),
    ("iam.v1.roles.permissions.put", "5 segments"),
    ("ore.v1.ore.import.execute", "5 segments"),
    ("iam.v1.accounts.save", "closed set of eight"),
    ("iam.v1.accounts.update", "closed set of eight"),
    ("trading.v1.trades.book", "closed set of eight"),
    ("iam.v1.accounts.change-password", "snake_case"),
    ("assets.v1.images.upload-policy", "snake_case"),
    ("workspace.v1.trade-scope.set", "snake_case"),
    ("iam.v1.tenants_events.frobnicated", "event action"),
    ("refdata.v2.currencies.get", "'v1'"),
])
def test_a_non_conforming_subject_is_named_with_its_reason(subject, expected):
    ok, reason = check_subject_conformance.classify(subject)
    assert not ok
    assert expected in reason, reason


def test_the_ops_namespace_is_exempt_from_the_verb_set():
    # The specification reserves it for domain operations and says nothing
    # about its last segment being one of the eight.
    ok, reason = check_subject_conformance.classify("reporting.v1.ops.run_report")
    assert ok, reason


def test_an_ops_subject_is_still_lower_snake_case():
    ok, reason = check_subject_conformance.classify("reporting.v1.ops.run-report")
    assert not ok
    assert "snake_case" in reason


# -- the tree ---------------------------------------------------------------


def test_the_models_declare_subjects_to_check():
    subjects = check_subject_conformance.declared_subjects()
    assert subjects, "the scan found no :subject: declaration"
    assert all(subject for _, _, subject in subjects)


def test_the_committed_tree_is_clean_against_its_baseline(capsys):
    assert check_subject_conformance.main([]) == 0
    assert "none new" in capsys.readouterr().out


def test_every_baseline_entry_is_a_real_violation():
    # A baseline that lists a conforming subject would hide nothing and claim
    # a defect that is not there, so the file could not be trusted to shrink.
    baseline = check_subject_conformance.load_baseline()
    assert baseline, "the baseline is empty"
    for subject in baseline:
        ok, reason = check_subject_conformance.classify(subject)
        assert not ok, f"{subject} is in the baseline but conforms"


def test_the_baseline_has_no_stale_entry():
    # A subject the tree no longer declares is drift of the other kind: the
    # exception outlived the thing it excepted.
    baseline = check_subject_conformance.load_baseline()
    declared = {subject for _, _, subject in check_subject_conformance.declared_subjects()}
    assert not set(baseline) - declared


def test_the_baseline_names_a_reason_for_every_entry():
    payload = json.loads(check_subject_conformance.BASELINE.read_text())
    for entry in payload["accepted"]:
        assert entry["reason"].strip(), entry["subject"]
        assert entry["where"].strip(), entry["subject"]
