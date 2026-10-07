"""Tests for the check that every open read is on the allow-list.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_open_reads.py
"""
import importlib.util
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
SCRIPT = REPO_ROOT / "projects/ores.codegen/scripts/check_open_reads.py"

spec = importlib.util.spec_from_file_location("check_open_reads", SCRIPT)
check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check)

HANDLER = """\
 * Template: cpp_nats_handler.hpp.mustache
    /**
     * @brief Serves probe.v1.probes.list.
     */
    void list_probes(ores::nats::message msg) {
        auto req_ctx = make();
        if (!has_permission(req_ctx, "probe::probes:read")) {
            return;
        }
    }

    /**
     * @brief Serves probe.v1.probes.get.
     */
    void get_probe(ores::nats::message msg) {
        auto req_ctx = make();
    }

    void get_legs_read(ores::nats::message msg) {
        auto req_ctx = make();
    }

    void put_probe(ores::nats::message msg) {
        auto req_ctx = make();
    }
"""


def test_a_read_without_a_check_is_found_and_a_guarded_read_is_not():
    found = dict(check.open_reads(HANDLER))

    assert "list_probes" not in found
    assert found["get_probe"] == "probe.v1.probes.get"


def test_a_method_without_its_own_comment_is_not_given_another_subject():
    found = dict(check.open_reads(HANDLER))

    assert found["get_legs_read"] == "an unnamed subject"


def test_a_write_is_not_a_read():
    assert "put_probe" not in dict(check.open_reads(HANDLER))


def test_the_allow_list_is_read_from_its_own_section():
    allowed = check.allow_list(check.ALLOW_LIST_DOC.read_text(encoding="utf-8"))

    assert "iam.v1.roles.list" in allowed
    assert "inbox.v1.ops.list_my_notifications" in allowed
    # The access reads table further down names guarded subjects; it is not
    # the allow-list.
    assert "iam.v1.ops.get_account_roles" not in allowed


INJECTED_HANDLER = """\
 * Template: cpp_nats_handler.hpp.mustache
    void composite_as_of(ores::nats::message msg) {
        auto ctx = make();
        auto req = decode<get_party_composite_as_of_request>(msg);
    }

    void guarded_composite_as_of(ores::nats::message msg) {
        auto ctx = make();
        if (!has_permission(ctx, "refdata::parties:read")) {
            return;
        }
        auto req = decode<get_party_composite_as_of_request>(msg);
    }
"""

SUBJECTS = {"get_party_composite_as_of_request": "refdata.v1.ops.get_party_composite_as_of"}


def test_a_method_named_unlike_a_read_is_found_by_its_decoded_request():
    found = dict(check.open_reads(INJECTED_HANDLER, SUBJECTS))

    assert found["composite_as_of"] == "refdata.v1.ops.get_party_composite_as_of"
    assert "guarded_composite_as_of" not in found


def test_a_missing_allow_list_section_is_a_reviewed_failure():
    with pytest.raises(check.MissingAllowList):
        check.allow_list("** How the code obeys it\n")


def test_the_tree_obeys_the_rule():
    assert check.main() == 0
