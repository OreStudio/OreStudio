"""
Tests for compass notify.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_notify.py -v
No network, no database and no shell binary: the account read and the shell run
are the two seams the command talks to, and both are replaced here.
"""

import sys
from pathlib import Path

import pytest

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass_db  # noqa: E402
import compass_notify  # noqa: E402

SCENARIO = """\
:PROPERTIES:
:ID: BE0384AF-AFAA-49B9-B313-19EE86C02733
:END:
#+title: Test scenario: verify the route from a feed to a published curve
#+description: A feed publishes, its ticks store, and the set publishes.
#+type: test_scenario
#+filetags: :sprint_29:v0:

* Steps

** Log in as <persona>

Open the app and sign in as =henry.jones@acme_corporation= with
=Secure-Password-123=.

*** Result

| Field  | Value |
| Status | PASS  |
"""

SCAFFOLD = """\
:PROPERTIES:
:ID: 11111111-1111-4111-8111-111111111111
:END:
#+title: Test scenario: a new one
#+description: Something to check.
#+type: test_scenario

* Steps

** Log in as <persona>

# This must be the scenario's first step. Name the exact account to log in
# as, for example =tenant_admin@acme_corporation= / =Secure-Password-123=.

** (Next manual step the tester should perform.)

* Results
"""


@pytest.fixture
def repo(tmp_path, monkeypatch):
    """A checkout holding one scenario doc, at the path compass resolves.

    The doc graph reads a path against the checkout it indexed, so the fixture
    states the temp directory as that checkout rather than writing into the
    real one.
    """
    import doc_index

    monkeypatch.setattr(doc_index, "REPO_ROOT", tmp_path)
    folder = tmp_path / "doc" / "agile" / "sprint_29" / "story"
    folder.mkdir(parents=True)
    path = folder / "scenario_verify-the-route.org"
    path.write_text(SCENARIO, encoding="utf-8")
    return tmp_path, path


@pytest.fixture
def scaffold(tmp_path):
    path = tmp_path / "scenario_a-new-one.org"
    path.write_text(SCAFFOLD, encoding="utf-8")
    return path


# --- which doc was named ----------------------------------------------------


def test_a_path_names_the_doc(repo):
    root, path = repo

    doc = compass_notify.resolve_scenario(root, str(path))

    assert doc.id == "be0384af-afaa-49b9-b313-19ee86c02733"
    assert doc.title == "Test scenario: verify the route from a feed to a published curve"
    assert doc.doctype == "test_scenario"


def test_an_id_names_the_doc(repo, monkeypatch):
    root, path = repo
    import doc_index

    doc = doc_index.parse_doc(path)
    docs = {doc.id: doc}
    monkeypatch.setattr(doc_index, "load_all", lambda: docs)

    doc = compass_notify.resolve_scenario(root, "BE0384AF")

    assert doc.path == path


def test_a_token_that_names_nothing_is_refused(repo):
    root, _ = repo

    with pytest.raises(SystemExit) as refused:
        compass_notify.resolve_scenario(root, "not-a-file-and-not-an-id")

    assert "is not a document id" in str(refused.value)


# --- the persona a scenario names ------------------------------------------


def test_the_persona_is_read_from_the_first_step(repo):
    _, path = repo

    assert compass_notify.persona_of(path.read_text(encoding="utf-8")) == (
        "henry.jones@acme_corporation"
    )


def test_a_fresh_scaffold_names_no_persona(scaffold):
    assert compass_notify.persona_of(scaffold.read_text(encoding="utf-8")) == ""


def test_the_suggestion_names_the_persona_it_found(repo):
    root, path = repo

    suggestion = compass_notify.notify_suggestion(path, root)

    assert suggestion == (
        "compass notify scenario doc/agile/sprint_29/story/scenario_verify-the-route.org "
        "--to henry.jones@acme_corporation"
    )


def test_the_suggestion_states_a_placeholder_when_the_scaffold_names_nobody(scaffold, tmp_path):
    suggestion = compass_notify.notify_suggestion(scaffold, tmp_path)

    assert suggestion.endswith("--to <username@tenant>")


# --- the command line the raise runs on ------------------------------------


def test_an_empty_link_id_is_an_absent_flag():
    script = compass_notify.raise_script(
        kind_code="scenario.to_run",
        link_route="/development/tests/BE0384AF-AFAA-49B9-B313-19EE86C02733",
        link_id="",
        recipients=["8e940c9e-8aea-4c61-9e99-ec33c4abfe2c"],
        arguments={"scenario": "verify the route"},
    )

    assert "--link_id" not in script
    assert script == [
        "notification_operations",
        "raise-notification",
        "scenario.to_run",
        "/development/tests/BE0384AF-AFAA-49B9-B313-19EE86C02733",
        "8e940c9e-8aea-4c61-9e99-ec33c4abfe2c",
        "--argument",
        '"scenario=verify the route"',
    ]


def test_a_link_id_is_stated_as_a_flag():
    script = compass_notify.raise_script("k", "/r", "row-1", ["a"], {})

    assert script[-2:] == ["--link_id", "row-1"]


def test_no_recipient_is_the_empty_list_token():
    script = compass_notify.raise_script("k", "/r", "", [], {})

    assert script[4] == "__none__"


# --- the recipient ----------------------------------------------------------


class _PsqlResult:
    def __init__(self, stdout):
        self.returncode = 0
        self.stdout = stdout
        self.stderr = ""


def test_the_recipient_is_read_by_username_and_tenant(monkeypatch):
    seen = {}

    def fake_psql(env, *args, **kwargs):
        seen["sql"] = args[-1]
        return _PsqlResult("8e940c9e-8aea-4c61-9e99-ec33c4abfe2c\n")

    monkeypatch.setattr(compass_db, "_psql", fake_psql)

    found = compass_notify.account_id({"ORES_DATABASE_NAME": "db"}, "henry.jones@acme_corporation")

    assert found == "8e940c9e-8aea-4c61-9e99-ec33c4abfe2c"
    assert "a.username = 'henry.jones'" in seen["sql"]
    assert "t.code = 'acme_corporation'" in seen["sql"]


def test_an_address_without_a_tenant_is_refused():
    with pytest.raises(SystemExit) as refused:
        compass_notify.account_id({}, "henry.jones")

    assert "username@tenant" in str(refused.value)


def test_an_unknown_account_is_refused(monkeypatch):
    monkeypatch.setattr(compass_db, "_psql", lambda *a, **k: _PsqlResult(""))

    with pytest.raises(SystemExit) as refused:
        compass_notify.account_id({}, "nobody@acme_corporation")

    assert "no account" in str(refused.value)


# --- the answer the shell prints -------------------------------------------


def test_the_answer_is_the_last_json_line_with_a_notification_id():
    class Completed:
        stdout = (
            "ORE Studio Shell\n"
            '{"result": {"outcome": "ok"}, "notification_id": "abc", "recipient_count": 1}\n'
            "Script complete: 1 command executed.\n"
        )
        returncode = 0

    answer = compass_notify.answer_of(Completed())

    assert answer["notification_id"] == "abc"


def test_no_answer_when_the_shell_printed_none():
    class Completed:
        stdout = "Loading script: /tmp/x.ores\nFailed to parse response\n"
        returncode = 1

    assert compass_notify.answer_of(Completed()) is None
