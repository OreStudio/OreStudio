"""Tests for the * Foreign keys drawer -> template-ready dict conversion.

Regression coverage for the bug fixed alongside this test: calendar_rule
and calendar_exception's .org models set :referenced_column:, a property
name sql_schema_domain_entity_create.mustache never reads (only
:target_column: is consumed) -- the override was a silent no-op, and
every generated insert trigger checked a nonexistent `id` column against
`calendars`, whose real primary key is `code`. There is deliberately no
`referenced_column` alias; see org_loader.py's docstring for why.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_org_loader_foreign_keys.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import parse_org, org_document_to_model  # noqa: E402

REQUIRED_FLAGS = """\
* Flags
:PROPERTIES:
:schema: public
:product: ores
:component: refdata
:END:
"""


def _foreign_keys(body: str) -> list:
    doc = parse_org(REQUIRED_FLAGS + body)
    de = org_document_to_model(doc)["domain_entity"]
    return de["foreign_keys"]


def test_target_column_defaults_to_id_when_unset():
    [fk] = _foreign_keys(
        """
* Foreign keys

** owner_id
:PROPERTIES:
:table:         ores_example_owners_tbl
:error_message: Invalid owner_id: %.
:END:
"""
    )
    assert fk["target_column"] == "id"


def test_target_column_honours_explicit_override():
    [fk] = _foreign_keys(
        """
* Foreign keys

** calendar_code
:PROPERTIES:
:table:         ores_refdata_calendars_tbl
:target_column: code
:error_message: Invalid calendar_code: %.
:END:
"""
    )
    assert fk["target_column"] == "code"


def test_parent_seed_block_becomes_parent_seed_snippet():
    """A named parent_seed source block under the FK heading is captured
    verbatim: the eventing test emits it for parent tables that have no
    modeling org (hand-authored tables such as iam accounts)."""
    [fk] = _foreign_keys(
        """
* Foreign keys

** account_id
:PROPERTIES:
:table:         ores_iam_accounts_tbl
:nullable:      false
:error_message: Invalid account_id: %.
:END:

#+begin_src cpp :name parent_seed
    auto account_id_parent = generate_synthetic_account(ctx);
    account_id_parent_repo.write(account_id_parent);
    v.account_id = account_id_parent.id;
#+end_src
"""
    )
    assert fk["parent_seed_snippet"].startswith("    auto account_id_parent")
    assert "v.account_id = account_id_parent.id;" in fk["parent_seed_snippet"]


def test_fk_without_parent_seed_block_has_no_snippet():
    [fk] = _foreign_keys(
        """
* Foreign keys

** owner_id
:PROPERTIES:
:table:         ores_example_owners_tbl
:error_message: Invalid owner_id: %.
:END:
"""
    )
    assert "parent_seed_snippet" not in fk


def test_referenced_column_is_not_a_recognised_override():
    """The dead key from the calendar_rule/calendar_exception bug: setting
    :referenced_column: must NOT silently look like a working override --
    target_column must still fall back to its "id" default, exactly the
    way it did (wrongly) in production before this was caught."""
    [fk] = _foreign_keys(
        """
* Foreign keys

** calendar_code
:PROPERTIES:
:table:              ores_refdata_calendars_tbl
:referenced_column:  code
:error_message:      Invalid calendar_code: %.
:END:
"""
    )
    assert fk["target_column"] == "id"
    assert fk.get("referenced_column") == "code"
