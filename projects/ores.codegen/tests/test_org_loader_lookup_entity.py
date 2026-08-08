"""Tests for load_org_lookup_entity_model's ``* Primary key`` heading.

Regression coverage for a bug found while surveying ores.dq for the
entity-classification-drift rollout: every ``*_lookup_entity.org`` model in
the repo declares its key via a dedicated ``* Primary key`` heading
(:column:/:type:/:is_text: on the heading itself), not the domain_entity
convention of a ``** <name>`` sub-heading with :primary_key: true under
``* Columns``. The loader never read the ``* Primary key`` heading, so
entity.primary_key.column rendered empty in sql_schema_table_create.mustache
-- silently corrupting the generated DDL (empty column name in the CREATE
TABLE, PRIMARY KEY, and CHECK clauses) for every lookup entity in the repo.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_org_loader_lookup_entity.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import load_org_lookup_entity_model  # noqa: E402

REQUIRED_FRONTMATTER = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000000
:END:
#+title: ores.dq.widget_type
#+type: ores.codegen.lookup_entity
#+product: ores
#+schema: public
#+component: dq
#+entity_singular: widget_type
#+entity_plural: widget_types
"""


def _model(body: str, tmp_path: Path) -> dict:
    doc_path = tmp_path / "ores.dq.widget_type_lookup_entity.org"
    doc_path.write_text(REQUIRED_FRONTMATTER + body, encoding="utf-8")
    return load_org_lookup_entity_model(doc_path)["entity"]


def test_primary_key_heading_populates_column_and_type(tmp_path):
    e = _model(
        """
* Primary key
:PROPERTIES:
:column:  code
:type:    text
:is_text: true
:END:

* Columns

** name
:PROPERTIES:
:type:     text
:nullable: false
:END:
""",
        tmp_path,
    )
    assert e["primary_key"]["column"] == "code"
    assert e["primary_key"]["type"] == "text"
    assert e["primary_key"]["is_text"] is True
    assert [c["name"] for c in e["columns"]] == ["name"]


def test_primary_key_heading_without_is_text(tmp_path):
    e = _model(
        """
* Primary key
:PROPERTIES:
:column: id
:type:   uuid
:END:

* Columns

** name
:PROPERTIES:
:type: text
:END:
""",
        tmp_path,
    )
    assert e["primary_key"]["column"] == "id"
    assert e["primary_key"]["type"] == "uuid"
    assert "is_text" not in e["primary_key"]
