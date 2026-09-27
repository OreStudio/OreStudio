"""Tests for the ``* Artefact indexes`` section, and for the ``unique``
property it carries.

An artefact index is extra on the staging table rather than on the entity's
own table, so it is declared in its own section. One thing about it is not
text: whether the index is a constraint or a lookup aid. The template asks
the model that question with a section, and a template section treats any
non-empty string as true -- so ``:unique: false`` would have rendered a
unique index. The property is therefore parsed into a boolean, in both
loaders that read the section.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_artefact_indexes.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.org_loader import (  # noqa: E402
    load_org_lookup_entity_model,
    org_document_to_model,
    parse_org,
)

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

HEADER = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000c1
:END:
#+title: ores.testcomp.test_entity
#+type: {model_type}
#+component: testcomp
#+entity_singular: test_entity
#+entity_plural: test_entities
#+entity_title: Test Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

A test entity.
"""

FLAGS = """\

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:END:
"""

COLUMNS = """\

* Columns

** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:primary_key: true
:END:

The key.

** name
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

A plain column.
"""

INDEXES = """\

* Artefact indexes

** natural_key
:PROPERTIES:
:columns: tenant_id, dataset_id, name
{unique}:END:
"""


def _body(unique="", model_type="ores.codegen.entity"):
    return (HEADER.format(model_type=model_type) + FLAGS + COLUMNS
            + INDEXES.format(unique=unique))


def _loaded(unique="", model_type="ores.codegen.entity"):
    """The model's artefact indexes, through the real parser."""
    model = org_document_to_model(parse_org(_body(unique, model_type)))
    return model["domain_entity"]["artefact_indexes"]


def _render(tmp_path, body, template, out):
    model_path = tmp_path / f"{out}.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template=template,
        target_output=f"{out}.sql",
    )
    return (output_dir / f"{out}.sql").read_text(encoding="utf-8")


class TestTheUniqueProperty:
    """A section cannot tell false from true; a decoded flag can."""

    def test_true_parses_to_true(self):
        assert _loaded(":unique: true\n")[0]["unique"] is True

    def test_false_parses_to_false(self):
        # The bug: a template section treats the string "false" as true, so
        # an index declared not unique was rendered unique.
        assert _loaded(":unique: false\n")[0]["unique"] is False

    def test_absent_is_falsy(self):
        # An index that says nothing is a plain index: the property is simply
        # not there, and a template section reads a missing key as false.
        assert not _loaded("")[0].get("unique")

    def test_the_lookup_entity_loader_parses_it_the_same_way(self, tmp_path):
        for unique, expected in ((":unique: true\n", True),
                                 (":unique: false\n", False),
                                 ("", None)):
            path = tmp_path / f"lookup_{expected}_{len(unique)}.org"
            path.write_text(_body(unique, "ores.codegen.lookup_entity"),
                            encoding="utf-8")
            model = load_org_lookup_entity_model(path)
            assert model["entity"]["artefact_indexes"][0].get("unique") is expected

    def test_the_columns_are_kept_verbatim(self):
        # The property that decides the constraint is parsed; the column list
        # is raw text the index definition takes as written.
        assert _loaded(f":unique: true\n")[0]["columns"] == \
            "tenant_id, dataset_id, name"


class TestWhatItRenders:
    """The keyword the index is created with, not just the parsed value."""

    def test_a_unique_index_is_rendered_unique(self, tmp_path):
        sql = _render(tmp_path, _body(":unique: true\n"),
                      "sql_schema_domain_entity_artefact_create.mustache",
                      "unique_index")
        assert ("create unique index if not exists "
                "dq_test_entities_artefact_natural_key_idx") in sql

    def test_an_index_declared_not_unique_is_not_rendered_unique(self, tmp_path):
        sql = _render(tmp_path, _body(":unique: false\n"),
                      "sql_schema_domain_entity_artefact_create.mustache",
                      "plain_index")
        assert ("create index if not exists "
                "dq_test_entities_artefact_natural_key_idx") in sql
        assert "create unique index if not exists " \
               "dq_test_entities_artefact_natural_key_idx" not in sql

    def test_an_index_with_no_unique_property_is_not_rendered_unique(self, tmp_path):
        # The four models that declared artefact indexes before the property
        # existed state no :unique:, so their tables must not change.
        sql = _render(tmp_path, _body(""),
                      "sql_schema_domain_entity_artefact_create.mustache",
                      "default_index")
        assert ("create index if not exists "
                "dq_test_entities_artefact_natural_key_idx") in sql
        assert "create unique index if not exists " \
               "dq_test_entities_artefact_natural_key_idx" not in sql
