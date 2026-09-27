"""Regression test for the default a primary-key column declares.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_pk_column_default.py

The create template rendered ``{{#default}} default {{{default}}}{{/default}}``
for every non-key column but not for the key block, so a primary key that
declared ``:default: gen_random_uuid()`` rendered ``"id" uuid not null,`` and
the insert had to supply the surrogate itself. The model's declaration was
dropped in silence, which no gate could see: the dq publication table was the
only table in the tree whose key declares a default, so the omission sat
unnoticed behind one model.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

# The smallest entity that exercises the key block: one uuid key that declares
# a default, and one plain column so the table has a body.
KEY_WITH_DEFAULT_ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000A1
:END:
#+title: ores.testcomp.key_default_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: key_default_entity
#+entity_plural: key_default_entities
#+entity_title: Key Default Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

Entity whose surrogate key declares its own default.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:default:     gen_random_uuid()
:END:

Surrogate identifier.

** name
:PROPERTIES:
:type:     text
:cpp_type: std::string
:END:

A name.
"""


def _generate(tmp_path: Path, template: str, output_name: str) -> str:
    model_path = tmp_path / "ores.testcomp.key_default_entity.org"
    model_path.write_text(KEY_WITH_DEFAULT_ENTITY, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template=template,
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def test_a_key_column_that_declares_a_default_renders_that_default(tmp_path):
    sql = _generate(
        tmp_path,
        "sql_schema_domain_entity_create.mustache",
        "testcomp_key_default_entities_create.sql",
    )
    assert '"id" uuid not null default gen_random_uuid(),' in sql
