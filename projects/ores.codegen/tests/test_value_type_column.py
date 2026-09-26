"""Tests for a value type's representation in the repository layer.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_value_type_column.py

A column whose ``:cpp_type:`` is a value type -- a domain class such as
``cron_expression`` that wraps a string behind a validated constructor --
cannot be serialised by sqlgen, which rejects the type inside its parsing
layer. The model opts in with ``:is_value_type: true`` and the entity layer
carries the string instead, with the mapper converting through the type's own
``to_string()`` and ``from_string()``. The domain layer keeps the value type,
so validation still happens on the way in and on the way out.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F2
:END:
#+title: ores.testcomp.value_type_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: value_type_entity
#+entity_plural: value_type_entities
#+entity_title: Value Type Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

An entity whose one column is a value type.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:has_tenant_id: true
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

The surrogate key.

** schedule_expression
:PROPERTIES:
:type:          text
:cpp_type:      domain::cron_expression
:is_value_type: {flag}
:nullable:      false
:END:

The value type.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_value_type_entities_tbl
:END:
"""


def _render(tmp_path, template, output_name, body):
    model_path = tmp_path / "ores.testcomp.value_type_entity.org"
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
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def _entity(tmp_path, flag="true"):
    return _render(tmp_path, "cpp_domain_type_entity.hpp.mustache",
                   "value_type_entity_entity.hpp", MODEL.format(flag=flag))


def _mapper(tmp_path, flag="true"):
    return _render(tmp_path, "cpp_domain_type_mapper.cpp.mustache",
                   "value_type_entity_mapper.cpp", MODEL.format(flag=flag))


def _domain(tmp_path, flag="true"):
    return _render(tmp_path, "cpp_domain_type_class.hpp.mustache",
                   "value_type_entity.hpp", MODEL.format(flag=flag))


def test_value_type_entity_member_is_the_string_it_wraps(tmp_path):
    entity = _entity(tmp_path)
    assert "std::string schedule_expression;" in entity
    assert "domain::cron_expression schedule_expression;" not in entity


def test_value_type_domain_member_keeps_the_value_type(tmp_path):
    domain = _domain(tmp_path)
    assert "domain::cron_expression schedule_expression;" in domain


def test_value_type_mapper_converts_through_the_type(tmp_path):
    mapper = _mapper(tmp_path)
    assert ("schedule_expression = "
            "domain::cron_expression::from_string(v.schedule_expression).value();") in mapper
    assert "r.schedule_expression = v.schedule_expression.to_string();" in mapper


def test_column_without_the_flag_stays_a_plain_member(tmp_path):
    entity = _entity(tmp_path, flag="false")
    assert "domain::cron_expression schedule_expression;" in entity
