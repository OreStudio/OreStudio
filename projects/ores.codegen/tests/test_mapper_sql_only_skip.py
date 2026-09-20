"""Tests that the repository mapper skips a :sql_only: column.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_mapper_sql_only_skip.py

A :sql_only: column is one the domain type deliberately does not carry.
The domain class template guards on the flag and the generator template
guards on it too, but the mapper template did not: for a column whose type
matched a mapper flag (a nullable string, say) it emitted a domain member
assignment for a member the domain struct does not have, and the entity
never compiled. The account model found this -- its
service_password_hash is a nullable string that only the repository's
credential check may read. The fix guards both mapper loops on sql_only.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000041
:END:
#+title: ores.testcomp.probe
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: probe
#+entity_plural: probes
#+entity_title: Probe

Probe entity.

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
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

Primary key.

** secret_hash
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: true
:sql_only: true
:END:

A nullable secret the domain type does not carry.

** label
:PROPERTIES:
:type:     text
:cpp_type: std::string
:END:

A plain string column.
"""


def _generate(tmp_path, template, output):
    model_path = tmp_path / "ores.testcomp.probe.org"
    model_path.write_text(ENTITY, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template=template,
        target_output=output,
    )
    return (output_dir / output).read_text(encoding="utf-8")


def test_mapper_does_not_read_a_sql_only_column_into_the_domain(tmp_path):
    mapper = _generate(tmp_path, "cpp_domain_type_mapper.cpp.mustache", "probe_mapper.cpp")
    assert "secret_hash" not in mapper
    assert "r.label = v.label;" in mapper


def test_mapper_does_not_write_a_sql_only_column_from_the_domain(tmp_path):
    mapper = _generate(tmp_path, "cpp_domain_type_mapper.cpp.mustache", "probe_mapper.cpp")
    assert "r.secret_hash" not in mapper


def test_domain_omits_the_sql_only_column_but_the_entity_keeps_it(tmp_path):
    domain = _generate(tmp_path, "cpp_domain_type_class.hpp.mustache", "probe.hpp")
    assert "secret_hash" not in domain

    entity = _generate(tmp_path, "cpp_domain_type_entity.hpp.mustache", "probe_entity.hpp")
    assert "std::optional<std::string> secret_hash;" in entity


def test_history_field_mapper_skips_a_sql_only_column(tmp_path):
    mapper = _generate(
        tmp_path, "cpp_history_field_mapper.cpp.mustache", "probe_history_field_mapper.cpp"
    )
    assert "secret_hash" not in mapper
    assert "v.label" in mapper
