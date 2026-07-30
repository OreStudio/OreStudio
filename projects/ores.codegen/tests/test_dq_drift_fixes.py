"""Regression tests for the codegen fixes in the dq entity model drift task
(composite-PK NATS handler args, junction generator counter/idx gating).

Run::

    python3 -m pytest projects/ores.codegen/tests/test_dq_drift_fixes.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

COMPOUND_KEY_ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000010
:END:
#+title: ores.testcomp.compound_key_history_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: compound_key_history_entity
#+entity_plural: compound_key_history_entities
#+entity_title: Compound Key History Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

Test entity with a compound text primary key, used to verify the NATS
handler's history()/remove() pass every key column to the service.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:END:

* Columns

** name
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:primary_key: true
:END:

First key column.

** domain_name
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:nullable:    false
:primary_key: true
:END:

Second key column.

** description
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

A plain column.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_compound_key_history_entities_tbl
:END:

* C++

** Flags
:PROPERTIES:
:subcomponent: api
:END:

** Repository
:PROPERTIES:
:entity_singular_short: entity
:entity_plural_short:   entities
:END:
"""


def _generate_handler(tmp_path, body=COMPOUND_KEY_ENTITY):
    model_path = tmp_path / "ores.testcomp.compound_key_history_entity.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_nats_handler.hpp.mustache",
        target_output="compound_key_history_entity_handler.hpp",
    )
    return (output_dir / "compound_key_history_entity_handler.hpp").read_text(encoding="utf-8")


def test_composite_text_pk_history_passes_every_key_column(tmp_path):
    """A composite text PK's history() must not silently drop every column
    after the first -- see subject_area's name+domain_name key, which the
    handler previously truncated to svc.get_..._history(req->name) alone."""
    handler = _generate_handler(tmp_path)
    assert "svc.get_entity_history(req->name, req->domain_name)" in handler


def test_composite_text_pk_delete_passes_every_key_column(tmp_path):
    handler = _generate_handler(tmp_path)
    assert "svc.delete_entities(req->names, req->domain_names)" in handler


def test_single_column_pk_history_unaffected(tmp_path):
    """A single-column PK entity gets no extra args -- the composite-PK fix
    must be a no-op for the common case."""
    body = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000011
:END:
#+title: ores.testcomp.single_key_history_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: single_key_history_entity
#+entity_plural: single_key_history_entities
#+entity_title: Single Key History Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:END:

* Columns

** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:primary_key: true
:END:

Sole key column.

** description
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

A plain column.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_single_key_history_entities_tbl
:END:

* C++

** Flags
:PROPERTIES:
:subcomponent: api
:END:

** Repository
:PROPERTIES:
:entity_singular_short: entity
:entity_plural_short:   entities
:END:
"""
    handler = _generate_handler(tmp_path, body=body)
    assert "svc.get_entity_history(req->code)" in handler
    assert "svc.get_entity_history(req->code, req->" not in handler


JUNCTION_WITH_IDX = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000020
:END:
#+title: ores.testcomp.needs_counter_junction
#+type: ores.codegen.junction
#+component: testcomp
#+name: needs_counter_junctions
#+name_singular: needs_counter_junction
#+name_title: Needs Counter Junction
#+name_singular_words: junction row
#+product: ores
#+schema: public
#+has_tenant_id: true

Junction whose left code needs the generator's idx suffix for uniqueness.

* Left
:PROPERTIES:
:column:        left_code
:column_short:  left
:column_title:  Left
:type:          text
:cpp_type:      std::string
:END:

#+begin_src cpp :name generator
std::string(faker::word::noun()) + "_" + std::to_string(idx)
#+end_src

* Right
:PROPERTIES:
:column:        right_code
:column_short:  right
:column_title:  Right
:type:          text
:cpp_type:      std::string
:END:

#+begin_src cpp :name generator
std::string(faker::word::noun())
#+end_src

* Columns

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_needs_counter_junctions_tbl
:END:

* C++

** Flags
:PROPERTIES:
:subcomponent: api
:END:

** Domain includes

#+begin_src cpp :name includes
#include <string>
#+end_src

** Entity includes

#+begin_src cpp :name includes
#include <string>
#+end_src

** Conventions
:PROPERTIES:
:iterator_var: m
:END:

* Repository
:PROPERTIES:
:name_singular_short: junction_row
:name_short:          junction_rows
:name_singular_words: junction row
:name_words:          junction rows
:order_column:        left_code
:END:
"""


def _generate_junction_generator(tmp_path, body):
    model_path = tmp_path / "ores.testcomp.needs_counter_junction.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_domain_type_generator.cpp.mustache",
        target_output="needs_counter_junction_generator.cpp",
    )
    return (output_dir / "needs_counter_junction_generator.cpp").read_text(encoding="utf-8")


def test_junction_generator_declares_counter_when_idx_referenced(tmp_path):
    generator = _generate_junction_generator(tmp_path, JUNCTION_WITH_IDX)
    assert "static std::atomic<int> counter{0};" in generator
    assert "const auto idx = counter.fetch_add(1, std::memory_order_relaxed);" in generator


def test_junction_generator_omits_counter_when_idx_unused(tmp_path):
    """A junction whose generator expressions never reference idx (e.g.
    badge_mapping) must not get an unused counter/idx local -- this is what
    the medium-severity review finding on PR #1777 caught."""
    body = JUNCTION_WITH_IDX.replace(
        'std::string(faker::word::noun()) + "_" + std::to_string(idx)',
        'std::string(faker::word::noun())',
    )
    generator = _generate_junction_generator(tmp_path, body)
    assert "static std::atomic<int> counter{0};" not in generator
    assert "const auto idx" not in generator
