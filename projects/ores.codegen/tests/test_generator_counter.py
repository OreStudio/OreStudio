"""Tests for when a generated generator declares its idx counter.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_generator_counter.py

A generator declares =idx= only when something reads it. Declaring it for a
model whose text natural key takes no suffix left an unused variable, which
the warning gate turns into a build error.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000C9
:END:
#+title: ores.testcomp.coded_record
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: coded_record
#+entity_plural: coded_records
#+entity_title: Coded Record
#+coding_scheme: none
#+image_id: false

A row keyed by a code.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: true
:END:

* Columns

** id
:PROPERTIES:
:type:         uuid
:cpp_type:     boost::uuids::uuid
:primary_key:  true
:END:

The row identity.

** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:natural_key: true
{suffix}:END:

The code.

#+begin_src cpp :name generator
{code_expr}
#+end_src

* SQL
** Flags
:PROPERTIES:
:tablename: ores_testcomp_coded_records_tbl
:END:
"""


def _generator(tmp_path, suffix, code_expr='std::string("A")'):
    model = tmp_path / "ores.testcomp.coded_record.org"
    model.write_text(
        MODEL.format(suffix=":no_generator_suffix: true\n" if not suffix else "",
                     code_expr=code_expr),
        encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True,
        target_template="cpp_domain_type_generator.cpp.mustache",
        target_output="generator.cpp")
    return (output_dir / "generator.cpp").read_text(encoding="utf-8")


def test_a_suffixed_text_key_declares_the_counter(tmp_path):
    cpp = _generator(tmp_path, suffix=True)

    assert "const auto idx = counter.fetch_add" in cpp
    assert '+ std::to_string(idx)' in cpp


def test_a_text_key_without_a_suffix_declares_no_counter(tmp_path):
    cpp = _generator(tmp_path, suffix=False)

    assert "const auto idx" not in cpp


def test_a_snippet_that_names_idx_declares_the_counter(tmp_path):
    cpp = _generator(tmp_path, suffix=False,
                     code_expr='std::string(idx % 2 == 0 ? "A" : "B")')

    assert "const auto idx = counter.fetch_add" in cpp
