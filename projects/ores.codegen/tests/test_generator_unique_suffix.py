"""Tests for :unique: plain-string column suffixing in the entity generator.

A :unique: plain-string column (e.g. tenant hostname) must produce a
distinct value per synthetic row. faker-derived bases are process
constants, so the generator appends the process counter suffix unless
the model opts out via :no_generator_suffix: -- without it, the second
write of the entity in one process collides on the column's unique
index. See the iam tenant hostname collision this fixes.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_generator_unique_suffix.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

UUID_PK_ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000030
:END:
#+title: ores.testcomp.{entity}
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: {entity}
#+entity_plural: {entity}s
#+entity_title: {entity}
#+coding_scheme: none
#+image_id: false

Test entity with a uuid primary key and a :unique: hostname-style column.

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

{extra_columns}
* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_{table}_tbl
:END:

* C++

** Flags
:PROPERTIES:
:subcomponent: api
:END:

** Repository
:PROPERTIES:
:entity_singular_short: {entity_short}
:entity_plural_short:   {entity_short}s
:END:
"""

UNIQUE_HOSTNAME = """\
** hostname
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:unique:   true
:END:

Unique routing hostname.

#+begin_src cpp :name generator
std::string(faker::word::noun()) + ".example.com"
#+end_src

"""

NATURAL_KEY_CODE = """\
** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:natural_key: true
:END:

Natural key.

#+begin_src cpp :name generator
std::string(faker::word::noun()) + "_tenant"
#+end_src

"""

PLAIN_DESCRIPTION = """\
** description
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

Plain column.

"""


def _generate_generator(tmp_path, entity, extra_columns, table=None):
    body = UUID_PK_ENTITY.format(
        entity=entity,
        extra_columns=extra_columns,
        table=table or entity,
        entity_short=entity.removesuffix("_entity"),
    )
    model_path = tmp_path / f"ores.testcomp.{entity}.org"
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
        target_output=f"{entity}_generator.cpp",
    )
    return (output_dir / f"{entity}_generator.cpp").read_text(encoding="utf-8")


def test_unique_column_with_natural_key_gets_suffix(tmp_path):
    generator = _generate_generator(
        tmp_path, "unique_suffix_entity",
        NATURAL_KEY_CODE + UNIQUE_HOSTNAME + PLAIN_DESCRIPTION)
    assert "static std::atomic<int> counter{0};" in generator
    # One declaration, shared by the natural key and the unique column.
    assert generator.count(
        "const auto idx = counter.fetch_add(1, std::memory_order_relaxed);") == 1
    # Layout-tolerant: clang-format may keep the generator's own line break
    # after the "-" literal or rejoin the statement onto one line.
    assert ('r.code = std::string(faker::word::noun()) + "_tenant" + "-"'
            in generator)
    assert ('r.hostname = std::string(faker::word::noun()) + ".example.com" + "-"'
            in generator)
    assert "+ std::to_string(idx);" in generator
    assert "r.description = std::string(faker::word::noun());" in generator


def test_unique_column_without_natural_keys_declares_idx(tmp_path):
    generator = _generate_generator(
        tmp_path, "unique_only_entity", UNIQUE_HOSTNAME + PLAIN_DESCRIPTION)
    assert "static std::atomic<int> counter{0};" in generator
    assert ("const auto idx = counter.fetch_add(1, std::memory_order_relaxed);"
            in generator)
    assert ('r.hostname = std::string(faker::word::noun()) + ".example.com" + "-"'
            in generator)
    assert "+ std::to_string(idx);" in generator


def test_no_unique_column_means_no_counter(tmp_path):
    generator = _generate_generator(
        tmp_path, "plain_uuid_entity", PLAIN_DESCRIPTION)
    assert "static std::atomic<int> counter" not in generator
    assert "std::to_string(idx)" not in generator


def test_no_generator_suffix_opts_out_of_unique_suffix(tmp_path):
    opted_out = UNIQUE_HOSTNAME.replace(":unique:   true",
                                        ":unique:   true\n"
                                        ":no_generator_suffix: true")
    generator = _generate_generator(
        tmp_path, "unsuffixed_entity", opted_out + PLAIN_DESCRIPTION)
    assert "static std::atomic<int> counter{0};" not in generator
    assert 'r.hostname = std::string(faker::word::noun()) + ".example.com";' in generator
