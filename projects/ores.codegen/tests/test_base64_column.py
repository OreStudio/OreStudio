"""A base64 column carries raw bytes in the domain and text in the row.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_base64_column.py

A column can say ``:base64: true`` to mean its database column is ``text``
holding the base64 spelling of a byte vector, while the domain member is the
raw ``std::vector<std::uint8_t>``. The entity layer is a database row type
that sqlgen binds, so it keeps the ``std::string`` the column stores, and the
mapper performs the base64 hop in both directions -- the same shape a
required timestamp gets, with the encoding in place of the datetime
conversion.

The property is explicit rather than derived from ``:type:``: a ``text``
column that is already text is indistinguishable from this one by type
alone, so only the model can say which spelling it holds. A column without
the property is untouched.

These cases pin that choice, and pin that the synthetic generator produces
non-empty bytes rather than a default-constructed empty vector.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000042
:END:
#+title: ores.testcomp.probe
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: probe
#+entity_plural: probes
#+entity_title: Probe
#+coding_scheme: none
#+image_id: false

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

{columns}"""

BASE64_COLUMN = """\
** payload
:PROPERTIES:
:type:     text
:cpp_type: std::vector<std::uint8_t>
:base64:   true
:nullable: false
:END:

The raw bytes, stored base64-encoded.

"""

PLAIN_COLUMN = """\
** note
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

An ordinary text column.

"""


def _render(tmp_path, template, output, columns):
    model_path = tmp_path / "ores.testcomp.probe.org"
    model_path.write_text(ENTITY.format(columns=columns), encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True, target_template=template,
        target_output=output)
    return (output_dir / output).read_text(encoding="utf-8")


def _render_domain(tmp_path, columns):
    return _render(
        tmp_path, "cpp_domain_type_class.hpp.mustache", "probe.hpp", columns)


def _render_entity(tmp_path, columns):
    return _render(
        tmp_path, "cpp_domain_type_entity.hpp.mustache", "probe_entity.hpp", columns)


def _render_mapper(tmp_path, columns):
    return _render(
        tmp_path, "cpp_domain_type_mapper.cpp.mustache", "probe_mapper.cpp", columns)


def _render_generator(tmp_path, columns):
    return _render(
        tmp_path, "cpp_domain_type_generator.cpp.mustache", "probe_generator.cpp", columns)


def test_a_base64_column_is_bytes_in_the_domain_struct(tmp_path):
    """The domain type holds the raw bytes, not their base64 spelling."""
    domain = _render_domain(tmp_path, BASE64_COLUMN + PLAIN_COLUMN)
    assert "std::vector<std::uint8_t> payload;" in domain
    assert "std::string payload;" not in domain
    assert "#include <vector>" in domain


def test_a_base64_column_is_base64_text_in_the_sqlgen_struct(tmp_path):
    """The entity is the database row type, and the column stores text."""
    entity = _render_entity(tmp_path, BASE64_COLUMN + PLAIN_COLUMN)
    assert "std::string payload;" in entity
    assert "std::vector<std::uint8_t> payload;" not in entity


def test_the_mapper_converts_a_base64_column_in_both_directions(tmp_path):
    """Decode on read, encode on write, with the header the conversion needs."""
    mapper = _render_mapper(tmp_path, BASE64_COLUMN + PLAIN_COLUMN)
    conversion = (
        "r.payload = utility::converter::base64_converter::convert(v.payload);")
    assert mapper.count(conversion) == 2
    assert '#include "ores.utility/convert/base64_converter.hpp"' in mapper


def test_the_generator_produces_non_empty_base64_bytes(tmp_path):
    """A byte-vector member gets no faker default, so codegen states bytes."""
    generator = _render_generator(tmp_path, BASE64_COLUMN + PLAIN_COLUMN)
    assert ("r.payload = std::vector<std::uint8_t>{'<', 's', 'v', 'g', '/', '>'};"
            in generator)
    assert "#include <vector>" in generator


def test_a_column_without_the_property_is_left_alone(tmp_path):
    """A plain text column takes no base64 hop and drags in no converter."""
    columns = PLAIN_COLUMN
    domain = _render_domain(tmp_path, columns)
    assert "std::string note;" in domain
    assert "#include <vector>" not in domain

    entity = _render_entity(tmp_path, columns)
    assert "std::string note;" in entity

    mapper = _render_mapper(tmp_path, columns)
    assert "r.note = v.note;" in mapper
    assert "base64_converter" not in mapper

    generator = _render_generator(tmp_path, columns)
    assert "base64_converter" not in generator
