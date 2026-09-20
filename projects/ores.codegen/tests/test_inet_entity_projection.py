"""Tests for the entity-layer projection of a boost::asio::ip::address column.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_inet_entity_projection.py

sqlgen cannot transpile or bind ``boost::asio::ip::address``: its parsing
layer rejects the type with a ``static_assert``. A NOT NULL column whose
domain member is that type therefore reaches the entity struct as
``std::string``, and the mapper converts between the string and the address
-- the same shape a required timestamp already gets.

The projection follows the column's ``:cpp_type:``, not its ``:type: inet``
SQL type. An inet column that already declares ``std::string`` binds as text
and needs no conversion; the asio type breaks sqlgen whatever SQL type it is
declared under. These cases pin that choice.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"

PRIMARY_KEY = """\
** id
:PROPERTIES:
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

Primary key.
"""

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

{columns}
"""


def _ip_column(cpp_type, *, nullable=False):
    """An inet-typed column declaring *cpp_type* for its domain member."""
    return f"""\
** last_ip
:PROPERTIES:
:type:     inet
:cpp_type: {cpp_type}
:nullable: {str(nullable).lower()}
:END:

The address column.
"""


def _render(tmp_path, template, suffix, columns):
    model_path = tmp_path / "ores.testcomp.probe.org"
    model_path.write_text(ENTITY.format(columns=columns), encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True, target_template=template,
        target_output=f"probe.{suffix}")
    return (output_dir / f"probe.{suffix}").read_text(encoding="utf-8")


def _render_entity(tmp_path, columns):
    return _render(
        tmp_path, "cpp_domain_type_entity.hpp.mustache", "hpp", columns)


def _render_mapper(tmp_path, columns):
    return _render(
        tmp_path, "cpp_domain_type_mapper.cpp.mustache", "cpp", columns)


def test_an_address_column_reaches_the_entity_as_a_string(tmp_path):
    """The entity is the database row type, and sqlgen cannot bind an address."""
    entity = _render_entity(
        tmp_path, PRIMARY_KEY + _ip_column("boost::asio::ip::address"))
    assert "std::string last_ip;" in entity
    assert "boost::asio::ip::address last_ip;" not in entity


def test_the_mapper_converts_the_address_column(tmp_path):
    """make_address on read, to_string on write, with the header it needs."""
    mapper = _render_mapper(
        tmp_path, PRIMARY_KEY + _ip_column("boost::asio::ip::address"))
    assert "r.last_ip = boost::asio::ip::make_address(v.last_ip);" in mapper
    assert "r.last_ip = v.last_ip.to_string();" in mapper
    assert "#include <boost/asio/ip/address.hpp>" in mapper


def test_a_string_inet_column_needs_no_conversion(tmp_path):
    """The flag follows the cpp_type, so an already-string inet is untouched.

    Keying the flag on ':type: inet' instead would emit a make_address call
    for a column whose domain member is std::string, which does not compile.
    """
    columns = PRIMARY_KEY + _ip_column("std::string")
    entity = _render_entity(tmp_path, columns)
    assert "std::string last_ip;" in entity

    mapper = _render_mapper(tmp_path, columns)
    assert "r.last_ip = v.last_ip;" in mapper
    assert "make_address" not in mapper
    assert "#include <boost/asio/ip/address.hpp>" not in mapper


def test_a_nullable_address_column_is_refused(tmp_path):
    """A nullable address reaches no entity member, so codegen refuses it.

    Neither the entity template nor the mapper has a shape for a NULL
    address today; refusing is what keeps the column from being dropped
    from the struct in silence. A model that needs one has to spell the
    entity member itself.
    """
    with pytest.raises(ValueError, match="reaches no member"):
        _render_entity(
            tmp_path,
            PRIMARY_KEY + _ip_column("boost::asio::ip::address", nullable=True))
