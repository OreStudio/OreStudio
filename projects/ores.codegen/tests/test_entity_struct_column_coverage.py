"""Tests for entity-struct column coverage in cpp_domain_type_entity.hpp.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_entity_struct_column_coverage.py

The entity template gives a column its struct member by switching on one
of nine type flags. A column matching none of them is dropped from the
struct with nothing to show for it: the SQL column and the domain member
both still read as present, so no other layer notices, and the field
quietly stops being on the wire. The generator refuses instead, and
:sql_only: is the way a model says the omission is meant.
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
:ID: 00000000-0000-0000-0000-000000000040
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

** label
:PROPERTIES:
:type:     text
:cpp_type: std::string
:END:

A plain string column.
{extra}
"""


def _generate(tmp_path, extra=""):
    model_path = tmp_path / "ores.testcomp.probe.org"
    model_path.write_text(ENTITY.format(extra=extra), encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_domain_type_entity.hpp.mustache",
        target_output="probe_entity.hpp",
    )
    return (output_dir / "probe_entity.hpp").read_text(encoding="utf-8")


def test_a_column_the_template_cannot_express_is_refused(tmp_path):
    """sqlgen cannot bind a Postgres array, so no entity member carries one."""
    with pytest.raises(ValueError, match="reaches no member"):
        _generate(tmp_path, extra="""
** readings
:PROPERTIES:
:type:     numeric(5,4)[]
:cpp_type: std::vector<double>
:nullable: true
:END:

An array column.
""")


def test_a_nullable_enum_reaches_the_struct_as_an_optional(tmp_path):
    """is_enum wants a non-nullable column, but render_is_enum claims the rest.

    The template pairs render_is_enum with an is_enum inversion, so a
    nullable enum lands as std::optional<std::string> rather than being
    dropped. trade's product_type is nullable for exactly this reason: an
    unset product type has to reach the column as NULL, because the
    Postgres enum has no member to spell it.
    """
    entity = _generate(tmp_path, extra="""
** status
:PROPERTIES:
:type:     text
:cpp_type: std::string
:is_enum:  true
:nullable: true
:END:

A nullable enum column.
""")
    assert "std::optional<std::string> status;" in entity


def test_sql_only_declares_the_omission(tmp_path):
    """The same column generates once the model says the omission is meant."""
    entity = _generate(tmp_path, extra="""
** readings
:PROPERTIES:
:type:     numeric(5,4)[]
:cpp_type: std::vector<double>
:nullable: true
:sql_only: true
:END:

An array column the C++ layers do not carry.
""")
    assert "readings" not in entity
    assert "std::string label;" in entity


def test_an_ordinary_column_still_reaches_the_struct(tmp_path):
    entity = _generate(tmp_path)
    assert "std::string label;" in entity
