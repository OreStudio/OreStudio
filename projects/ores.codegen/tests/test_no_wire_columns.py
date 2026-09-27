"""Tests that a :no_wire: column stays in C++ and leaves the wire.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_no_wire_columns.py

The generated domain type doubles as the wire type, so a column on the
struct is a field in every response that carries it. A credential column
must therefore stay readable and writable in C++ while never being
serialized. The model says so with :no_wire: true, and the domain class
template wraps the member in rfl::Skip. The TypeScript twin and the
history field mapper, which render the same struct, must drop it too, or
the field leaves by a second door.
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
:no_wire:  true
:END:

A credential the domain carries and the wire does not.

** label
:PROPERTIES:
:type:     text
:cpp_type: std::string
:END:

A plain string column.
"""

PLAIN_ENTITY = ENTITY.replace(
    "** secret_hash\n:PROPERTIES:\n:type:     text\n:cpp_type: std::string\n"
    ":no_wire:  true\n:END:\n\nA credential the domain carries and the "
    "wire does not.\n\n",
    "")


def _generate(tmp_path, template, output, body=ENTITY):
    model_path = tmp_path / "ores.testcomp.probe.org"
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
        target_output=output,
    )
    return (output_dir / output).read_text(encoding="utf-8")


def test_domain_keeps_the_member_and_skips_it_on_the_wire(tmp_path):
    domain = _generate(tmp_path, "cpp_domain_type_class.hpp.mustache", "probe.hpp")

    assert "rfl::Skip<std::string> secret_hash;" in domain
    assert '#include "ores.utility/rfl/skip_comparison.hpp"' in domain
    # The plain column is untouched, so the flag cannot have spread to it.
    assert "std::string label;" in domain
    assert "rfl::Skip<std::string> label;" not in domain


def test_a_model_without_the_flag_emits_no_skip(tmp_path):
    domain = _generate(
        tmp_path, "cpp_domain_type_class.hpp.mustache", "probe.hpp",
        body=PLAIN_ENTITY)

    assert "std::string label;" in domain
    assert "rfl::Skip" not in domain
    assert "skip_comparison.hpp" not in domain


def test_the_typescript_twin_omits_the_column(tmp_path):
    types = _generate(tmp_path, "domain_types.ts.mustache", "probe.ts")

    assert "secret_hash" not in types
    assert "label" in types


def test_the_history_field_mapper_omits_the_column(tmp_path):
    mapper = _generate(
        tmp_path, "cpp_history_field_mapper.cpp.mustache",
        "probe_history_field_mapper.cpp")

    assert "secret_hash" not in mapper
    assert "v.label" in mapper


def test_the_generated_entity_still_carries_the_column(tmp_path):
    """The repository reads and writes the row through the entity type, so
    the credential has to survive there even though the domain skips it."""
    entity = _generate(
        tmp_path, "cpp_domain_type_entity.hpp.mustache", "probe_entity.hpp")

    assert "secret_hash" in entity
