"""Tests that the mapper templates guard a nullable uuid natural key.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_optional_uuid_mapper.py

A uuid column whose C++ type is std::optional<boost::uuids::uuid> cannot be
passed to boost::uuids::to_string directly, and both the repository mapper and
the history field mapper emitted exactly that, so a model with a nullable uuid
natural key produced code that did not compile. The fsm_transition model found
it: its from_state_id is a nullable uuid natural key.

Two things had to be fixed, and both are pinned here. The templates gained a
nullable arm on the natural-key uuid branch, and the loader now sets the
render_is_optional_uuid flag on natural-key and primary-key columns as well as
on plain columns -- without that flag the branch had nothing to test, which is
why the columns loop already handled the case and the natural-key loop did not.
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

Probe entity with a nullable uuid natural key.

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

** machine_id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:natural_key: true
:END:

A non-nullable uuid natural key.

** from_state_id
:PROPERTIES:
:type:        uuid
:cpp_type:    std::optional<boost::uuids::uuid>
:nullable:    true
:natural_key: true
:END:

A nullable uuid natural key.
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


def test_sql_mapper_guards_a_nullable_uuid_natural_key(tmp_path):
    mapper = _generate(tmp_path, "cpp_domain_type_mapper.cpp.mustache", "probe_mapper.cpp")
    assert "if (v.from_state_id) {" in mapper
    assert "r.from_state_id = boost::uuids::to_string(*v.from_state_id);" in mapper
    # the non-nullable natural key is untouched
    assert "r.machine_id = boost::uuids::to_string(v.machine_id);" in mapper


def test_history_field_mapper_guards_a_nullable_uuid_natural_key(tmp_path):
    mapper = _generate(
        tmp_path, "cpp_history_field_mapper.cpp.mustache", "probe_history_field_mapper.cpp"
    )
    assert (
        'v.from_state_id ? boost::uuids::to_string(*v.from_state_id) : std::string{}' in mapper
    )
    assert '"Machine ID", .value = boost::uuids::to_string(v.machine_id)' in mapper
