"""A jsonb column's generated sample value is valid JSON."""
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
:ID: 00000000-0000-0000-0000-0000000000F4
:END:
#+title: ores.testcomp.jsonb_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: jsonb_entity
#+entity_plural: jsonb_entities
#+entity_title: Jsonb Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

An entity with a jsonb column.

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

** payload
:PROPERTIES:
:type:     jsonb
:cpp_type: std::string
:nullable: false
:END:

A JSON payload.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_jsonb_entities_tbl
:END:
"""


def _generator(tmp_path):
    model_path = tmp_path / "ores.testcomp.jsonb_entity.org"
    model_path.write_text(MODEL, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_domain_type_generator.cpp.mustache",
        target_output="jsonb_entity_generator.cpp",
    )
    return (output_dir / "jsonb_entity_generator.cpp").read_text(encoding="utf-8")


def test_jsonb_sample_value_is_valid_json(tmp_path):
    generated = _generator(tmp_path)
    assert 'r.payload = std::string("{}");' in generated
    assert "faker::word::noun()" not in generated.split("r.payload")[1].split(";")[0]
