"""Tests for the optional tenant of a :nullable_tenant_id: entity.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_nullable_tenant_optional.py

An entity whose * SQL ** Flags drawer sets :nullable_tenant_id: true stores
SQL NULL for a row that belongs to no tenant. The SQL projection already
emits the nullable column; before this test the C++ projection still
declared a non-optional tenant with a system() default, so a NULL row
either threw in the mapper or was silently read back as the system tenant.
The domain and entity tenant are now an std::optional and the mapper
round-trips NULL to nullopt. :nullable_tenant_id: is an SQL flag, so the
C++ flag is lifted from the SQL drawer onto the entity as
=nullable_tenant_id=; an entity that does not set it keeps the old shape.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

# A plain temporal entity that carries a tenant. Modelled on the two
# entities that set :nullable_tenant_id: today (scheduler's
# job_definition and synthetic's market_data_generation_config).
NULLABLE_TENANT_MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000E1
:END:
#+title: ores.testcomp.nullable_tenant_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: nullable_tenant_entity
#+entity_plural: nullable_tenant_entities
#+entity_title: Nullable Tenant Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

An entity whose tenant column admits SQL NULL.

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
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

The key column.

** label
:PROPERTIES:
:type:     text
:cpp_type: std::string
:END:

A plain string column.

* SQL

** Flags
:PROPERTIES:
:tablename:          ores_testcomp_nullable_tenant_entities_tbl
:nullable_tenant_id: true
:END:

* C++

** Repository
:PROPERTIES:
:entity_singular_short: nullable_tenant_entity
:entity_plural_short:   nullable_tenant_entities
:entity_singular_words: nullable tenant entity
:entity_plural_words:   nullable tenant entities
:END:
"""

# The same entity without :nullable_tenant_id: -- the standard-tenant
# control. Its rendered facets must be byte-identical to the pre-change
# templates.
NON_NULLABLE_TENANT_MODEL = NULLABLE_TENANT_MODEL.replace(
    ":nullable_tenant_id: true\n", "")


def _render(tmp_path, template, output_name, body):
    model_path = tmp_path / "ores.testcomp.nullable_tenant_entity.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / output_name
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template=template,
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def _domain(tmp_path, body=NULLABLE_TENANT_MODEL):
    return _render(tmp_path, "cpp_domain_type_class.hpp.mustache",
                   "nullable_tenant_entity.hpp", body)


def _entity(tmp_path, body=NULLABLE_TENANT_MODEL):
    return _render(tmp_path, "cpp_domain_type_entity.hpp.mustache",
                   "nullable_tenant_entity_entity.hpp", body)


def _mapper(tmp_path, body=NULLABLE_TENANT_MODEL):
    return _render(tmp_path, "cpp_domain_type_mapper.cpp.mustache",
                   "nullable_tenant_entity_mapper.cpp", body)


def test_nullable_tenant_domain_declares_an_optional(tmp_path):
    """A NULL tenant has no tenant, so the domain member is an optional."""
    domain = _domain(tmp_path)
    assert "std::optional<utility::uuid::tenant_id> tenant_id;" in domain
    assert "utility::uuid::tenant_id tenant_id = " \
           "utility::uuid::tenant_id::system();" not in domain


def test_nullable_tenant_entity_declares_an_optional_string(tmp_path):
    """The repository entity member carries the SQL NULL as an optional."""
    entity = _entity(tmp_path)
    assert "std::optional<std::string> tenant_id;" in entity
    assert "    std::string tenant_id;" not in entity


def test_non_nullable_tenant_domain_keeps_the_system_default(tmp_path):
    """The control: without the flag the tenant stays non-optional."""
    domain = _domain(tmp_path, NON_NULLABLE_TENANT_MODEL)
    assert "utility::uuid::tenant_id tenant_id = " \
           "utility::uuid::tenant_id::system();" in domain
    assert "std::optional<utility::uuid::tenant_id> tenant_id;" not in domain


def test_non_nullable_tenant_entity_keeps_a_plain_string(tmp_path):
    """The entity control: without the flag the tenant stays a string."""
    entity = _entity(tmp_path, NON_NULLABLE_TENANT_MODEL)
    assert "    std::string tenant_id;" in entity
    assert "std::optional<std::string> tenant_id;" not in entity


def test_nullable_tenant_mapper_maps_null_to_an_empty_optional(tmp_path):
    """A null entity tenant must not reach from_string.

    The guard is what turns SQL NULL into an empty optional instead of a
    parse throw, and the value branch dereferences the optional it tested.
    """
    mapper = _mapper(tmp_path)
    assert mapper.count("if (v.tenant_id)") == 2
    assert "from_string(*v.tenant_id).value()" in mapper
    assert "from_string(v.tenant_id).value()" not in mapper
    assert "v.tenant_id->to_string()" in mapper


def test_nullable_tenant_mapper_maps_an_empty_optional_to_absent(tmp_path):
    """The domain-to-entity direction leaves the member unset for nullopt.

    The entity member defaults to std::nullopt and the guarded assignment
    never runs, so the column reaches SQL as NULL.
    """
    mapper = _mapper(tmp_path)
    assert "r.tenant_id = v.tenant_id->to_string();" in mapper
    assert "r.tenant_id = v.tenant_id.to_string();" not in mapper


def test_non_nullable_tenant_mapper_keeps_the_unguarded_conversion(tmp_path):
    """The mapper control: without the flag both conversions stay direct."""
    mapper = _mapper(tmp_path, NON_NULLABLE_TENANT_MODEL)
    assert "from_string(v.tenant_id).value()" in mapper
    assert "r.tenant_id = v.tenant_id.to_string();" in mapper
    assert "if (v.tenant_id)" not in mapper


def test_typescript_domain_tenant_is_nullable(tmp_path):
    """The wire shape mirrors the C++ optional as a nullable field."""
    ts = _render(tmp_path, "domain_types.ts.mustache",
                 "nullable_tenant_entity.ts", NULLABLE_TENANT_MODEL)
    assert "tenant_id: string | null;" in ts
