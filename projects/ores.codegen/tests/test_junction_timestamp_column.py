"""A junction's own timestamp column must reach a serialisable C++ type.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_junction_timestamp_column.py

The junction half of the entity and mapper templates once emitted a column's
raw ``cpp_type``, unlike the entity half, which switches on the type flags.
A junction declaring a ``timestamp with time zone`` column therefore got a
raw ``std::chrono::system_clock::time_point`` in its sqlgen struct, and
sqlgen cannot serialise one: the failure surfaced as a compile error inside
sqlgen's transpilation layer, naming the query rather than the model. The
template now switches on ``is_required_timestamp`` in both directions.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

JUNCTION = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000050
:END:
#+title: ores.testcomp.probe_link
#+description: Test junction.
#+type: ores.codegen.junction
#+component: testcomp
#+filetags: :model:junction:testcomp:
#+name: probe_links
#+name_singular: probe_link
#+name_title: Probe Link
#+name_singular_words: probe link association
#+brief: Links two probes.
#+product: ores
#+schema: public
#+has_tenant_id: true

A junction carrying its own timestamp column.

* Flags
:PROPERTIES:
:profile: tenant-scoped-junction
:END:

* Left
:PROPERTIES:
:column:        left_id
:column_short:  left
:column_title:  Left
:type:          uuid
:cpp_type:      boost::uuids::uuid
:index_comment: Index for looking up the left side
:list_by:       true
:END:

Left identifier.

* Right
:PROPERTIES:
:column:        right_id
:column_short:  right
:column_title:  Right
:type:          uuid
:cpp_type:      boost::uuids::uuid
:index_comment: Index for finding the right side
:END:

Right identifier.

* Columns

** assigned_at
:PROPERTIES:
:type:       timestamp with time zone
:cpp_type:   std::chrono::system_clock::time_point
:read_only:  true
:END:

When the association was made.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_probe_links_tbl
:END:

* C++

** Flags
:PROPERTIES:
:subcomponent: api
:END:
"""


def _render(tmp_path, template, output):
    model_path = tmp_path / "ores.testcomp.probe_link.org"
    model_path.write_text(JUNCTION, encoding="utf-8")
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


def test_a_junction_timestamp_column_is_a_string_in_the_sqlgen_struct(tmp_path):
    header = _render(
        tmp_path, "cpp_domain_type_entity.hpp.mustache", "probe_link_entity.hpp")

    assert "std::string assigned_at;" in header
    assert "std::chrono::system_clock::time_point assigned_at" not in header


def test_a_junction_timestamp_column_converts_in_both_mapper_directions(tmp_path):
    mapper = _render(
        tmp_path, "cpp_domain_type_mapper.cpp.mustache", "probe_link_mapper.cpp")

    assert "r.assigned_at = timestamp_to_timepoint(std::string_view{v.assigned_at});" in mapper
    assert "r.assigned_at = ores::platform::time::datetime::to_iso8601_utc(v.assigned_at);" in mapper
