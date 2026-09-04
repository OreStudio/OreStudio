"""Regression tests for the auto-default qt.detail_fields shape in
generate_from_model() (name-keyed lookup entities, e.g. the compute app
entity whose key field IS name).

Before the fix, the auto-default emitted a hardcoded codeEdit key row
plus an unconditional nameEdit display row. For an entity whose key
field is name that bound two widgets (codeEdit and nameEdit) to one
column. The key row must be named after the field like every other
row; a code+name+description entity is unchanged.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_qt_detail_fields_default.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"


def _entity_body(key_field):
    """A small uuid-identified-lookup entity with the compute-app shape:
    a UUID primary key plus a natural key. key_field names the natural
    key; when it is 'name' there is no separate display-name column."""
    if key_field == 'name':
        columns = """\
** name
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:natural_key: true
:END:

The key field.
"""
    else:
        columns = """\
** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:natural_key: true
:END:

The key field.

** name
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:END:

The display name.
"""
    return f"""\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000{40 if key_field == 'name' else 41}
:END:
#+title: ores.testcomp.thing
#+description: Regression fixture for the auto-default detail_fields shape.
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: thing
#+entity_plural: things
#+entity_title: Thing

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:profile:   uuid-identified-lookup
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

UUID primary key.

{columns}** description
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: true
:END:

A description.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_things_tbl
:END:

* C++

** Flags
:PROPERTIES:
:subcomponent: api
:END:

** Repository
:PROPERTIES:
:entity_singular_short: thing
:entity_plural_short:   things
:entity_singular_words: test thing
:entity_plural_words:   test things
:END:

** Qt
:PROPERTIES:
:domain_include:       ores.testcomp.api/domain/thing.hpp
:domain_class:         testcomp::domain::thing
:protocol_include:     ores.testcomp.api/messaging/thing_protocol.hpp
:collection_name:      things
:key_field:            {key_field}
:has_uuid_primary_key: true
:END:
"""


def _generate_ui(tmp_path, key_field):
    model_path = tmp_path / "ores.testcomp.thing.org"
    model_path.write_text(_entity_body(key_field), encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="qt_detail_dialog_ui.mustache",
        target_output="ThingDetailDialog.ui",
    )
    return (output_dir / "ThingDetailDialog.ui").read_text(encoding="utf-8")


def test_name_keyed_entity_gets_single_key_widget(tmp_path):
    """An entity whose key field IS name (compute app) must render one
    key row named after the field -- no fabricated codeEdit bound to the
    name column alongside the nameEdit (the pre-fix duplicate binding)."""
    ui = _generate_ui(tmp_path, key_field='name')
    assert 'name="codeEdit"' not in ui
    assert ui.count('name="nameEdit"') == 1


def test_code_keyed_entity_keeps_code_plus_name_shape(tmp_path):
    """The default code+name+description lookup form is unchanged: a
    codeEdit key row plus a nameEdit display row."""
    ui = _generate_ui(tmp_path, key_field='code')
    assert ui.count('name="codeEdit"') == 1
    assert ui.count('name="nameEdit"') == 1
