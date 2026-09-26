"""Tests for the read-only current-state profile.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_readonly_current_state_profile.py

A model that binds this profile must derive a read-only wire surface over a
current-state table, without restating either flag. The profile carries
``read_only`` to the entity root, where the write surface is decided, and
``current_state`` to the SQL namespace, where the table shape is decided.
Both were authored per entity before the profile existed, in two different
drawers, so a binding that reaches only one of them is the failure this file
exists to catch.
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

PROFILE = "read-only-current-state"

# An entity that declares neither flag. Everything read-only about it comes
# from the bound profile.
MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000C3
:END:
#+title: ores.testcomp.bootstrap_record
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: bootstrap_record
#+entity_plural: bootstrap_records
#+entity_title: Bootstrap Record
#+coding_scheme: none
#+image_id: false

A row the system writes when it provisions the database and everyone reads.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: false
:profile:       {profile}
:END:

* Columns

** id
:PROPERTIES:
:type:         uuid
:cpp_type:     boost::uuids::uuid
:primary_key:  true
:END:

The row identity.

** build_environment
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

The environment the database was built for.
""".format(profile=PROFILE)

# The same model with the profile removed, as the control: nothing about it
# is read-only, so it must keep the write surface.
WRITABLE_MODEL = MODEL.replace(":profile:       " + PROFILE + "\n", "")


def _render(tmp_path, template, output_name, body):
    model_path = tmp_path / "ores.testcomp.bootstrap_record.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / output_name
    output_dir.mkdir()
    generate_from_model(
        str(model_path), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True,
        target_template=template, target_output=output_name)
    return (output_dir / output_name).read_text(encoding="utf-8")


def _protocol(tmp_path, body):
    return _render(tmp_path, "cpp_protocol.hpp.mustache",
                   "bootstrap_record_protocol.hpp", body)


def _table_sql(tmp_path, body):
    return _render(tmp_path, "sql_schema_domain_entity_create.mustache",
                   "bootstrap_records_create.sql", body)


def test_the_profile_derives_the_reads_and_no_writes(tmp_path):
    protocol = _protocol(tmp_path, MODEL)

    assert "struct list_bootstrap_records_request" in protocol
    assert "struct get_bootstrap_record_request" in protocol
    assert "struct get_many_bootstrap_records_request" in protocol
    assert "put_" not in protocol
    assert "delete_" not in protocol


def test_the_profile_derives_no_version_surface(tmp_path):
    protocol = _protocol(tmp_path, MODEL)

    assert "_versions_" not in protocol
    assert "_version_" not in protocol


def test_the_profile_derives_the_current_state_table(tmp_path):
    sql = _table_sql(tmp_path, MODEL)

    assert '"build_environment" text not null' in sql
    assert "valid_from" not in sql
    assert "valid_to" not in sql


def test_a_model_without_the_profile_keeps_the_write_surface(tmp_path):
    protocol = _protocol(tmp_path, WRITABLE_MODEL)

    assert "put_" in protocol
    assert "delete_" in protocol


def test_the_profile_is_catalogued():
    catalogue = (REPO_ROOT / "projects/modeling/variability_profiles.org").read_text(
        encoding="utf-8")

    assert "Read-only current-state" in catalogue


@pytest.mark.parametrize("flag", ["read_only", "current_state"])
def test_both_flags_are_profile_bindable(flag):
    """A feature the namespace map omits is skipped in silence, so the flags
    this profile carries must be in the map."""
    from codegen.org_loader import _FEATURE_NAMESPACE

    assert flag in _FEATURE_NAMESPACE


# A junction, because it derives its write surface on a different path from a
# domain entity and reads it from a key computed during load rather than from
# the flag itself. The profile supplies read_only; the model states it nowhere.
JUNCTION_MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000c4
:END:
#+title: ores.testcomp.widget_owner_junction
#+type: ores.codegen.junction
#+component: testcomp
#+name: widget_owners
#+name_singular: widget_owner
#+name_title: Widget Owner
#+name_singular_words: widget owner association
#+brief: Links a widget to its owner.
#+product: ores
#+schema: public
#+has_tenant_id: true

* Flags
:PROPERTIES:
:profile: {profile}
:END:

* Left
:PROPERTIES:
:column:        widget_id
:column_short:  widget
:column_title:  Widget
:type:          uuid
:cpp_type:      boost::uuids::uuid
:list_by:       true
:END:

* Right
:PROPERTIES:
:column:        owner_id
:column_short:  owner
:column_title:  Owner
:type:          uuid
:cpp_type:      boost::uuids::uuid
:END:

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_widget_owners_tbl
:END:
""".format(profile=PROFILE)


def test_a_profile_bound_junction_derives_its_write_switch_after_the_profile(
        tmp_path):
    """The junction's ``wire_write_enabled`` folds ``read_only`` in, so it has
    to be derived after the profile supplies the flag. Derived earlier, a
    profile-bound junction advertises write messages for repository methods
    the repository template has already dropped."""
    from codegen.org_loader import load_org_junction_model

    model_path = tmp_path / "ores.testcomp.widget_owner_junction.org"
    model_path.write_text(JUNCTION_MODEL, encoding="utf-8")

    loaded = load_org_junction_model(model_path)["junction"]

    assert loaded["read_only"] is True
    assert loaded["wire_write_enabled"] is False

