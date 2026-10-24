"""Tests for N-way domain group composition.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_domain_groups.py

A grouped struct is its member list: the template emits one member per
group and nothing else. So every way a model can declare a field the
groups do not carry is a way to lose that field silently, and the loader
refuses each of them rather than generating a struct with a hole in it.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import load_org_model  # noqa: E402

GROUP = """\
#+title: ores.probe.thing_bits
#+type: ores.codegen.field_group
#+component: probe
#+entity_singular: thing_bits
#+brief: Probe field group.

* Fields

** id
:PROPERTIES:
:cpp_type: boost::uuids::uuid
:END:

Key.
"""

ENTITY = """\
#+title: ores.probe.thing
#+type: ores.codegen.entity
#+component: probe
#+entity_singular: thing
#+entity_plural: things
#+entity_title: Thing
#+brief: Probe entity.

* Flags
:PROPERTIES:
:schema:       public
:product:      ores
:component:    probe
:subcomponent: api
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

Key.
{extra_columns}
* C++

** Domain groups

| member | field_group      |
|--------+------------------|
| bits   | ores.probe.thing_bits |
{extra_rows}{extra_cpp}
"""


def _write(tmp_path, extra_columns="", extra_cpp="", extra_rows=""):
    (tmp_path / "ores.probe.thing_bits_field_group.org").write_text(
        GROUP, encoding="utf-8")
    p = tmp_path / "ores.probe.thing.org"
    p.write_text(
        ENTITY.format(extra_columns=extra_columns, extra_cpp=extra_cpp,
                      extra_rows=extra_rows),
        encoding="utf-8")
    return p


def test_a_column_in_no_group_is_refused(tmp_path):
    p = _write(tmp_path, extra_columns="""
** orphan
:PROPERTIES:
:type:     text
:cpp_type: std::string
:END:

Carried by no group.
""")
    with pytest.raises(ValueError, match="belong to no domain group"):
        load_org_model(p)


def test_a_domain_includes_block_is_refused(tmp_path):
    p = _write(tmp_path, extra_cpp="""
** Domain includes

#+begin_src cpp :name includes
#include <string>
#+end_src
""")
    with pytest.raises(ValueError, match="Domain includes block"):
        load_org_model(p)


def test_the_older_identity_slot_is_refused_alongside(tmp_path):
    """Domain groups supersedes the pair; declaring both says two things."""
    p = _write(tmp_path, extra_cpp="""
** Flags
:PROPERTIES:
:domain_identity_group: ores.probe.thing_bits
:END:
""")
    with pytest.raises(ValueError, match="domain_identity_group"):
        load_org_model(p)


def test_an_unknown_field_group_is_refused(tmp_path):
    p = _write(tmp_path)
    p.write_text(
        p.read_text(encoding="utf-8").replace(
            "ores.probe.thing_bits |", "ores.probe.absent |"),
        encoding="utf-8")
    with pytest.raises(ValueError, match="does not exist"):
        load_org_model(p)


def test_a_cross_component_group_is_found_in_its_own_component(tmp_path):
    """ores.dq.audit_record is not beside a refdata model, and does not
    need to be: its dotted name says which component's modeling tree
    holds it."""
    p = _write(tmp_path, extra_rows="| audit  | ores.dq.audit_record |\n")
    de = load_org_model(p)["domain_entity"]
    assert [f["name"] for f in de["domain_group_fields"]] == [
        "bits.id",
        "audit.modified_by",
        "audit.performed_by",
        "audit.change_reason_code",
        "audit.change_commentary",
        "audit.recorded_at",
    ]
