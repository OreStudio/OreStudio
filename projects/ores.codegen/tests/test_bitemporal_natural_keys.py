"""Tests for the bitemporal trigger's natural-key guard.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_bitemporal_natural_keys.py

A soft update closes the current row it replaces before it inserts the new
version. The close names the row by the natural key, so a model that states
the trigger without a key would close every current row of the tenant. A
report's rows would then close the rows of every report before it, which is
never what a model means, so the loader refuses the model.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import org_document_to_model, parse_org  # noqa: E402

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000E2
:END:
#+title: ores.testcomp.tick_record
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: tick_record
#+entity_plural: tick_records
#+entity_title: Tick Record
#+coding_scheme: none
#+image_id: false

A point in a series that is never edited in place.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: false
:END:

* Columns

** id
:PROPERTIES:
:type:         uuid
:cpp_type:     boost::uuids::uuid
:primary_key:  true
:END:

The row identity.

** series_id
:PROPERTIES:
:type:         uuid
:cpp_type:     boost::uuids::uuid
:natural_key:  true
:END:

The series the row belongs to.

** as_of
:PROPERTIES:
:type:         timestamp with time zone
:cpp_type:     std::chrono::system_clock::time_point
:natural_key:  true
:END:

The instant the row states.

* SQL
** Flags
:PROPERTIES:
{trigger}:END:

{keys}"""

TRIGGER = ":bitemporal_trigger: soft_update_delete\n"

KEYS = """\
** Bitemporal natural keys

| column    | nullable |
|-----------+----------|
| series_id | false    |
| as_of     | false    |

"""


def _model(trigger=TRIGGER, keys=""):
    return MODEL.format(trigger=trigger, keys=keys)


def test_a_soft_update_trigger_without_a_key_is_refused():
    with pytest.raises(ValueError, match="Bitemporal natural keys"):
        org_document_to_model(parse_org(_model()))


def test_a_soft_update_trigger_with_a_key_states_the_key():
    de = org_document_to_model(parse_org(_model(keys=KEYS)))["domain_entity"]

    assert [k["column"] for k in de["sql"]["bitemporal_natural_keys"]] == [
        "series_id",
        "as_of",
    ]


def test_a_model_without_the_trigger_needs_no_key():
    de = org_document_to_model(parse_org(_model(trigger="")))["domain_entity"]

    assert "bitemporal_natural_keys" not in de.get("sql", {})
