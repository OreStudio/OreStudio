"""Tests for a grouped date column's conversion in the write path.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_grouped_date_write.py

A grouped entity declares a column's database row type on the entity model
and the domain member on its field group. For a date column the two differ:
the row stores the ISO-8601 text and the member is a
``std::chrono::year_month_day`` (or its optional), and for an instant the
member is a ``std::chrono::system_clock::time_point``. The write record
carries the row's text spelling, so the generated service cannot assign it
to the typed member and must parse it -- the same shape the enum conversion
already has. These cases pin the flags and the rendered conversion.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"

GROUP = """\
#+title: ores.testcomp.probe_dates
#+type: ores.codegen.field_group
#+component: testcomp
#+entity_singular: probe_dates
#+namespace: ores::testcomp::domain
#+brief: Probe dates.

* Includes

#+begin_src cpp :name includes
#include <chrono>
#include <optional>
#+end_src

* Fields

** id
:PROPERTIES:
:cpp_type: boost::uuids::uuid
:END:

Key.

** trade_date
:PROPERTIES:
:cpp_type: std::optional<std::chrono::year_month_day>
:END:

The agreed date.

** execution_timestamp
:PROPERTIES:
:cpp_type: std::optional<std::chrono::system_clock::time_point>
:END:

The execution instant.
"""

ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000044
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

Key.

** trade_date
:PROPERTIES:
:type:     date
:cpp_type: std::string
:nullable: true
:END:

The row stores the date as text.

** execution_timestamp
:PROPERTIES:
:type:     timestamp with time zone
:cpp_type: std::string
:nullable: true
:END:

The row stores the instant as text.

* C++

** Domain groups

| member | field_group               |
|--------+---------------------------|
| dates  | ores.testcomp.probe_dates |
"""


def _write_model(tmp_path):
    (tmp_path / "ores.testcomp.probe_dates_field_group.org").write_text(
        GROUP, encoding="utf-8")
    model = tmp_path / "ores.testcomp.probe.org"
    model.write_text(ENTITY, encoding="utf-8")
    return model


def _render(tmp_path, template, suffix):
    model_path = _write_model(tmp_path)
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True, target_template=template,
        target_output=f"probe.{suffix}")
    return (output_dir / f"probe.{suffix}").read_text(encoding="utf-8")


def test_the_service_parses_a_grouped_date_and_instant(tmp_path):
    service = _render(tmp_path, "cpp_service.cpp.mustache", "cpp")
    assert ("v.dates.trade_date = "
            "ores::platform::time::datetime::from_iso8601_date(write.trade_date);") in service
    assert ("v.dates.execution_timestamp = "
            "ores::platform::time::datetime::from_iso8601_utc("
            "write.execution_timestamp);") in service
    assert "v.dates.trade_date = write.trade_date;" not in service
    assert "v.dates.execution_timestamp = write.execution_timestamp;" not in service


def test_the_protocol_still_carries_the_row_text(tmp_path):
    """The member is typed in the domain, and text on the wire."""
    protocol = _render(tmp_path, "cpp_protocol.hpp.mustache", "hpp")
    assert "std::string trade_date;" in protocol
    assert "std::string execution_timestamp;" in protocol
