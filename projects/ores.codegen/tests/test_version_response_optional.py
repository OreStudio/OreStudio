"""Regression test: a version read carries its row optionally, like a plain get.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_version_response_optional.py

``get_<entity>_version_response`` carried its row by value while
``get_<entity>_response`` carried it in a ``std::optional``. When the row did not
exist the service returned early with ``outcome = missing`` and left the member
default-constructed, and the serializer then wrote that default row on the wire.
For an entity with a date or a timestamp member the default is the zero value,
so the client refused the whole response -- ``Failed to parse field 'version':
Failed to parse field 'event_date': Invalid date value: 0000-00-00`` -- before it
could read the outcome that said the row was absent.

The two responses answer the same question, so they state the row the same way;
this pins the version one to the shape the plain get already had.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_DIR = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_DIR / "library" / "data"
TEMPLATES_DIR = CODEGEN_DIR / "library" / "templates"

# An entity with a date member, which is the shape that made the default row
# unparseable rather than merely useless.
BASE_ENTITY = REPO_ROOT / "projects/ores.refdata/modeling/ores.refdata.calendar_event.org"

DOMAIN_TYPE = "ores::refdata::domain::calendar_event"


def _render_header(tmp_path) -> str:
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    output_name = "calendar_event_protocol.hpp"
    generate_from_model(
        str(BASE_ENTITY),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_protocol.hpp.mustache",
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def _member(rendered: str, struct: str, member: str) -> str:
    """One member declaration, whitespace-flattened."""
    flat = re.sub(r"\s+", " ", rendered)
    start = flat.index(f"struct {struct} {{")
    end = flat.index("};", start)
    body = flat[start:end]
    match = re.search(rf"[^;]*\b{member};", body)
    assert match, f"{struct} declares no {member}"
    return match.group(0).strip().rstrip(";")


def test_the_version_response_carries_its_row_optionally(tmp_path):
    rendered = _render_header(tmp_path)
    assert _member(rendered, "get_calendar_event_version_response", "version") == (
        f"std::optional<{DOMAIN_TYPE}> version")


def test_the_plain_get_response_states_its_row_the_same_way(tmp_path):
    """The precedent the version response is pinned to."""
    rendered = _render_header(tmp_path)
    assert _member(rendered, "get_calendar_event_response", "calendar_event") == (
        f"std::optional<{DOMAIN_TYPE}> calendar_event")


def test_the_put_response_states_its_row_the_same_way(tmp_path):
    """A refused change writes no row, so the response must be able to say so.

    This one was not optional either, and the client refused the whole response
    for a date-bearing entity: ``calendar_events add`` answered
    ``Failed to parse field 'calendar_event': ... Invalid date value:
    0000-00-00`` instead of the refusal the result carried.
    """
    rendered = _render_header(tmp_path)
    assert _member(rendered, "put_calendar_event_response", "calendar_event") == (
        f"std::optional<{DOMAIN_TYPE}> calendar_event")
