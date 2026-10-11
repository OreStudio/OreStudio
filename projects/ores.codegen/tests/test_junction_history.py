"""Tests for the junction history surface.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_junction_history.py

A junction is stored with its history: each link has a version, an author, a
reason and a validity range, and a closing ends the range and deletes nothing.
A junction that states ``:history: true`` gets a repository read of every
version of every link of one parent, a field mapper and a history provider
registrar. These tests render the templates from a small model and pin what the
history depends on: the read is by the parent side and includes closed links,
the link is named by both keys, each link is numbered on its own, and a model
that does not ask for a history gets none.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.generate import resolve_targets  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

JUNCTION = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000e1
:END:
#+title: ores.testcomp.test_link
#+type: ores.codegen.junction
#+component: testcomp
#+name: test_links
#+name_singular: test_link
#+name_title: Test Link
#+name_singular_words: test link
#+brief: A test link.
#+product: ores
#+schema: public
#+has_tenant_id: true

A test link.

* Flags
:PROPERTIES:
:profile: tenant-scoped-junction
{flags}:END:

* Left
:PROPERTIES:
:column:        owner_id
:column_short:  owner
:column_title:  Owner
:type:          uuid
:cpp_type:      boost::uuids::uuid
:list_by:       true
:END:

The owner.

* Right
:PROPERTIES:
:column:        item_code
:column_short:  item
:column_title:  Item
:type:          text
:cpp_type:      std::string
:END:

The item.
{columns}
"""

WEIGHT = """
* Columns

** weight
:PROPERTIES:
:type:     integer
:cpp_type: int
:nullable: false
:END:

How much.
"""

ODD_COLUMN = """
* Columns

** blob
:PROPERTIES:
:type:     text
:cpp_type: std::vector<char>
:nullable: false
:END:

Something a history cannot render.
"""


def render(tmp_path, template, flags="", columns=""):
    model_path = tmp_path / "ores.testcomp.test_link_junction.org"
    model_path.write_text(JUNCTION.format(flags=flags, columns=columns), encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template=template,
        target_output="out.txt",
    )
    return (output_dir / "out.txt").read_text(encoding="utf-8")


HISTORY = ":history: true\n"


def test_the_repository_reads_every_version_of_every_link_of_the_parent(tmp_path):
    header = render(tmp_path, "cpp_domain_type_repository.hpp.mustache", HISTORY)
    source = render(tmp_path, "cpp_domain_type_repository.cpp.mustache", HISTORY)

    assert "struct test_link_history_row : domain::test_link" in header
    assert "std::optional<std::chrono::system_clock::time_point> closed_at;" in header
    assert "read_all_by_owner(const boost::uuids::uuid& owner_id)" in header
    # Grouped by link, newest first within a link, and with no filter on the
    # end of validity: a closed link is a row whose end is in the past.
    assert 'order_by("item_code"_c, "valid_from"_c.desc())' in source
    assert '"owner_id"_c == owner_id_str' in source
    start = source.index("read_all_by_owner(")
    assert '"valid_to"_c == max.value()' not in source[start : start + 900]


def test_a_version_a_later_version_replaced_is_not_a_closing(tmp_path):
    source = render(tmp_path, "cpp_domain_type_repository.cpp.mustache", HISTORY)

    # The row before this one in the group is the next newer version. It
    # replaced this one only if it carries the next version number, so a link
    # closed and opened again, which numbers from one, ends the first run.
    assert "rows.back().item_code == row.item_code" in source
    assert "rows.back().version == row.version + 1" in source
    assert "row.closed_at = timestamp_to_timepoint(entity.valid_to);" in source


def test_the_link_is_named_by_both_keys_and_says_when_it_ended(tmp_path):
    source = render(tmp_path, "cpp_history_field_mapper.cpp.mustache", HISTORY, WEIGHT)

    assert (
        'boost::uuids::to_string(v.owner_id) + "/" + v.item_code' in source
    )
    assert '{.name = "Owner"' in source and '{.name = "Item"' in source
    assert '{.name = "Weight", .value = std::to_string(v.weight)}' in source
    assert '"Closed At"' in source
    assert "provenance_fields::recorded_at" in source


def test_the_provider_reads_by_the_parent_and_numbers_each_link_on_its_own(tmp_path):
    source = render(tmp_path, "cpp_history_provider_registrar.cpp.mustache", HISTORY)

    assert '"ores.testcomp.test_link"' in source
    assert '"testcomp::test_links:read"' in source
    assert "boost::lexical_cast<boost::uuids::uuid>(entity_id)" in source
    assert "repo.read_all_by_owner(" in source
    # One call per link, so a version number is a link's and not the parent's.
    assert "row.item_code != first->item_code" in source
    assert "build_entity_history_versions(" in source


def test_a_model_can_name_the_read_code_its_history_needs(tmp_path):
    source = render(
        tmp_path,
        "cpp_history_provider_registrar.cpp.mustache",
        HISTORY + ":history_read_permission: testcomp::owners:read\n",
    )

    assert '"testcomp::owners:read"' in source
    assert '"testcomp::test_links:read"' not in source


def test_a_junction_that_does_not_ask_for_a_history_gets_none(tmp_path):
    header = render(tmp_path, "cpp_domain_type_repository.hpp.mustache")

    assert "history_row" not in header
    assert "read_all_by_" not in header


def test_a_column_a_history_cannot_render_is_refused_and_not_left_out(tmp_path):
    with pytest.raises(ValueError, match="cannot render"):
        render(tmp_path, "cpp_history_field_mapper.cpp.mustache", HISTORY, ODD_COLUMN)


def test_the_history_facets_resolve_only_for_a_junction_that_asks(tmp_path):
    asks = tmp_path / "asks" / "modeling"
    plain = tmp_path / "plain" / "modeling"
    asks.mkdir(parents=True)
    plain.mkdir(parents=True)
    (asks / "ores.testcomp.test_link_junction.org").write_text(
        JUNCTION.format(flags=HISTORY, columns=""), encoding="utf-8")
    (plain / "ores.testcomp.test_link_junction.org").write_text(
        JUNCTION.format(flags="", columns=""), encoding="utf-8")

    history_templates = {
        "cpp_history_provider_registrar.cpp.mustache",
        "cpp_history_provider_registrar.hpp.mustache",
        "cpp_history_field_mapper.cpp.mustache",
        "cpp_history_field_mapper.hpp.mustache",
    }
    asked, _, _ = resolve_targets(asks / "ores.testcomp.test_link_junction.org", CODEGEN_BASE)
    not_asked, _, _ = resolve_targets(plain / "ores.testcomp.test_link_junction.org", CODEGEN_BASE)

    assert history_templates <= {u["template"] for u in asked}
    assert not (history_templates & {u["template"] for u in not_asked})
