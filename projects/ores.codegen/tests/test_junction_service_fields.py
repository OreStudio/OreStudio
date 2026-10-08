"""Regression test: a junction service writes the response field its protocol declares.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_junction_service_fields.py

A junction repository carries an abbreviated plural (``name_short``) that the
service template used to write as the response collection, while the protocol
names the same field after the resource. A junction with no presentation
drawer gets no name aliasing, so the protocol fell back to the resource plural
and the two disagreed: ``currency_currency_group_service.cpp`` wrote
``response.currency_groups`` where the header declares
``currency_currency_groups``, and the component stopped compiling.

The base model is a live junction, so it stays structurally valid; only the
names are rewritten, which forces the short form to differ from the resource
plural and keeps the test about the disagreement rather than about whichever
names an org happens to carry today.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

# A live junction, used as a valid base rather than copied and frozen.
BASE_JUNCTION = (
    REPO_ROOT
    / "projects/ores.refdata/modeling/ores.refdata.currency_currency_group_junction.org"
)

PLURAL = "widget_thing_links"
SINGULAR = "widget_thing_link"
SHORT = "links"


def _renamed_junction(tmp_path):
    """The base junction with a plural and an abbreviated plural that differ."""
    body = BASE_JUNCTION.read_text(encoding="utf-8")
    body = body.replace("#+name: currency_currency_groups", f"#+name: {PLURAL}")
    body = body.replace("#+name_singular: currency_currency_group",
                        f"#+name_singular: {SINGULAR}")
    body = body.replace(":name_singular_short: currency_group",
                        f":name_singular_short: {SINGULAR}")
    body = body.replace(":name_short:          currency_groups",
                        f":name_short:          {SHORT}")
    assert SHORT in body and PLURAL in body
    model = tmp_path / f"ores.testjunc.{PLURAL}.org"
    model.write_text(body, encoding="utf-8")
    return model


def _render_service(tmp_path):
    model = _renamed_junction(tmp_path)
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    output_name = "widget_thing_link_service.cpp"
    generate_from_model(
        str(model),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_service.cpp.mustache",
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def test_service_uses_the_resource_plural_not_the_abbreviated_one(tmp_path):
    rendered = _render_service(tmp_path)
    assert f"response.{PLURAL} " in rendered
    assert f"response.{SHORT} " not in rendered


def test_the_collection_is_written_and_read_on_the_same_field(tmp_path):
    """Every response field the service writes must be one the header declares."""
    rendered = _render_service(tmp_path)
    written = {
        line.split("response.", 1)[1].split(" =", 1)[0].split(".", 1)[0]
        for line in rendered.splitlines()
        if "response." in line and "response.result" not in line
    }
    assert PLURAL in written
    assert SHORT not in written


# A junction that stamps a target party takes the change intent as a parameter.
# A branch that stamps and then ignores the intent both loses the caller's
# reason and leaves the parameter unused, which -Werror refuses.
PARTY_JUNCTION = (
    REPO_ROOT
    / "projects/ores.refdata/modeling/ores.refdata.party_counterparty_junction.org"
)


def _render_party_service(tmp_path):
    model = tmp_path / PARTY_JUNCTION.name
    model.write_text(PARTY_JUNCTION.read_text(encoding="utf-8"), encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    output_name = "party_counterparty_service.cpp"
    generate_from_model(
        str(model),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_service.cpp.mustache",
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def test_a_target_party_stamp_keeps_the_caller_reason(tmp_path):
    rendered = _render_party_service(tmp_path)
    assert "out.change_reason_code = intent.reason_code;" in rendered


# A scoped read whose relation the wire may leave unstated has no scope when
# it does. The template used to pass the optional straight to a repository
# method that takes a plain key, which does not convert.
SCOPED_ENTITY = (
    REPO_ROOT / "projects/ores.refdata/modeling/ores.refdata.tenor_schedule.org"
)


def _render_scoped_service(tmp_path):
    model = tmp_path / SCOPED_ENTITY.name
    model.write_text(SCOPED_ENTITY.read_text(encoding="utf-8"), encoding="utf-8")
    output_dir = tmp_path / "scoped"
    output_dir.mkdir()
    output_name = "tenor_schedule_service.cpp"
    generate_from_model(
        str(model),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_service.cpp.mustache",
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def test_an_unstated_scoped_relation_is_refused(tmp_path):
    rendered = _render_scoped_service(tmp_path)
    assert "refuse(outcome_code::relation_required" in rendered
    assert '{.entity = "tenor schedules", .field = "calendar_code"}' in rendered
    assert "const auto relation = *request.calendar_code;" in rendered


# An entity whose parent has two mandatory FKs onto one table seeds that
# ancestor twice, once per leg. Naming the ancestor after the entity alone
# declared the same variable twice, which does not compile: currency_pair's
# base and quote legs both reach currency. The first leg keeps the entity name
# and the colliding leg falls back to its FK column, so the pair is seeded with
# a currency of its own on each side.
#
# The committed artefact is asserted rather than a single-model render: the
# parent chain is resolved by scanning every component's models, which a model
# rendered on its own from a temporary directory does not reach.
TWO_LEG_GENERATED = (
    REPO_ROOT
    / "projects/ores.refdata/core/tests"
    / "currency_pair_convention_eventing_integration_tests.cpp"
)


def test_a_parent_reached_twice_seeds_one_ancestor_per_leg():
    import re

    rendered = TWO_LEG_GENERATED.read_text(encoding="utf-8")
    declared = re.findall(
        r"auto (\w+) =\s+ores::\w+::\w+::generate_synthetic_", rendered)
    assert declared, "the test seeds no parent"
    assert len(declared) == len(set(declared)), f"a seed is declared twice: {declared}"
    assert "pair_code_parent_currency_parent" in declared
    assert "pair_code_parent_quote_currency_parent" in declared




