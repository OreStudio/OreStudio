"""Tests for build/scripts/local_checks.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_local_checks.py

The runner is the pull-request gate now that GitHub runs no check on a pull
request, so its decision tree is the contract: a documentation change must not
reach the compiler, a header change must, and a path nobody classified must
ask for everything rather than nothing.
"""
import importlib.util
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "local_checks", REPO_ROOT / "build" / "scripts" / "local_checks.py")
local_checks = importlib.util.module_from_spec(SPEC)
# The runner uses dataclasses, and @dataclass looks its module up in
# sys.modules while it processes the class.
sys.modules[SPEC.name] = local_checks
SPEC.loader.exec_module(local_checks)


def select(paths, **kwargs):
    _, checks = local_checks.select_checks(paths, **kwargs)
    return {check.id for check in checks}


def heavy(paths, **kwargs):
    return {
        check.id
        for check in local_checks.select_checks(paths, **kwargs)[1]
        if check.phase == local_checks.HEAVY
    }


# -- the tree ---------------------------------------------------------------


def test_a_documentation_change_never_reaches_the_compiler():
    chosen = select(["doc/knowledge/architecture/x.org", "README.md"])
    assert not heavy(["doc/knowledge/architecture/x.org", "README.md"])
    assert "site-page" in chosen
    assert "lint" in chosen
    assert "build" not in chosen
    assert "ctest" not in chosen


def test_a_prose_only_change_builds_the_pages_and_nothing_else_heavy():
    chosen = select(["doc/recipes/github/x.org"])
    assert heavy(["doc/recipes/github/x.org"]) == set()
    assert "site-page" in chosen


def test_a_header_change_builds_and_tests():
    assert heavy(["projects/ores.iam/core/include/ores.iam.core/x.hpp"]) == {
        "build", "db", "ctest",
    }


def test_a_source_change_builds_and_tests():
    assert heavy(["projects/ores.marketdata/core/src/a.cpp"]) == {
        "build", "db", "ctest",
    }


def test_a_sql_change_rebuilds_the_database():
    assert heavy(["projects/ores.sql/create/x.sql"]) == {"build", "db", "ctest"}


def test_a_web_change_runs_the_web_checks_and_nothing_heavy():
    paths = ["projects/ores.web/src/app.ts"]
    assert heavy(paths) == set()
    assert "web" in select(paths)
    assert "format" not in select(paths)


def test_a_plugin_change_runs_only_that_plugin():
    environment = select(["projects/ores.dsh_environment/lib/client.js"])
    assert "dsh-environment" in environment
    assert "dsh-kanban" not in environment
    kanban = select(["projects/ores.dsh_kanban/lib/client.js"])
    assert "dsh-kanban" in kanban
    assert "dsh-environment" not in kanban


def test_a_model_change_runs_the_drift_checks():
    paths = ["projects/ores.refdata/modeling/ores.refdata.book.org"]
    chosen = select(paths)
    assert "component-drift" in chosen
    assert "model-drift" in chosen
    assert "paste-blocks" in chosen


def test_a_path_nobody_classified_asks_for_everything():
    paths = ["projects/ores.brandnew/whatever.xyz"]
    classes, checks = local_checks.select_checks(paths)
    assert "unknown" in classes
    assert len(checks) == len(local_checks.CATALOGUE)


def test_no_change_selects_nothing():
    assert select([]) == set()


def test_a_mixed_change_takes_the_union():
    classes, _ = local_checks.select_checks(
        ["doc/knowledge/x.org", "projects/ores.marketdata/core/src/a.cpp"]
    )
    assert {"docs", "cpp"}.issubset(classes)


def test_a_model_change_is_also_a_documentation_change():
    classes, _ = local_checks.select_checks(
        ["projects/ores.refdata/modeling/ores.refdata.book.org"]
    )
    assert {"docs", "modeling"}.issubset(classes)


def test_a_forced_class_overrides_the_classification():
    classes, checks = local_checks.select_checks(
        ["doc/knowledge/x.org"], classes={"cpp"}
    )
    assert classes == {"cpp"}
    assert "build" in {check.id for check in checks}


def test_a_check_with_paths_only_runs_when_one_of_them_changed():
    # The codegen suite belongs to the tooling class, but the skill checks do
    # not: a compass source change must not run them.
    assert "skill-catalogue" not in select(["projects/ores.compass/src/compass.py"])
    assert "skill-catalogue" in select(["doc/llm/skills/compass-pr-raise/SKILL.org"])


# -- the catalogue ----------------------------------------------------------


def test_every_check_id_is_unique():
    ids = [check.id for check in local_checks.CATALOGUE]
    assert len(ids) == len(set(ids))


def test_every_check_declares_at_least_one_class():
    for check in local_checks.CATALOGUE:
        assert check.classes, check.id


def test_every_declared_class_is_a_known_class():
    for check in local_checks.CATALOGUE:
        unknown = set(check.classes) - set(local_checks.CLASSES)
        assert not unknown, f"{check.id}: {unknown}"


def test_every_check_declares_a_known_phase():
    for check in local_checks.CATALOGUE:
        assert check.phase in local_checks.PHASE_NAMES, check.id


def test_every_check_can_be_reached_by_the_tree():
    # A check whose classes no path rule can produce is dead weight: it would
    # never run. 'unknown' is not a class a rule produces, so a check may not
    # rely on it alone.
    produced = {name for name, _ in local_checks.PATH_RULES}
    for check in local_checks.CATALOGUE:
        assert set(check.classes) & produced, check.id


def test_every_check_has_a_title_and_a_command():
    for check in local_checks.CATALOGUE:
        assert check.title.strip(), check.id
        assert check.argv, check.id


def test_the_heavy_checks_are_the_build_the_database_and_ctest():
    heavy_ids = {
        check.id for check in local_checks.CATALOGUE
        if check.phase == local_checks.HEAVY
    }
    assert heavy_ids == {"build", "db", "ctest"}


# -- the command line -------------------------------------------------------


def test_the_plan_does_not_run_anything(capsys):
    assert local_checks.main(["--plan", "--paths", "doc/knowledge/x.org"]) == 0
    out = capsys.readouterr().out
    assert "site-page" in out
    assert "compass.sh build" not in out
    assert "ctest" not in out


def test_an_unknown_check_id_is_an_error(capsys):
    assert local_checks.main(["--only", "no-such-check", "--paths", "x.cpp"]) == 2
    assert "unknown check id" in capsys.readouterr().err


@pytest.mark.parametrize("path", ["doc/knowledge/x.org", "projects/ores.web/src/app.ts"])
def test_a_plan_lists_the_reasons(path, capsys):
    assert local_checks.main(["--plan", "--paths", path]) == 0
    assert "Why these checks" in capsys.readouterr().out
