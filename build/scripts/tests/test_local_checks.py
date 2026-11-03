"""Tests for build/scripts/local_checks.py.

Run::

    python3 -m pytest build/scripts/tests/test_local_checks.py

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
        "build", "db", "ctest", "step-consumers", "ticks-smoke",
    }


def test_a_source_change_builds_and_tests():
    assert heavy(["projects/ores.marketdata/core/src/a.cpp"]) == {
        "build", "db", "ctest", "step-consumers", "ticks-smoke",
    }


def test_a_sql_change_rebuilds_the_database_and_runs_the_sql_suites():
    assert heavy(["projects/ores.sql/create/x.sql"]) == {
        "build", "db", "ctest", "pgtap", "ticks-smoke",
    }


def test_a_web_change_runs_the_web_checks_and_nothing_heavy():
    paths = ["projects/ores.web/src/app.ts"]
    assert heavy(paths) == set()
    assert "web" in select(paths)
    assert "format" not in select(paths)


def test_a_plugin_change_runs_only_the_plugin_check():
    environment = select(["projects/ores.dsh_environment/lib/client.js"])
    assert "dsh-environment" in environment
    assert "web" not in environment


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


def test_an_untracked_scratch_file_does_not_ask_for_everything():
    untracked = ["build/tmp/chrome9-a/Default/Preferences", "projects/ores.x/src/new.cpp"]
    assert local_checks.claimed(untracked) == ["projects/ores.x/src/new.cpp"]


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


def test_the_heavy_checks_are_the_ones_that_need_a_built_tree():
    heavy_ids = {
        check.id for check in local_checks.CATALOGUE
        if check.phase == local_checks.HEAVY
    }
    # step-consumers reads a live server, which no ordinary run has, so it is
    # heavy: the phase where the infrastructure it needs is expected to be up.
    assert heavy_ids == {
        "build",
        "db",
        "ctest",
        "pgtap",
        "ore-xml-roundtrip",
        "step-consumers",
        "ticks-smoke",
    }


def test_only_a_sql_change_runs_the_sql_suites():
    assert "pgtap" not in heavy(["projects/ores.iam/core/src/a.cpp"])
    assert not heavy(["projects/ores.trading/modeling/ores.trading.trade.org"])
    assert "pgtap" in heavy(["projects/ores.sql/test/x_test.sql"])


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


# -- the order within a phase -----------------------------------------------


def phase_order(phase):
    known = {check.id for check in local_checks.CATALOGUE}
    group = [check for check in local_checks.CATALOGUE if check.phase == phase]
    return [check.id for check in local_checks.in_dependency_order(group, known)]


def test_the_tangle_follows_the_generation_it_reads():
    # tangle-shell copies the recipe's tangle block into the script library,
    # and component-drift is what writes the recipe. Tangle first and it
    # copies the previous revision, so the run leaves the scripts stale
    # against the recipes it is about to commit.
    order = phase_order(local_checks.PREPARE)
    assert order.index("component-drift") < order.index("tangle-shell")
    assert order.index("component-drift") < order.index("tangle-http")


def test_every_writer_that_derives_from_generated_output_follows_the_generator():
    order = phase_order(local_checks.PREPARE)
    generated = order.index("component-drift")
    for check in local_checks.CATALOGUE:
        if "component-drift" in check.after:
            assert order.index(check.id) > generated, check.id


def test_only_the_generator_moves_and_the_rest_keeps_its_catalogue_place():
    # The point of ordering rather than reordering: component-drift is pulled
    # forward to just before the first check that names it, and every other
    # check keeps the relative position the catalogue gives it.
    catalogue = [check.id for check in local_checks.CATALOGUE
                 if check.phase == local_checks.PREPARE]
    order = phase_order(local_checks.PREPARE)
    without = [cid for cid in order if cid != "component-drift"]
    assert without == [cid for cid in catalogue if cid != "component-drift"]
    dependents = [cid for cid in order
                  if cid in {c.id for c in local_checks.CATALOGUE
                             if "component-drift" in c.after}]
    # Pulled forward to immediately before the first check that names it, not
    # further and not to the front.
    assert order.index("component-drift") == min(
        order.index(cid) for cid in dependents) - 1


def test_a_dependency_that_is_not_a_check_is_an_error():
    bogus = local_checks.Check(
        id="x", title="x", argv=("true",), classes=("docs",),
        phase=local_checks.PREPARE, after=("no-such-check",))
    with pytest.raises(ValueError, match="which is not a check"):
        local_checks.in_dependency_order([bogus], {bogus.id})


def test_checks_that_depend_on_each_other_are_an_error():
    a = local_checks.Check(id="a", title="a", argv=("true",),
                           classes=("docs",), phase=local_checks.PREPARE,
                           after=("b",))
    b = local_checks.Check(id="b", title="b", argv=("true",),
                           classes=("docs",), phase=local_checks.PREPARE,
                           after=("a",))
    with pytest.raises(ValueError, match="cycle"):
        local_checks.in_dependency_order([a, b], {"a", "b"})


def test_a_dependency_that_is_not_running_this_time_is_not_an_error():
    # A docs-only change picks site-page but not component-drift. The
    # dependency names a check; whether it runs this time is a separate
    # question, and absence must not read as a typo.
    chosen = select(["doc/knowledge/architecture/x.org"])
    assert "site-page" in chosen
    assert "component-drift" not in chosen
    known = {check.id for check in local_checks.CATALOGUE}
    group = [check for check in local_checks.CATALOGUE
             if check.id in chosen and check.phase == local_checks.PREPARE]
    assert [check.id for check in local_checks.in_dependency_order(group, known)] == [
        check.id for check in group]


def test_the_suites_run_in_parallel():
    check = next(c for c in local_checks.CATALOGUE if c.id == "ctest")
    command = check.command("origin/main", "some-preset")[-1]
    assert command.endswith(f"test run --preset some-preset -- -j {local_checks.CTEST_JOBS}")
    assert local_checks.CTEST_JOBS > 1


def test_a_build_script_test_change_runs_no_codegen_check():
    chosen = select(["build/scripts/tests/test_local_checks.py"])
    assert "build-script-tests" in chosen
    assert not chosen & {"component-drift", "drift-sweep", "model-drift"}


# -- the fleet checks for a change that compiles only into tests ----------------

TEST_CPP = "projects/ores.trading/api/tests/blotter_protocol_codec_roundtrip_tests.cpp"
TEST_HPP = "projects/ores.analytics/core/tests/test_suite.hpp"
LIBRARY_HPP = "projects/ores.utility/include/ores.utility/codec/wire_fields.hpp"
REACH = {
    TEST_CPP: {"ores.trading.api.tests"},
    TEST_HPP: {"ores.analytics.core.tests"},
    LIBRARY_HPP: {"ores.utility.lib", "ores.trading.api.tests"},
}


def test_a_change_that_compiles_only_into_tests_skips_the_fleet():
    reason = local_checks.fleet_skip_reason([TEST_CPP, TEST_HPP], REACH)
    assert reason == ("the change compiles only into "
                      "ores.analytics.core.tests, ores.trading.api.tests")


def test_a_change_that_reaches_a_library_keeps_the_fleet():
    assert local_checks.fleet_skip_reason([TEST_CPP, LIBRARY_HPP], REACH) == ""


def test_a_file_the_build_does_not_know_keeps_the_fleet():
    added = "projects/ores.trading/api/tests/a_new_test.cpp"
    assert local_checks.fleet_skip_reason([TEST_CPP, added], REACH) == ""


def test_a_build_description_or_sql_change_keeps_the_fleet():
    for other in ("projects/ores.trading/api/tests/CMakeLists.txt",
                  "projects/ores.sql/create/trading/trade_create.sql"):
        assert local_checks.fleet_skip_reason([TEST_CPP, other], REACH) == ""


def test_an_unclaimed_path_keeps_the_fleet():
    assert local_checks.fleet_skip_reason([TEST_CPP, "some/unclaimed.path"], REACH) == ""


def test_a_documentation_path_beside_a_test_does_not_keep_the_fleet():
    assert local_checks.fleet_skip_reason([TEST_CPP, "doc/notes.org"], REACH) != ""


def test_the_target_comes_from_the_object_path():
    target_of = local_checks.target_of
    assert target_of("projects/ores.trading/api/tests/CMakeFiles/"
                     "ores.trading.api.tests.dir/Unity/unity_4_cxx.cxx.o") == "ores.trading.api.tests"
    assert target_of("publish/bin/ores.trading.api.tests") is None


def _run_heavy(monkeypatch, changed, build_status="PASS", skips=None):
    ran = []
    if skips is None:
        skips = {c: "the change compiles only into x.tests" for c in local_checks.FLEET_CHECKS}

    def fake_run_one(check, base, preset):
        ran.append(check.id)
        status = build_status if check.id == "build" else "PASS"
        return local_checks.Result(check=check, status=status)

    monkeypatch.setattr(local_checks, "run_one", fake_run_one)
    monkeypatch.setattr(local_checks, "fleet_skips", lambda changed, preset: skips)
    selected = [c for c in local_checks.CATALOGUE
                if c.id in ("build", "db", "ctest", "ticks-smoke", "step-consumers")]
    results = local_checks.run_all(selected, "origin/main", "p", 1, changed=changed)
    return ran, {r.check.id: r for r in results}


def test_the_heavy_phase_skips_the_fleet_after_the_build(monkeypatch):
    ran, results = _run_heavy(monkeypatch, [TEST_CPP])
    assert ran == ["build", "ctest"]
    for check_id in local_checks.FLEET_CHECKS:
        assert results[check_id].status == "SKIP"
        assert results[check_id].reason == "the change compiles only into x.tests"


def test_a_run_of_every_check_or_a_failed_build_skips_nothing(monkeypatch):
    ran, _ = _run_heavy(monkeypatch, None)
    assert ran == ["build", "db", "ctest", "ticks-smoke", "step-consumers"]
    ran, _ = _run_heavy(monkeypatch, [TEST_CPP], build_status="FAIL")
    assert "db" in ran


def test_the_report_names_why_a_check_was_skipped():
    db = local_checks.CHECKS_BY_ID["db"]
    build = local_checks.CHECKS_BY_ID["build"]
    results = [local_checks.Result(check=build, status="PASS"),
               local_checks.Result(check=db, reason="the change compiles only into x.tests")]
    report = local_checks.render({"cpp"}, {}, [TEST_CPP], "origin/main", "abc",
                                 results, "p")
    assert "1 run, 1 passed, 0 failed, 1 skipped" in report
    assert "- `db`: the change compiles only into x.tests." in report


def test_a_database_from_another_schema_is_rebuilt(monkeypatch):
    skips = {"ticks-smoke": "the change compiles only into x.tests",
             "step-consumers": "the change compiles only into x.tests"}
    ran, results = _run_heavy(monkeypatch, [TEST_CPP], skips=skips)
    assert ran == ["build", "db", "ctest"]
    assert results["ticks-smoke"].status == "SKIP"


def test_the_database_keeps_its_check_unless_its_schema_matches(monkeypatch):
    monkeypatch.setattr(local_checks, "read_reach",
                        lambda build_dir, wanted: {TEST_CPP: {"ores.trading.api.tests"}})
    monkeypatch.setattr(local_checks, "database_in_sync", lambda: False)
    assert set(local_checks.fleet_skips([TEST_CPP], "p")) == {"ticks-smoke", "step-consumers"}
    monkeypatch.setattr(local_checks, "database_in_sync", lambda: True)
    assert set(local_checks.fleet_skips([TEST_CPP], "p")) == set(local_checks.FLEET_CHECKS)
