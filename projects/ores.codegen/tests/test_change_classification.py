"""Tests for the pull-request change classifier.

The classifier decides whether the expensive half of the pull-request check
set runs, so the cases are the contract: a documentation change must not build
the tree, a header change must, and a path nobody has classified must ask for
everything rather than nothing.
"""

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.change_classification import classify  # noqa: E402


def test_a_documentation_change_runs_nothing_heavy():
    for path in ("doc/llm/memory/x.org", "prose.md", "README.md", ".gitignore"):
        decision = classify([path])
        assert not decision.anything, path


def test_a_workflow_change_runs_nothing_heavy():
    # The check set is configuration; the fast gates cover it and the pull
    # request that lands after it is what proves the change.
    decision = classify([".github/workflows/codegen-drift.yml"])
    assert not decision.anything


def test_a_web_change_runs_nothing_heavy():
    for path in (
        "projects/ores.web/src/app.ts",
        "projects/ores.web/packages/wire-protocol/src/generated/x.ts",
        "pnpm-lock.yaml",
    ):
        assert not classify([path]).anything, path


def test_a_header_change_builds_and_tests():
    decision = classify(["projects/ores.iam/core/include/ores.iam.core/x.hpp"])
    assert decision.cpp
    assert decision.db
    assert not decision.services


def test_a_source_change_builds_and_tests():
    decision = classify(["projects/ores.marketdata/service/src/app/loop.cpp"])
    assert decision.cpp
    assert decision.db
    # A .cpp under a service directory is the service's own code, so the
    # services are started as well.
    assert decision.services


def test_a_build_description_change_builds_and_tests():
    for path in (
        "CMakeLists.txt",
        "projects/ores.marketdata/core/src/CMakeLists.txt",
        "CMakePresets.json",
        "vcpkg.json",
        "cmake/x.cmake",
    ):
        decision = classify([path])
        assert decision.cpp, path
        assert decision.db, path


def test_a_model_change_builds_and_tests():
    decision = classify(["projects/ores.refdata/modeling/ores.refdata.book.org"])
    assert decision.cpp
    assert decision.db


def test_a_codegen_change_builds_and_tests():
    decision = classify(["projects/ores.codegen/library/templates/x.mustache"])
    assert decision.cpp
    assert decision.db


def test_a_sql_change_recreates_the_database_and_tests():
    decision = classify(["projects/ores.sql/create/marketdata/x_create.sql"])
    assert decision.db
    assert decision.cpp
    assert not decision.services


def test_a_service_change_starts_the_services():
    for path in (
        "projects/ores.marketdata/service/src/main.cpp",
        "projects/ores.service/src/launcher.cpp",
        "build/config/supervisor.yaml",
    ):
        decision = classify([path])
        assert decision.services, path
        assert decision.cpp, path
        assert decision.db, path


def test_a_path_nobody_classified_asks_for_everything():
    decision = classify(["projects/ores.brandnew/whatever.xyz"])
    assert decision.cpp
    assert decision.db
    assert decision.services
    assert "unclassified" in " ".join(decision.reasons)


def test_a_mixed_change_takes_the_union():
    decision = classify(
        [
            "doc/llm/memory/x.org",
            "projects/ores.marketdata/core/src/a.cpp",
            "projects/ores.iam/service/src/main.cpp",
        ]
    )
    assert decision.cpp
    assert decision.db
    assert decision.services


def test_the_github_outputs_are_the_three_flags():
    decision = classify(["projects/ores.marketdata/core/src/a.cpp"])
    assert decision.as_github_outputs() == {
        "cpp": "true",
        "db": "true",
        "services": "false",
    }


def test_no_paths_run_nothing_heavy():
    decision = classify([])
    assert not decision.anything
