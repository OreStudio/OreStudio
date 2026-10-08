#!/usr/bin/env python3
"""Run every check a change needs, locally, in one pass.

GitHub no longer checks a pull request: the pull_request triggers were
retired, so this module is the gate. It classifies the changed paths against a
decision tree, runs the checks the matching classes need, and writes one
report that names every failure, the command that reproduces it, and the
command that fixes it.

The decision tree is data: a table of change classes and path patterns in
this file, and a catalogue of checks below it. Adding a check means adding a
row; the report, the class filter and the decision tree all follow from it.

Two rules matter:

- A path no rule claims asks for everything. A change this table cannot
  classify is a change it cannot vouch for.
- A check runs only when its class matched *and* one of its own path patterns
  matched, when it declares patterns. That is what keeps a documentation
  change off the C++ build.
"""

from __future__ import annotations

import argparse
import fnmatch
import json
import os
import shutil
import subprocess
import sys
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, field
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

# The codegen scripts need pystache, faker and pytest, which CI installs from
# projects/ores.codegen/requirements.txt. The checkout's own venv already has
# them. The build/scripts checks are stdlib-only and run on any python3.
SYSTEM_PY = "python3"
CODEGEN_PY = "projects/ores.codegen/venv/bin/python"
CODEGEN_TESTS_PY = "projects/ores.codegen/venv/bin/python"

OUTPUT_DIR = REPO_ROOT / "build" / "output" / "local_checks"
LOG_DIR = OUTPUT_DIR / "logs"

# Phases run in order; checks inside a phase run at the same time.
PREPARE = 0
CHECKS = 1
HEAVY = 2
PHASE_NAMES = {PREPARE: "Prepare", CHECKS: "Checks", HEAVY: "Build and test"}

TAIL_LINES = 40


@dataclass(frozen=True)
class Check:
    """One thing the runner can prove about the tree."""

    id: str
    title: str
    argv: tuple[str, ...]
    classes: tuple[str, ...]
    fix: str = ""
    paths: tuple[str, ...] = ()
    phase: int = CHECKS
    cwd: str = "."
    timeout: int = 900
    # Checks this one must follow, by id. The catalogue order is the default;
    # this states the dependencies that order must not be allowed to break.
    after: tuple[str, ...] = ()

    def command(self, base: str, preset: str) -> list[str]:
        return [a.replace("{base}", base).replace("{preset}", preset) for a in self.argv]

    def applies(self, classes: set[str], changed: list[str]) -> bool:
        if not self.classes or not classes.intersection(self.classes):
            return False
        if not self.paths:
            return True
        return any(
            fnmatch.fnmatch(path, pattern)
            for path in changed
            for pattern in self.paths
        )


def in_dependency_order(group: list[Check], known: set[str]) -> list[Check]:
    """One phase's checks, ordered so a deriving writer follows its source.

    The catalogue order is the default, and a check that names nothing keeps
    its place. A check that reads what another writes states that with
    `after`, because two writers over the same artefacts must not be ordered
    by where they happen to sit in the file. `tangle-shell` tangles the
    recipes `component-drift` writes: run first, it tangles the previous
    revision, so the run re-tangles a tree the same run is about to change and
    the scripts are stale again the moment the recipes are committed.

    A name that is no check at all is a typo and raises. A name in an earlier
    phase is already satisfied by the phase order, so it is ignored here. A
    cycle raises rather than looping.
    """
    ids = {check.id for check in group}
    for check in group:
        for dependency in check.after:
            if dependency not in known:
                raise ValueError(
                    f"check {check.id!r} runs after {dependency!r}, "
                    "which is not a check")
    ordered: list[Check] = []
    placed: set[str] = set()
    remaining = list(group)
    while remaining:
        # One at a time, first in catalogue order, so a check that names no
        # dependency never moves and only the deriving writer is deferred.
        for check in remaining:
            if all(d not in ids or d in placed for d in check.after):
                ordered.append(check)
                placed.add(check.id)
                remaining.remove(check)
                break
        else:
            raise ValueError(
                "checks depend on each other in a cycle: "
                + ", ".join(c.id for c in remaining))
    return ordered


# --------------------------------------------------------------------------
# The decision tree: which change class a path belongs to.
# --------------------------------------------------------------------------

CLASSES = (
    "docs",
    "modeling",
    "codegen",
    "sql",
    "cpp",
    "services",
    "web",
    "plugins",
    "tooling",
    "ci",
)

CLASS_NOTES = {
    "docs": "documentation, the site corpus and the agile record",
    "modeling": "a component model, which is what codegen reads",
    "codegen": "the generator, its templates and its tests",
    "sql": "the schema, its populate scripts and the migrations",
    "cpp": "compiled sources, headers and the build description",
    "services": "a service, its launcher and what starts it",
    "web": "the ores.web client and its packages",
    "plugins": "the DSH profile plugins",
    "tooling": "the project's own tools and their checks",
    "ci": "the check configuration itself",
}

PATH_RULES: tuple[tuple[str, tuple[str, ...]], ...] = (
    ("docs", ("doc/*", "assets/*", ".claude/*", "*.md", "*.org", "LICENSE*", ".gitignore")),
    ("modeling", ("projects/*/modeling/*", "projects/ores.shell/scripts/library/*")),
    ("codegen", ("projects/ores.codegen/*", "projects/ores.http/*")),
    ("sql", ("projects/ores.sql/*", "projects/ores.seeder/*", "*.sql")),
    (
        "cpp",
        (
            "*.cpp",
            "*.hpp",
            "*.h",
            "*.ipp",
            "*.c",
            "*.cc",
            "*.cxx",
            "*.cmake",
            "CMakeLists.txt",
            "CMakePresets.json",
            "vcpkg.json",
            "vcpkg-configuration.json",
        ),
    ),
    (
        "services",
        (
            "projects/*/service/*",
            "projects/ores.service/*",
            "build/config/*",
            "docker/*",
            "compose*.yml",
        ),
    ),
    (
        "web",
        (
            "projects/ores.web/*",
            "*.ts",
            "*.tsx",
            "package.json",
            "pnpm-lock.yaml",
            ".prettierrc.yaml",
        ),
    ),
    ("plugins", ("projects/ores.dsh_environment/*", "projects/ores.dsh_kanban/*")),
    ("tooling", ("projects/ores.compass/*", "build/scripts/*")),
    ("ci", (".github/*", "projects/ores.codegen/library/templates/*")),
)


def classify(changed: list[str]) -> tuple[set[str], dict[str, list[str]]]:
    """The classes the changed paths belong to, and the reason for each.

    A path matching no rule yields no class; when the whole change yields no
    class the caller runs every check, because an unknown path is one this
    table cannot vouch for.
    """
    classes: set[str] = set()
    reasons: dict[str, list[str]] = {}
    for path in changed:
        matched = False
        for name, patterns in PATH_RULES:
            if any(fnmatch.fnmatch(path, pattern) for pattern in patterns):
                matched = True
                classes.add(name)
                reasons.setdefault(name, []).append(path)
        if not matched:
            classes.add("unknown")
            reasons.setdefault("unknown", []).append(path)
    return classes, reasons


# --------------------------------------------------------------------------
# The catalogue. Every check CI used to run on a pull request, plus the four
# checks that had no CI job at all.
# --------------------------------------------------------------------------

SKILL_SCRIPTS = ("build/scripts/generate_skill*.py", "build/scripts/link_structure_notes.py")

CATALOGUE: tuple[Check, ...] = (
    # -- prepare: the writers run before anything reads what they write ------
    Check(
        id="tangle-templates",
        title="The templates tangle to the committed files",
        argv=(
            "bash",
            "-c",
            "./compass.sh build --direct codegen_templates"
            " && git diff --exit-code -- projects/ores.codegen/library/templates"
            " && test -z \"$(git ls-files --others --exclude-standard"
            " -- projects/ores.codegen/library/templates)\"",
        ),
        classes=("codegen", "ci"),
        paths=("projects/ores.codegen/library/templates/*",
               "projects/ores.lisp/src/ores-build-codegen-templates.el"),
        phase=PREPARE,
        fix="Commit the regenerated projects/ores.codegen/library/templates files.",
    ),
    Check(
        id="tangle-shell",
        title="The shell recipes tangle to the committed scripts",
        argv=(
            "bash",
            "-c",
            "./compass.sh build --direct tangle_shell_scripts"
            " && git diff --exit-code -- projects/ores.shell/scripts/library"
            " && test -z \"$(git ls-files --others --exclude-standard"
            " -- projects/ores.shell/scripts/library)\"",
        ),
        classes=("modeling", "codegen"),
        paths=("doc/recipes/shell/*",
               "projects/ores.lisp/src/ores-build-recipe-scripts.el",
               "projects/ores.shell/scripts/library/*"),
        after=("component-drift",),
        phase=PREPARE,
        fix="Commit the regenerated scripts under projects/ores.shell/scripts/library.",
    ),
    Check(
        id="tangle-http",
        title="The HTTP recipes tangle to the committed files",
        argv=(
            "bash",
            "-c",
            "./compass.sh build --direct tangle_http_recipes"
            " && git diff --exit-code -- doc/recipes/http projects/ores.http/scripts/library"
            " && test -z \"$(git ls-files --others --exclude-standard"
            " -- doc/recipes/http projects/ores.http/scripts/library)\"",
        ),
        classes=("modeling", "codegen"),
        paths=("doc/recipes/http/*",
               "projects/ores.lisp/src/ores-build-http-recipes.el"),
        after=("component-drift",),
        phase=PREPARE,
        fix="Commit the regenerated files under doc/recipes/http and"
            " projects/ores.http/scripts/library.",
    ),
    Check(
        id="site-page",
        title="The changed pages build and their links resolve",
        argv=("./compass.sh", "site", "page"),
        classes=("docs",),
        after=("component-drift",),
        phase=PREPARE,
        fix="Fix the page the report names, then run compass site page again.",
    ),
    # -- checks: everything that reads the tree -----------------------------
    Check(
        id="lint",
        title="Filetags, generator markers and id links are valid",
        argv=("./compass.sh", "lint"),
        classes=("docs", "modeling", "codegen", "sql", "cpp", "services", "web", "plugins", "tooling", "ci"),
    ),
    Check(
        id="no-pr-triggers",
        title="No workflow triggers on a pull request",
        argv=("python3", "build/scripts/check_no_pr_triggers.py"),
        classes=("ci",),
        fix="Drop the pull_request trigger; keep push to main and dispatch.",
    ),
    Check(
        id="org-links",
        title="Every org link resolves",
        argv=("emacs", "-Q", "--script", "projects/ores.lisp/src/ores-check-org-links.el"),
        classes=("docs",),
    ),
    Check(
        id="skill-namespace",
        title="Every skill uses the compass namespace",
        argv=("python3", "build/scripts/rename_skills_to_compass_namespace.py", "--check"),
        classes=("docs", "tooling"),
        paths=SKILL_SCRIPTS + ("doc/llm/skills/*",),
    ),
    Check(
        id="skill-catalogue",
        title="The skills catalogue matches the skill tree",
        argv=("python3", "build/scripts/generate_skills_catalogue.py", "--check"),
        classes=("docs", "tooling"),
        paths=SKILL_SCRIPTS + ("doc/llm/skills/*",),
    ),
    Check(
        id="skill-type-tables",
        title="The skill type tables match the catalogue",
        argv=("python3", "build/scripts/generate_skill_type_tables.py", "--check"),
        classes=("docs", "tooling"),
        paths=SKILL_SCRIPTS + ("doc/llm/skills/*",),
    ),
    Check(
        id="skill-levels",
        title="Every skill declares its level",
        argv=("python3", "build/scripts/apply_skill_levels.py", "--check"),
        classes=("docs", "tooling"),
        paths=SKILL_SCRIPTS + ("doc/llm/skills/*",),
    ),
    Check(
        id="skill-recipes",
        title="Every skill links its recipes both ways",
        argv=("python3", "build/scripts/link_skill_recipes.py", "--check"),
        classes=("docs", "tooling"),
        paths=SKILL_SCRIPTS + ("doc/llm/skills/*",),
    ),
    Check(
        id="structure-notes",
        title="Every structure note is linked from and to its neighbours",
        argv=("python3", "build/scripts/link_structure_notes.py", "--check"),
        classes=("docs",),
    ),
    Check(
        id="pattern-uses",
        title="Pattern links run both ways",
        argv=("python3", "build/scripts/check_pattern_uses.py"),
        classes=("docs", "tooling"),
        paths=("doc/*", "build/scripts/check_pattern_uses.py"),
    ),
    Check(
        id="prototypes",
        title="The prototype inventory matches the tree",
        argv=("python3", "build/scripts/check_prototype_inventory.py"),
        classes=("docs", "tooling"),
        paths=("doc/*", "build/scripts/check_prototype_inventory.py"),
    ),
    Check(
        id="component-docs",
        title="Every component has an overview and a diagram",
        argv=("bash", "projects/ores.codegen/validate_docs.sh"),
        classes=("modeling", "codegen"),
        after=("component-drift",),
        phase=PREPARE,
    ),
    Check(
        id="cmake-sources",
        title="Every component_files.cmake list matches the tree",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/regenerate_cmake_component_files.py", "--all", "--check"),
        classes=("codegen", "modeling", "cpp", "sql", "services"),
        after=("component-drift",),
        phase=PREPARE,
        fix="Run the same command with --all to regenerate, then commit.",
    ),
    Check(
        id="component-drift",
        title="Every artefact matches its model",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_component_drift.py", "--all"),
        classes=("modeling", "codegen"),
        phase=PREPARE,
        fix="Run compass codegen regenerate for the component the report names.",
    ),
    Check(
        id="drift-sweep",
        title="A render of every catalogue component matches the tree",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_component_drift.py", "--sweep"),
        classes=("modeling", "codegen"),
    ),
    Check(
        id="paste-blocks",
        title="Every pasted block names a symbol that exists",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_paste_block_symbols.py"),
        classes=("modeling", "codegen"),
    ),
    Check(
        id="model-drift",
        title="Every model matches its schema",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_model_drift.py", "--summary"),
        classes=("modeling", "codegen"),
        phase=PREPARE,
    ),
    Check(
        id="er-diagram",
        title="The committed ER diagram matches the schema",
        argv=(
            "bash",
            "-c",
            "python3 projects/ores.codegen/src/plantuml_er_parse_sql.py"
            " --create-dir projects/ores.sql/create --drop-dir projects/ores.sql/drop"
            " --output build/output/codegen/plantuml_er_model.json"
            " --ignore-file projects/ores.sql/utility/validation_ignore.txt --warn --strict"
            " && python3 projects/ores.codegen/src/plantuml_er_generate.py"
            " --model build/output/codegen/plantuml_er_model.json"
            " --template projects/ores.codegen/library/templates/plantuml_er.mustache"
            " --output projects/ores.sql/modeling/ores_schema.puml --check",
        ),
        classes=("sql", "modeling"),
        fix="Run projects/ores.codegen/plantuml_er_generate.sh and commit the diagram.",
    ),
    Check(
        id="sql-hygiene",
        title="The SQL tree has no unterminated comment or repeated index",
        argv=("python3", "projects/ores.sql/utility/check_sql_hygiene.py", "projects/ores.sql"),
        classes=("sql",),
    ),
    Check(
        id="marketdata-identity-columns",
        title="The series identity projection matches the codec schema",
        argv=("python3", "build/scripts/check_marketdata_identity_columns.py"),
        classes=("sql", "cpp", "modeling"),
        paths=(
            "projects/ores.marketdata/api/include/ores.marketdata.api/datum/schema.hpp",
            "projects/ores.marketdata/core/src/repository/market_series_identity_projector.cpp",
            "projects/ores.marketdata/modeling/ores.marketdata.market_series_identity.org",
            "projects/ores.sql/create/marketdata/marketdata_market_series_identity_create.sql",
        ),
        fix="Add the column to the create table, or place the field in the"
            " projector's switch, to match the codec schema. One column per"
            " identity field the schema declares, and no other.",
    ),
    Check(
        id="generated-column-refs",
        title="Generated code names no column its entity does not have",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_generated_column_refs.py"),
        classes=("cpp", "sql", "modeling", "codegen"),
        fix="Remove the column from the model's Table display or Indexes table,"
            " or declare it in Columns, then regenerate the entity."
            " check_model_drift cannot see this: the generator reproduces both"
            " sections verbatim, so the output is in step with the model and"
            " only the compiler or a database recreate disagrees.",
    ),
    Check(
        id="protocol-twins",
        title="Every protocol header has its TypeScript twin",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_protocol_twin_coverage.py"),
        classes=("modeling", "codegen"),
    ),
    Check(
        id="subject-conformance",
        title="Every declared subject is inside the entity protocol",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_subject_conformance.py"),
        classes=("modeling", "codegen"),
        fix="Rename the verb to one of the closed set of eight, or move the"
            " operation to the reserved ops namespace. A genuine exception goes"
            " in subject_conformance_baseline.json with its reason.",
    ),
    Check(
        id="handler-permissions",
        title="Every handler permission code is seeded",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_handler_permissions.py"),
        classes=("modeling", "sql", "codegen"),
    ),
    Check(
        id="open-reads",
        title="Every open read is declared",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_open_reads.py"),
        classes=("modeling", "codegen"),
    ),
    Check(
        id="shell-menus",
        title="No two shell menu entries collide",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_shell_menu_collisions.py"),
        classes=("modeling", "codegen"),
    ),
    Check(
        id="route-auth",
        title="Every route declares its authorisation",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_route_auth_declarations.py"),
        classes=("modeling", "codegen"),
    ),
    Check(
        id="recipe-inventories",
        title="The shell and HTTP recipe inventories match the tree",
        argv=(
            "bash",
            "-c",
            "python3 projects/ores.codegen/scripts/regenerate_shell_recipe_inventory.py --check"
            " && python3 projects/ores.codegen/scripts/regenerate_http_recipe_inventory.py --check",
        ),
        classes=("modeling", "codegen", "docs"),
        paths=("doc/recipes/shell/*", "doc/recipes/http/*"),
        after=("component-drift",),
        phase=PREPARE,
    ),
    Check(
        id="physical-inventory",
        title="The physical-space inventories match the templates",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/regenerate_physical_space_inventories.py", "--check"),
        classes=("modeling", "codegen"),
        after=("component-drift",),
        phase=PREPARE,
        fix="Run the same command without --check, then commit the inventories.",
    ),
    Check(
        id="registry-exceptions",
        title="Every registry exception is justified",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_registry_exceptions.py"),
        classes=("codegen",),
    ),
    Check(
        id="reachability",
        title="No test case sits inside a conditional compilation block",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_test_case_reachability.py"),
        classes=("cpp", "modeling", "codegen"),
    ),
    Check(
        id="event-registrars",
        title="Every generated event registrar is composed",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_event_registrar_composition.py"),
        classes=("services", "cpp", "modeling", "codegen"),
    ),
    Check(
        id="populate-refs",
        title="Every populate script names a reference something defines",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_populate_references.py"),
        classes=("sql", "modeling"),
    ),
    Check(
        id="sql-reachability",
        title="Every SQL file is reached by a schema entry point",
        argv=(
            CODEGEN_PY,
            "projects/ores.codegen/scripts/check_sql_reachability.py",
        ),
        classes=("sql", "modeling"),
        fix="Add an include to the tree's aggregator in dependency order, or"
            " declare the file an entry point if it is run on its own.",
    ),
    Check(
        id="rls-widenings",
        title="No row-level security policy widens the system tenant",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_rls_system_tenant_widenings.py"),
        classes=("sql",),
    ),
    Check(
        id="entity-writes",
        title="Only the repositories write an entity table",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_entity_table_writes.py"),
        classes=("sql", "cpp", "modeling", "codegen"),
        fix="Move the write into the repository, or add the writer to the"
            " ratchet baseline at projects/ores.codegen/scripts/entity_table_writes_baseline.json.",
    ),
    Check(
        id="enum-attributes",
        title="No visibility macro decorates an enum",
        argv=("python3", "build/scripts/check_enum_export_attributes.py"),
        classes=("cpp", "modeling", "codegen"),
        fix="Remove the export macro from the enum the report names.",
    ),
    Check(
        id="exported-types",
        title="Every exported type is compiled",
        argv=("python3", "build/scripts/check_exported_types_are_compiled.py"),
        classes=("cpp",),
    ),
    Check(
        id="boost-deps",
        title="No Boost header is included where it is not declared",
        argv=("python3", "build/scripts/check_boost_dependencies.py"),
        classes=("cpp",),
    ),
    Check(
        id="format",
        title="The changed C++ files are clang-formatted",
        argv=(
            "bash",
            "-c",
            "files=$(git diff --name-only --diff-filter=ACMR {base} -- '*.cpp' '*.hpp' '*.h'); "
            "if [ -z \"$files\" ]; then echo 'no changed C++ files'; exit 0; fi; "
            "echo \"$files\" | xargs clang-format --dry-run --Werror",
        ),
        classes=("cpp",),
        fix="clang-format -i on the files the report names.",
    ),
    Check(
        id="web",
        title="ores.web typechecks, formats and tests",
        argv=(
            "bash",
            "-c",
            "npm ci --no-audit --no-fund && npm run typecheck && npm run format:check && npm test",
        ),
        classes=("web",),
        cwd="projects/ores.web",
        # npm ci deletes and rebuilds node_modules, which the codegen
        # formatting step reads prettier from. It mutates what other checks
        # use, so it runs in the serial prepare phase like the other writers.
        phase=PREPARE,
    ),
    Check(
        id="dsh-environment",
        title="The DSH environment plugin passes its own checks",
        argv=(
            "bash",
            "-c",
            "node scripts/check-manifest.mjs && node --check lib/client.js && node --test",
        ),
        classes=("plugins",),
        paths=("projects/ores.dsh_environment/*",),
        cwd="projects/ores.dsh_environment",
    ),
    Check(
        id="dsh-kanban",
        title="The DSH kanban plugin passes its own checks",
        argv=(
            "bash",
            "-c",
            "node scripts/check-manifest.mjs && node --check lib/client.js && node --test",
        ),
        classes=("plugins",),
        paths=("projects/ores.dsh_kanban/*",),
        cwd="projects/ores.dsh_kanban",
    ),
    Check(
        id="codegen-tests",
        title="The codegen suite passes",
        argv=(CODEGEN_TESTS_PY, "-m", "pytest", "projects/ores.codegen/tests", "-q"),
        classes=("modeling", "codegen", "tooling"),
    ),
    Check(
        id="compass-tests",
        title="The compass suite passes",
        argv=("projects/ores.compass/venv/bin/pytest", "projects/ores.compass/tests", "-q"),
        classes=("tooling",),
    ),
    # -- heavy: the compile-and-test path ----------------------------------
    Check(
        id="build",
        title="The tree configures and compiles",
        argv=("./compass.sh", "build", "--preset", "{preset}"),
        classes=("cpp", "services", "sql"),
        phase=HEAVY,
        timeout=7200,
    ),
    Check(
        id="db",
        title="The database rebuilds from the schema and the populate scripts",
        argv=("./compass.sh", "db", "recreate", "-y", "-k"),
        classes=("cpp", "services", "sql"),
        phase=HEAVY,
        timeout=7200,
    ),
    Check(
        id="ctest",
        title="Every test suite passes",
        argv=("./compass.sh", "test", "run", "--preset", "{preset}"),
        classes=("cpp", "services", "sql"),
        phase=HEAVY,
        timeout=7200,
    ),
    # Runs against the database the db check above rebuilt, so it stays after
    # it. Every suite writes the reference data it reads and rolls it back,
    # which is what lets the corpus pass on that fresh database.
    Check(
        id="pgtap",
        title="Every pgTAP suite passes",
        argv=("./projects/ores.sql/test/run_tests.sh",),
        classes=("sql",),
        phase=HEAVY,
        timeout=1800,
        fix="Read the suite's plan line and the first ERROR it printed.",
    ),
)

CHECKS_BY_ID = {check.id: check for check in CATALOGUE}


# --------------------------------------------------------------------------
# Running
# --------------------------------------------------------------------------


@dataclass
class Result:
    check: Check
    status: str = "SKIP"
    seconds: float = 0.0
    log: Path | None = None
    tail: str = ""

    @property
    def failed(self) -> bool:
        return self.status == "FAIL"


def select_checks(
    changed: list[str],
    classes: set[str] | None = None,
    run_all: bool = False,
) -> tuple[set[str], list[Check]]:
    """The classes and the checks a change needs.

    A path no rule claims selects every check: the table cannot vouch for it.
    A forced class overrides the classification but not the path narrowing.
    """
    if run_all:
        return set(classes or CLASSES), list(CATALOGUE)
    found, _ = classify(changed)
    if classes:
        found = set(classes)
    if not changed:
        return found, []
    if "unknown" in found:
        return found, list(CATALOGUE)
    return found, [check for check in CATALOGUE if check.applies(found, changed)]


def git(*args: str) -> str:
    proc = subprocess.run(
        ["git", *args], cwd=REPO_ROOT, capture_output=True, text=True, check=False
    )
    return proc.stdout


def diff_ref(base: str) -> str:
    """The commit the change is measured from.

    The merge base, not the base tip: main moves under a branch, and another
    author's merge must not read as this branch's change. Falls back to the
    base itself when the two share no ancestor.
    """
    return git("merge-base", base, "HEAD").strip() or base


def changed_paths(ref: str) -> list[str]:
    """Every path this branch touches, committed or not, tracked or not."""
    tracked = git("diff", "--name-only", ref).splitlines()
    untracked = git("ls-files", "--others", "--exclude-standard").splitlines()
    seen: list[str] = []
    for path in [*tracked, *untracked]:
        path = path.strip()
        if path and path not in seen:
            seen.append(path)
    return seen


def read_preset() -> str:
    env = REPO_ROOT / ".env"
    if env.exists():
        for line in env.read_text().splitlines():
            if line.startswith("ORES_PRESET="):
                return line.split("=", 1)[1].strip()
    return "linux-clang-debug-make"


def missing_tools(checks: list[Check]) -> list[str]:
    """The tools a selected check needs that this machine does not have."""
    needed = {"emacs": "emacs", "node": "node", "npm": "npm", "clang-format": "clang-format"}
    missing: list[str] = []
    blob = " ".join(" ".join(check.argv) for check in checks)
    for tool, binary in needed.items():
        if tool in blob and shutil.which(binary) is None:
            missing.append(binary)
    return missing


def run_one(check: Check, base: str, preset: str) -> Result:
    argv = check.command(base, preset)
    LOG_DIR.mkdir(parents=True, exist_ok=True)
    log = LOG_DIR / f"{check.id}.log"
    started = time.monotonic()
    try:
        proc = subprocess.run(
            argv,
            cwd=REPO_ROOT / check.cwd,
            capture_output=True,
            text=True,
            check=False,
            env=os.environ.copy(),
            timeout=check.timeout,
        )
        output = proc.stdout + proc.stderr
        status = "PASS" if proc.returncode == 0 else "FAIL"
    except subprocess.TimeoutExpired:
        output = f"timed out after {check.timeout}s\n"
        status = "FAIL"
    except FileNotFoundError as exc:
        output = f"{argv[0]}: {exc}\n"
        status = "FAIL"
    except Exception as exc:  # noqa: BLE001 - a broken check must not stop the run
        output = f"{type(exc).__name__}: {exc}\n"
        status = "FAIL"
    seconds = time.monotonic() - started
    log.write_text(f"$ {' '.join(argv)}\n\n{output}")
    tail = "\n".join(output.strip().splitlines()[-TAIL_LINES:])
    return Result(check=check, status=status, seconds=seconds, log=log, tail=tail)


def run_all(selected: list[Check], base: str, preset: str, jobs: int) -> list[Result]:
    results: list[Result] = []
    by_phase: dict[int, list[Check]] = {}
    for check in selected:
        by_phase.setdefault(check.phase, []).append(check)

    # Every check that exists, not every check that was selected: a
    # dependency names a check, and whether it runs this time is a separate
    # question. A narrow selection that picks site-page without
    # component-drift must not look like a typo.
    known = {check.id for check in CATALOGUE}
    for phase in sorted(by_phase):
        group = in_dependency_order(by_phase[phase], known)
        print(f"\n▶  {PHASE_NAMES.get(phase, phase)} ({len(group)})", flush=True)
        if phase == CHECKS and jobs > 1:
            with ThreadPoolExecutor(max_workers=jobs) as pool:
                phase_results = list(pool.map(lambda c: run_one(c, base, preset), group))
        else:
            phase_results = []
            for check in group:
                print(f"   running {check.id}…", flush=True)
                phase_results.append(run_one(check, base, preset))
        for result in phase_results:
            print(
                f"   {'✅' if not result.failed else '❌'} {result.check.id} "
                f"({result.seconds:.1f}s)",
                flush=True,
            )
        results.extend(phase_results)
    return results


# --------------------------------------------------------------------------
# Reporting
# --------------------------------------------------------------------------


def render(classes: set[str], reasons: dict[str, list[str]], changed: list[str],
           base: str, base_sha: str, results: list[Result], preset: str,
           ref: str | None = None) -> str:
    ref = ref or base
    passed = [r for r in results if r.status == "PASS"]
    failed = [r for r in results if r.failed]
    total = sum(r.seconds for r in results)

    lines: list[str] = []
    lines.append("# Local checks")
    lines.append("")
    verdict = "PASS" if not failed else f"FAIL — {len(failed)} check(s)"
    lines.append(f"**{verdict}.**  {len(results)} run, {len(passed)} passed, "
                 f"{len(failed)} failed.  Wall clock in checks: {total:.0f}s.")
    lines.append("")
    lines.append(f"- Base: `{base}`, merge base `{base_sha}`")
    lines.append(f"- Preset: `{preset}`")
    lines.append(f"- Changed files: {len(changed)}")
    if classes:
        lines.append("- Classes: " + ", ".join(
            "`unknown` (ran everything)" if name == "unknown" else f"`{name}`"
            for name in sorted(classes)
        ))
    else:
        lines.append("- Classes: none")
    lines.append("")

    if failed:
        lines.append("## Fix these first")
        lines.append("")
        for result in failed:
            lines.append(f"### `{result.check.id}` — {result.check.title}")
            lines.append("")
            lines.append(f"Command: `{' '.join(result.check.command(ref, preset))}`")
            if result.check.fix:
                lines.append(f"Fix: {result.check.fix}")
            if result.log:
                lines.append(f"Log: `{result.log.relative_to(REPO_ROOT)}`")
            lines.append("")
            lines.append("```")
            lines.append(result.tail or "(no output)")
            lines.append("```")
            lines.append("")

    lines.append("## Every check")
    lines.append("")
    lines.append("| Check | Result | Time | What it proves |")
    lines.append("|-------+--------+------+----------------|")
    for result in results:
        mark = {"PASS": "pass", "FAIL": "**FAIL**", "SKIP": "skip"}[result.status]
        lines.append(
            f"| `{result.check.id}` | {mark} | {result.seconds:.1f}s | {result.check.title} |"
        )
    lines.append("")

    lines.append("## Why these checks")
    lines.append("")
    for name in sorted(classes):
        if name == "unknown":
            lines.append("- `unknown`: a path no rule claims, so every check ran.")
            continue
        files = reasons.get(name, [])
        sample = ", ".join(f"`{f}`" for f in files[:4])
        more = f" (+{len(files) - 4})" if len(files) > 4 else ""
        lines.append(f"- `{name}` — {CLASS_NOTES.get(name, '')}: {sample}{more}")
    lines.append("")
    return "\n".join(lines)


def console_summary(results: list[Result], report: Path | None) -> None:
    failed = [r for r in results if r.failed]
    print()
    if not failed:
        print(f"✅  local checks: {len(results)}/{len(results)} passed")
    else:
        print(f"❌  local checks: {len(failed)} of {len(results)} failed")
        for result in failed:
            print(f"   • {result.check.id}: {result.check.title}")
            if result.check.fix:
                print(f"     fix: {result.check.fix}")
    if report:
        print(f"📝 Report: {report.relative_to(REPO_ROOT)}")


# --------------------------------------------------------------------------
# Command line
# --------------------------------------------------------------------------


def print_tree() -> None:
    print("Decision tree — the class a changed path belongs to\n")
    for name, patterns in PATH_RULES:
        print(f"  {name:<10} {CLASS_NOTES.get(name, '')}")
        for pattern in patterns:
            print(f"             {pattern}")
        print()
    print("  A path matching no rule asks for every check.\n")


def print_catalogue() -> None:
    print("Checks\n")
    for phase in sorted(PHASE_NAMES):
        group = [c for c in CATALOGUE if c.phase == phase]
        if not group:
            continue
        print(f"  {PHASE_NAMES[phase]}")
        for check in group:
            classes = ",".join(check.classes) or "every change"
            print(f"    {check.id:<20} [{classes}]")
            print(f"      {' '.join(check.argv)}")
        print()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="local_checks",
        description="Run the checks a change needs, locally, and write one report.",
    )
    parser.add_argument("--base", default="origin/main",
                        help="Ref the change is measured against (default: origin/main).")
    parser.add_argument("--class", dest="classes", action="append", default=[],
                        help="Force a change class (repeatable); skips classification.")
    parser.add_argument("--only", action="append", default=[],
                        help="Run one check by id (repeatable).")
    parser.add_argument("--all", action="store_true", help="Run every check.")
    parser.add_argument("--list", action="store_true", help="Print the tree and the catalogue, then exit.")
    parser.add_argument("--tree", action="store_true", help="Print the decision tree, then exit.")
    parser.add_argument("--plan", action="store_true",
                        help="Classify and print the checks that would run, then exit.")
    parser.add_argument("--paths", action="append", default=[],
                        help="Classify these paths instead of the branch diff (repeatable).")
    parser.add_argument("--json", action="store_true", help="Also write report.json.")
    parser.add_argument("--jobs", type=int, default=min(8, (os.cpu_count() or 2)),
                        help="Checks to run at once (default: 8).")
    parser.add_argument("--include-heavy", action="store_true",
                        help="Include build, database and ctest even for a change that does not need them.")
    parser.add_argument("--exclude-heavy", action="store_true",
                        help="Never run build, database or ctest.")
    args = parser.parse_args(argv)

    if args.tree:
        print_tree()
        return 0
    if args.list:
        print_tree()
        print_catalogue()
        return 0

    base = args.base
    ref = diff_ref(base)
    base_sha = git("rev-parse", "--short", ref).strip() or "unknown"
    changed = args.paths if args.paths else changed_paths(ref)
    classes, reasons = classify(changed)
    preset = read_preset()

    if args.only:
        selected = [CHECKS_BY_ID[c] for c in args.only if c in CHECKS_BY_ID]
        unknown = [c for c in args.only if c not in CHECKS_BY_ID]
        if unknown:
            print(f"unknown check id(s): {', '.join(unknown)}", file=sys.stderr)
            return 2
    else:
        if not changed and not args.all and not args.classes:
            print("nothing changed against the base; nothing to check")
            return 0
        classes, selected = select_checks(
            changed,
            classes=set(args.classes) or None,
            run_all=args.all,
        )
        if args.classes:
            reasons = {name: [] for name in classes}

    if args.exclude_heavy:
        selected = [c for c in selected if c.phase != HEAVY]
    if args.include_heavy:
        selected = selected + [
            c for c in CATALOGUE if c.phase == HEAVY and c not in selected
        ]

    print("🧭 ores.local_checks")
    print(f"   base     : {base} ({base_sha})")
    print(f"   changed  : {len(changed)} file(s)")
    print(f"   classes  : {', '.join(sorted(classes)) or '(none)'}")
    print(f"   checks   : {len(selected)}")
    print(f"   preset   : {preset}")

    if args.plan:
        print()
        for phase in sorted({c.phase for c in selected}):
            print(f"  {PHASE_NAMES.get(phase, phase)}")
            for check in selected:
                if check.phase == phase:
                    where = f"  (in {check.cwd})" if check.cwd != "." else ""
                    print(f"    {check.id:<20} {' '.join(check.command(ref, preset))}{where}")
        print()
        print("Why these checks")
        for name in sorted(classes):
            if name == "unknown":
                print("  unknown — a path no rule claims, so every check runs")
                continue
            files = reasons.get(name, [])
            sample = ", ".join(files[:4])
            more = f" (+{len(files) - 4})" if len(files) > 4 else ""
            print(f"  {name:<10} {sample}{more}")
        return 0

    missing = missing_tools(selected)
    if missing:
        print(f"⚠️  missing tools: {', '.join(missing)} — their checks will fail")

    results = run_all(selected, ref, preset, args.jobs)

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    report = OUTPUT_DIR / "report.md"
    report.write_text(
        render(classes, reasons, changed, base, base_sha, results, preset, ref=ref))
    if args.json:
        (OUTPUT_DIR / "report.json").write_text(
            json.dumps(
                {
                    "base": base,
                    "base_sha": base_sha,
                    "classes": sorted(classes),
                    "preset": preset,
                    "changed": changed,
                    "results": [
                        {
                            "id": r.check.id,
                            "status": r.status,
                            "seconds": round(r.seconds, 2),
                            "argv": r.check.command(ref, preset),
                            "log": str(r.log.relative_to(REPO_ROOT)) if r.log else None,
                        }
                        for r in results
                    ],
                },
                indent=2,
            )
        )

    console_summary(results, report)
    return 1 if any(r.failed for r in results) else 0


if __name__ == "__main__":
    sys.exit(main())
