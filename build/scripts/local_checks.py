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
    ("plugins", ("projects/ores.dsh_environment/*",)),
    ("tooling", ("projects/ores.compass/*", "projects/ores.ore/scripts/*",
                 "build/scripts/*")),
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
        argv=(
            "bash",
            "-c",
            "changed=$(git diff --name-only --diff-filter=ACMR {base} -- '*.org'; "
            "git ls-files --others --exclude-standard -- '*.org'); "
            "if [ -z \"$changed\" ]; then echo 'no changed org pages'; exit 0; fi; "
            "./compass.sh site page $changed",
        ),
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
        id="heading-numbers",
        title="No org heading carries its own number",
        argv=("python3", "build/scripts/check_heading_numbers.py"),
        classes=("docs",),
        fix="Run: python3 build/scripts/check_heading_numbers.py --write."
            " Org numbers every exported section, so a heading that numbers"
            " itself renders as '2.1. 1. The document's shape has one owner',"
            " and the copy goes stale as soon as a section is inserted above it.",
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
        id="rule-identifiers",
        title="Every UX rule carries one identifier and the checklist cites them",
        argv=("python3", "build/scripts/check_rule_identifiers.py"),
        classes=("docs",),
        fix="Run build/scripts/check_rule_identifiers.py --assign for a rule without an identifier; cite an identifier on the checklist line it names.",
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
    # The generated tables are the only copy of the schema's shape we keep, so
    # a hand edit or a schema change that the tables did not follow is the same
    # failure from here.
    Check(
        id="xml-bindings-drift",
        title="The generated codec tables match the schemas",
        argv=("python3", "build/scripts/check_xml_bindings_drift.py"),
        classes=("cpp", "codegen"),
        paths=("projects/ores.ore/scripts/*",
               "projects/ores.ore/core/include/ores.ore.core/xml/generated/*",
               "projects/ores.ore/core/src/xml/generated/*",
               "build/scripts/check_xml_bindings_drift.py",
               "external/ore/xsd/*"),
        fix="Run ./projects/ores.ore/scripts/regenerate_xml_bindings.sh and commit.",
    ),
    # The gate is narrow on purpose: it only applies when a file that changes
    # what the benchmark measures changed. A change to the XML sources, or to
    # the benchmark's own harness, owes a measurement; nothing else does.
    Check(
        id="xml-benchmark-recorded",
        title="An XML code change has a benchmark row recorded after it",
        argv=("python3", "build/scripts/check_xml_benchmark_recorded.py"),
        classes=("cpp",),
        paths=("projects/ores.ore/core/src/xml/*",
               "projects/ores.ore/core/include/ores.ore.core/xml/*",
               "benchmarks/xml/bench.cpp",
               "benchmarks/xml/run.sh",
               "benchmarks/xml/inputs.tsv"),
        fix="Run ./benchmarks/xml/run.sh and commit benchmarks/xml/history.tsv.",
    ),
    # The conventions document held every family in one type, and a unit that
    # reflected it broke the MSVC build. This keeps a unit to one family.
    Check(
        id="translation-unit-budget",
        title="No unit reflects more convention families than its budget",
        argv=("python3", "build/scripts/check_translation_unit_budget.py"),
        classes=("cpp",),
        paths=("projects/ores.refdata/core/src/*",
               "projects/ores.refdata/core/include/*",
               "projects/ores.refdata/api/include/ores.refdata.api/domain/convention_family_registry.hpp",
               "projects/ores.ore/core/src/*",
               "projects/ores.ore/core/include/*",
               "projects/ores.ore/service/src/*",
               "build/scripts/check_translation_unit_budget.py"),
        fix="Move each family's serialisation into a unit of its own, or lower the "
            "ceiling in KNOWN when a file has improved.",
    ),
    # The tree builds as a unity build. Generated code never collides; the
    # hand-written files that did are recorded, and the record only shrinks.
    Check(
        id="unity-collisions",
        title="No two sources of a target define the same file-local name",
        argv=("python3", "build/scripts/check_unity_collisions.py"),
        classes=("cpp",),
        paths=("projects/*",
               "cmake/unity_exclusions.cmake",
               "build/scripts/check_unity_collisions.py"),
        fix="Name the new helper for what it does, or share one definition; when a "
            "recorded file no longer collides, remove it from cmake/unity_exclusions.cmake.",
    ),
    # Two types of one qualified name share their template instantiations at
    # link time, so a message can be encoded with the other type's layout.
    Check(
        id="unique-type-names",
        title="No two headers define a type of the same name in one namespace",
        argv=("python3", "build/scripts/check_unique_type_names.py"),
        classes=("cpp",),
        paths=("projects/*",
               "build/scripts/check_unique_type_names.py"),
        fix="Rename one of the two types; for a generated message, rename it in its model.",
    ),
    # A family is either supported by a table, migrating from a mapper, or
    # unused. The census is a decision; this check is whether the tree still
    # agrees with it.
    Check(
        id="xml-family-census",
        title="Every ORE document family is classified and the tree agrees",
        argv=("python3", "build/scripts/check_xml_family_coverage.py"),
        classes=("cpp", "tooling"),
        paths=("external/ore/examples/*",
               "projects/ores.ore/core/src/xml/generated/*",
               "projects/*/*/tests/xml_*_tests.cpp",
               "build/scripts/check_xml_family_coverage.py"),
        fix="Classify the family in build/scripts/check_xml_family_coverage.py, "
            "or delete the test that names an unused one.",
    ),
    # The classification work needs a list that cannot be short. This reads the
    # routes from the code that registers them, on both surfaces, and fails when
    # one is answered but absent from the inventory, when the inventory names a
    # route nobody answers, or when a row disagrees with the decisions in
    # route_classification.
    Check(
        id="route-inventory",
        title="Every HTTP route is in the inventory",
        argv=("python3", "build/scripts/route_inventory.py", "--check"),
        classes=("cpp", "web", "docs", "codegen", "tooling"),
        paths=("projects/ores.http/*",
               "projects/ores.web/packages/bff/src/*",
               "doc/knowledge/architecture/http_route_inventory.org",
               "build/scripts/route_inventory.py",
               "build/scripts/route_classification.py"),
        fix="Run python3 build/scripts/route_inventory.py --write and commit the "
            "table, or delete the row of a route neither server answers.",
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
        id="template-sections",
        title="A mustache line holds one section tag, so the renderer can strip it",
        argv=("python3",
              "projects/ores.codegen/scripts/check_template_sections.py"),
        classes=("codegen", "modeling"),
        paths=(
            "projects/ores.codegen/library/templates/*",
            "projects/ores.codegen/scripts/check_template_sections.py",
            "projects/ores.codegen/scripts/template_section_baseline.json",
        ),
        fix="Give each section tag its own line. The renderer strips a line that"
            " holds nothing but one tag, so a line chaining several tags leaks a"
            " newline into every file the template produces.",
    ),
    Check(
        id="component-drift",
        title="The components this branch changed match their models, rendered without writing",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_component_drift.py",
              "--changed", "--base", "{base}"),
        classes=("modeling", "codegen"),
        phase=PREPARE,
        timeout=1800,
        fix="Run compass codegen regenerate for the component the report names."
            " The check renders into a temporary root and writes nothing; if a"
            " regeneration is needed, --write does it, and refuses on a dirty tree.",
    ),
    Check(
        id="drift-sweep",
        title="A render of every catalogue component matches the tree",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_component_drift.py", "--sweep"),
        classes=("modeling", "codegen"),
        timeout=1800,
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
        title="The committed ER diagrams match the schema",
        argv=(
            "bash",
            "-c",
            "projects/ores.codegen/venv/bin/python projects/ores.codegen/src/plantuml_er_parse_sql.py"
            " --create-dir projects/ores.sql/create --drop-dir projects/ores.sql/drop"
            " --output build/output/codegen/plantuml_er_model.json"
            " --ignore-file projects/ores.sql/utility/validation_ignore.txt --warn --strict"
            " && projects/ores.codegen/venv/bin/python projects/ores.codegen/src/plantuml_er_generate.py"
            " --model build/output/codegen/plantuml_er_model.json"
            " --template projects/ores.codegen/library/templates/plantuml_er.mustache"
            " --index-template projects/ores.codegen/library/templates/plantuml_er_index.mustache"
            " --output projects/ores.sql/modeling/ores_schema.puml --check",
        ),
        classes=("sql", "modeling"),
        fix="Run projects/ores.codegen/plantuml_er_generate.sh and commit the diagrams.",
    ),
    Check(
        id="money-decimal-columns",
        title="A money quantity is an exact decimal, not a binary float",
        argv=("python3", "build/scripts/check_money_decimal_columns.py"),
        classes=("modeling",),
        fix="Give the column an exact decimal type and take the double out of"
            " the domain type, or, when the column is a continuous quantity"
            " such as a volatility, exclude its entity in the check. The rule"
            " is in"
            " doc/knowledge/architecture/exact_numbers_and_economic_change.org.",
    ),
    Check(
        id="model-sql-types",
        title="A column's model type is the type the table is created with",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_model_sql_types.py"),
        classes=("modeling", "sql", "codegen"),
        paths=(
            "projects/*/modeling/*.org",
            "projects/*/*/modeling/*.org",
            "projects/ores.sql/create/*/*_create.sql",
        ),
        fix="Regenerate the component, then rebuild and recreate, or correct"
            " the model's :type: when the model is the one that is wrong."
            " model-drift compares structure and not types, so it stays green"
            " while the two disagree.",
    ),
    Check(
        id="entity-binary-types",
        title="A row binds each column in the format its column's type defines",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_entity_binary_types.py"),
        classes=("cpp", "sql", "modeling", "codegen"),
        paths=(
            "projects/*/*/include/*/repository/*_entity.hpp",
            "projects/*/modeling/*.org",
            "projects/*/*/modeling/*.org",
            "projects/ores.sql/create/*/*_create.sql",
            # The generator and its templates are what decide whether a column
            # binds binary or as text, so a change there must run the check.
            "projects/ores.codegen/src/*.py",
            "projects/ores.codegen/src/*/*.py",
            "projects/ores.codegen/library/templates/*",
            "projects/ores.codegen/scripts/check_entity_binary_types.py",
        ),
        fix="Give the model the type its column has and regenerate, or state the"
            " column as text when the row keeps a type the layer has no codec for."
            " The statement names no parameter types, so the server infers each"
            " from its column and refuses a member's bytes with incorrect binary"
            " data format in bind parameter N. model-sql-types compares the model"
            " with the table and neither of them with the member.",
    ),
    Check(
        id="sql-hygiene",
        title="The SQL tree has no split string, unterminated comment or repeated index",
        argv=("python3", "projects/ores.sql/utility/check_sql_hygiene.py", "projects/ores.sql"),
        classes=("sql",),
    ),
    Check(
        id="file-types-seed",
        title="The seeded file type names are the names the ORE samples use",
        argv=("python3", "projects/ores.sql/utility/generate_file_types_populate.py", "--check"),
        classes=("sql",),
        paths=(
            "projects/ores.sql/populate/reporting/reporting_file_types_populate.sql",
            "projects/ores.sql/utility/generate_file_types_populate.py",
            "projects/ores.sql/utility/file_type_descriptions.py",
            "projects/ores.sql/utility/file_type_guide.json",
        ),
        fix="Run projects/ores.sql/utility/generate_file_types_populate.py and commit the seed.",
    ),
    Check(
        id="ore-parameters-seed",
        title="The staged ore parameters match their texts",
        argv=("python3", "projects/ores.sql/utility/generate_ore_parameters_dataset.py", "--check"),
        classes=("sql",),
        paths=(
            "projects/ores.sql/populate/reporting/reporting_ore_parameters_artefact_populate.sql",
            "projects/ores.sql/utility/generate_ore_parameters_dataset.py",
            "projects/ores.sql/utility/ore_parameter_seed.py",
            "projects/ores.sql/utility/setup_parameter_rows.py",
            "projects/ores.sql/utility/setup_parameter_descriptions.py",
            "projects/ores.sql/utility/block_parameter_rows.py",
            "projects/ores.sql/utility/block_parameter_catalogue.py",
            "projects/ores.sql/utility/block_parameter_releases.py",
            "projects/ores.sql/utility/block_parameter_descriptions/*",
            "projects/ores.sql/utility/analytic_parameter_rows.py",
            "projects/ores.sql/utility/analytic_parameter_catalogue.py",
            "projects/ores.sql/utility/analytic_parameter_releases.py",
            "projects/ores.sql/utility/analytic_parameter_descriptions/*",
            "projects/ores.sql/utility/pricing_parameter_rows.py",
            "projects/ores.sql/utility/pricing_parameter_releases.json",
            "projects/ores.seeder/datasets/ore_pricing_engines/pricing_vocabulary.json",
            "projects/ores.seeder/datasets/ore_pricing_engines/curation/*",
            "projects/ores.sql/utility/convention_parameter_rows.py",
            "projects/ores.sql/utility/convention_terms.json",
            "projects/ores.sql/utility/convention_term_curation.json",
            "projects/ores.sql/utility/curve_parameter_rows.py",
            "projects/ores.sql/utility/curve_terms.json",
            "projects/ores.sql/utility/curve_term_curation.json",
            "projects/ores.sql/utility/extract_curve_terms.py",
        ),
        fix="Run projects/ores.sql/utility/generate_ore_parameters_dataset.py and commit the seed.",
    ),
    Check(
        id="ore-parameter-value-sets",
        title="The ore parameter value sets match their sources",
        argv=("python3", "projects/ores.sql/utility/generate_value_sets.py", "--check"),
        classes=("sql",),
        paths=(
            "projects/ores.sql/populate/reporting/reporting_ore_parameter_value_sets_populate.sql",
            "projects/ores.sql/populate/reporting/reporting_ore_parameter_vocabularies_populate.sql",
            "projects/ores.sql/utility/generate_value_sets.py",
            "projects/ores.sql/utility/curve_value_sets.py",
            "projects/ores.sql/utility/curve_parameter_rows.py",
            "projects/ores.sql/utility/curve_terms.json",
            "projects/ores.sql/utility/curve_term_curation.json",
            "projects/ores.sql/utility/curve_value_set_curation.json",
            "projects/ores.sql/utility/ore_parameter_vocabularies.json",
            "projects/ores.sql/utility/ore_schema_types.py",
            "projects/ores.sql/utility/convention_value_sets.py",
            "projects/ores.sql/utility/convention_parameter_rows.py",
            "projects/ores.sql/utility/convention_terms.json",
            "projects/ores.sql/utility/convention_term_curation.json",
            "projects/ores.seeder/datasets/ore_pricing_engines/pricing_vocabulary.json",
            "projects/ores.seeder/datasets/ore_pricing_engines/curation/*",
        ),
        fix="Run projects/ores.sql/utility/generate_value_sets.py and commit the populate script.",
    ),
    Check(
        id="parameter-value-sets-only",
        title="A parameter's control and rule come only from its value set",
        argv=("python3", "build/scripts/check_parameter_value_sets.py"),
        classes=("sql", "modeling", "web", "cpp"),
        paths=(
            "projects/ores.sql/populate/reporting/reporting_ore_parameter_value_sets_populate.sql",
            "projects/ores.sql/utility/generate_value_sets.py",
            "projects/ores.sql/utility/setup_value_sets.py",
            "projects/ores.sql/utility/convention_value_sets.py",
            "projects/ores.sql/utility/curve_value_sets.py",
            "projects/ores.seeder/datasets/ore_pricing_engines/curation/*",
            "projects/ores.seeder/datasets/ore_pricing_engines/pricing_vocabulary.json",
            "projects/ores.sql/create/analytics/analytics_pricing_pair_settings_create.sql",
            "projects/ores.analytics/modeling/ores.analytics.pricing_pair_setting.org",
            "projects/ores.sql/create/reporting/reporting_engine_settings_group_settings_create.sql",
            "projects/ores.reporting/modeling/ores.reporting.engine_settings_group_setting.org",
            "projects/ores.web/packages/bff/src/pricing-engines.ts",
            "projects/ores.web/packages/web/src/api/pricingEngines.ts",
        ),
        fix="State the rule in the setting's value set and run"
            " projects/ores.sql/utility/generate_value_sets.py, then commit the seed."
            " A per-pair column has no place here: the set states the rule once.",
    ),
    Check(
        id="convention-kind-terms",
        title="The convention kind terms match the mapper and the ORE terms",
        argv=("python3", "projects/ores.sql/utility/generate_convention_kind_terms.py", "--check"),
        classes=("sql", "cpp"),
        paths=(
            "projects/ores.sql/populate/refdata/refdata_convention_kind_terms_populate.sql",
            "projects/ores.sql/populate/refdata/refdata_convention_kinds_populate.sql",
            "projects/ores.sql/create/refdata/refdata_convention_terms_validate_create.sql",
            "projects/ores.sql/drop/refdata/refdata_drop.sql",
            "projects/ores.sql/drop/refdata/refdata_convention_terms_validate_drop.sql",
            "projects/ores.sql/utility/generate_convention_kind_terms.py",
            "projects/ores.sql/utility/convention_terms.json",
            "projects/ores.ore/core/src/domain/conventions_family_*.cpp",
            "projects/ores.ore/core/src/domain/conventions_fx_projection.cpp",
        ),
        fix="Run projects/ores.sql/utility/generate_convention_kind_terms.py and commit the populate script.",
    ),
    Check(
        id="curve-sections-wiring",
        title="The curve screen's column wiring is the projection's",
        argv=(
            "python3",
            "projects/ores.sql/utility/generate_curve_section_parameters.py",
            "--check",
        ),
        classes=("sql", "web", "cpp"),
        paths=(
            "projects/ores.web/packages/bff/src/curve-sections.generated.ts",
            "projects/ores.sql/utility/generate_curve_section_parameters.py",
            "projects/ores.ore/core/src/domain/curve_configuration_projection.cpp",
            "external/ore/xsd/curveconfig.xsd",
        ),
        fix="Run projects/ores.sql/utility/generate_curve_section_parameters.py"
            " and commit the module.",
    ),
    Check(
        id="shell-command-units",
        title="The shell composes each command unit once, under one name",
        argv=("python3", "build/scripts/check_shell_command_units.py", "projects/ores.shell"),
        classes=("cpp", "tooling"),
        paths=("projects/ores.shell/*",),
        fix="Drop the repeated registration. Two branches that both wire the same"
            " unit merge cleanly, and the shell then stops at start-up, which is"
            " also where a person provisions a tenant.",
    ),
    Check(
        id="curve-parameter-paths",
        title="Every curve parameter path is one the ORE schema declares",
        argv=("python3", "projects/ores.sql/utility/check_curve_parameter_paths.py"),
        classes=("sql", "modeling"),
        paths=(
            "projects/ores.sql/populate/reporting/reporting_ore_parameters_artefact_populate.sql",
            "projects/ores.sql/utility/check_curve_parameter_paths.py",
            "projects/ores.sql/utility/curve_schema_exemptions.json",
            "projects/ores.sql/utility/extract_curve_terms.py",
            "external/ore/xsd/curveconfig.xsd",
        ),
        fix="Fix the attribution in extract_curve_terms.py, or state why the ORE"
            " code reads the path where the schema omits it, in"
            " curve_schema_exemptions.json, with the ORE source line.",
    ),
    Check(
        id="ore-element-map",
        title="The ORE element map and its columns are the tree's",
        argv=("python3", "projects/ores.sql/utility/generate_ore_element_map.py",
              "--check"),
        classes=("sql", "cpp", "modeling"),
        paths=(
            "projects/ores.sql/populate/reporting/reporting_ore_parameter_vocabularies_populate.sql",
            "projects/ores.sql/populate/reporting/reporting_ore_element_map_columns_populate.sql",
            "projects/ores.sql/utility/generate_ore_element_map.py",
            "projects/ores.sql/utility/generate_curve_column_guards.py",
            "projects/ores.sql/utility/generate_convention_kind_terms.py",
            "projects/ores.sql/utility/generate_value_sets.py",
            "projects/ores.sql/utility/curve_value_sets.py",
            "projects/ores.sql/utility/convention_value_sets.py",
            "projects/ores.sql/utility/ore_parameter_vocabularies.json",
            "projects/ores.sql/utility/ore_schema_types.py",
            "projects/ores.sql/utility/curve_terms.json",
            "projects/ores.sql/utility/convention_terms.json",
            "projects/ores.sql/populate/reporting/reporting_ore_parameter_value_sets_populate.sql",
            "external/ore/xsd/curveconfig.xsd",
            "external/ore/xsd/conventions.xsd",
        ),
        fix="Run projects/ores.sql/utility/generate_ore_element_map.py and commit"
            " the map and the column seed.",
    ),
    Check(
        id="curve-column-guards",
        title="The curve column map and its guard are the projection's",
        argv=(
            "python3",
            "projects/ores.sql/utility/generate_curve_column_guards.py",
            "--check",
        ),
        classes=("sql", "cpp", "modeling"),
        paths=(
            "projects/ores.sql/populate/refdata/refdata_curve_configuration_columns_populate.sql",
            "projects/ores.sql/create/refdata/refdata_curve_configuration_columns_validate_create.sql",
            "projects/ores.sql/drop/refdata/refdata_curve_configuration_columns_validate_drop.sql",
            "projects/ores.sql/utility/generate_curve_column_guards.py",
            "projects/ores.sql/utility/generate_curve_section_parameters.py",
            "projects/ores.ore/core/src/domain/curve_configuration_projection.cpp",
            "external/ore/xsd/curveconfig.xsd",
        ),
        fix="Run projects/ores.sql/utility/generate_curve_column_guards.py"
            " and commit the map, the guard and the drop.",
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
        id="portable-stop",
        title="No code outside ores.platform names a C++20 stop type",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_portable_stop_types.py"),
        classes=("cpp",),
        fix="Use the same name from ores::platform::concurrency (stoppable_thread.hpp):"
            " Apple's libc++ has no std::jthread or std::stop_token.",
    ),
    Check(
        id="event-registrars",
        title="Every generated event registrar is composed",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_event_registrar_composition.py"),
        classes=("services", "cpp", "modeling", "codegen"),
    ),
    # A history provider nothing registers answers every request with "No
    # history provider registered for entity_type". No build, check or screen
    # catches that: the generated file compiles and the service serves.
    Check(
        id="history-registrars",
        title="Every generated history-provider registrar is composed",
        argv=(CODEGEN_PY, "projects/ores.codegen/scripts/check_history_registrar_composition.py"),
        classes=("services", "cpp", "modeling", "codegen"),
    ),
    # A durable subject with no stream times out at the publisher, and two
    # streams over one subject stop the second service starting. The server
    # refuses both, so no running broker can show them; the check reads the
    # declarations the services send at start. It needs no broker.
    Check(
        id="stream-coverage",
        title="Every durable subject has one stream, and no two streams overlap",
        argv=("python3", "build/scripts/check_stream_coverage.py"),
        classes=("cpp", "services"),
        paths=(
            "projects/*/*/src/*",
            "projects/*/*/include/*",
            "build/scripts/check_stream_coverage.py",
        ),
        fix="Ensure the stream that covers the subject, from the routes table "
            "the service reads (serve_step_commands does both), or move the "
            "subject to one owner. A stream's filter must cover every subject "
            "its service consumes and must not overlap another stream's.",
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
        id="bff-list-requests",
        title="Every hand-written list request states what the protocol decodes",
        argv=(
            CODEGEN_PY,
            "projects/ores.codegen/scripts/check_bff_list_requests.py",
        ),
        classes=("web", "modeling", "codegen"),
        fix="State the member the report names, or pass filter: null.",
    ),
    Check(
        id="backlog-indexes",
        title="The backlog bucket indexes match the captures",
        argv=(
            "bash",
            "-c",
            "python3 projects/ores.codegen/scripts/regenerate_backlog_indexes.py"
            " && git diff --exit-code -- doc/agile/product_backlog",
        ),
        classes=("docs",),
        paths=("doc/agile/product_backlog/*",),
        phase=PREPARE,
        fix="Promote or file the capture with compass (compass capture promote,"
            " compass capture file), which regenerates the indexes, or run"
            " projects/ores.codegen/scripts/regenerate_backlog_indexes.py and"
            " commit the result.",
    ),
    Check(
        id="ore-output-catalogue",
        title="The output catalogue matches the ORE samples",
        argv=("python3", "projects/ores.ore/scripts/generate_output_catalogue.py",
              "--check"),
        classes=("cpp", "tooling"),
        paths=(
            "external/ore/*",
            "projects/ores.ore/scripts/generate_output_catalogue.py",
            "projects/ores.ore/core/include/ores.ore.core/results/output_catalogue.hpp",
            "projects/ores.ore/core/src/results/output_catalogue_data.*",
        ),
        fix="Run projects/ores.ore/scripts/generate_output_catalogue.py and commit"
            " the data it writes, or state in the generator why the sample is not"
            " a family.",
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
        # The rebuild terminates every connection and then counts what is left,
        # so a fleet that is still up reconnects inside the half second it waits
        # and the count refuses the rebuild. Stopping the fleet is what makes
        # the check a statement about the schema rather than about timing.
        argv=(
            "bash",
            "-c",
            "./compass.sh services stop || true; ./compass.sh db recreate -y -k",
        ),
        classes=("cpp", "services", "sql"),
        phase=HEAVY,
        timeout=7200,
    ),
    # The marketdata and synthetic suites share the bus (and several subjects)
    # with the services of the same name, so a live service steals the test's
    # own messages. Stop those two before the suites; the smoke starts the fleet
    # again afterwards.
    Check(
        id="ctest",
        title="Every test suite passes",
        argv=(
            "bash",
            "-c",
            "./compass.sh services stop ores.marketdata.service || true;"
            " ./compass.sh services stop ores.synthetic.service || true;"
            " ./compass.sh test run --preset {preset}",
        ),
        classes=("cpp", "services", "sql"),
        phase=HEAVY,
        after=("db",),
        timeout=7200,
    ),
    # A fresh database, the services, a provisioned tenant and a tick: the one
    # path no unit suite runs. It recreates the database itself, so it follows
    # the suite rather than sharing the database the suite used.
    Check(
        id="ticks-smoke",
        title="A provisioned tenant ticks on a fresh database",
        argv=("./compass.sh", "smoke", "ticks", "--fresh"),
        classes=("cpp", "services", "sql"),
        phase=HEAVY,
        after=("ctest",),
        timeout=2400,
        fix="Read the evidence the check prints: the first reason names the stage that failed.",
    ),
    # The corpus test writes the documents it read back to the build tree,
    # and this reads that output, so it follows the suite that produced it.
    Check(
        id="ore-xml-roundtrip",
        title="The ORE XML written back from the corpus still validates",
        argv=("bash", "build/scripts/check_ore_xml_roundtrip.sh"),
        classes=("cpp",),
        paths=("projects/ores.ore/*", "external/ore/xsd/*", "external/ore/examples/*",
               "build/scripts/check_ore_xml_roundtrip.sh"),
        phase=HEAVY,
        after=("ctest",),
        timeout=900,
        fix="Read the [ore][xml][corpus] test's first failure, then validate one "
            "written document by hand with xmllint --schema external/ore/xsd/input.xsd.",
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
        after=("db",),
        timeout=1800,
        fix="Read the suite's plan line and the first ERROR it printed.",
    ),
    # The one check whose subject is a server-side object rather than a file.
    # A durable consumer is declared at runtime, so a service that adds a second
    # one per subject produces the same C++ as a service that follows the
    # contract, and no read of the tree can tell them apart. It is therefore in
    # the heavy phase, where the fleet that declares the streams is expected to
    # be up, and it fails rather than skips when it cannot reach the server: a
    # check that answers "nothing to do" when it cannot see what it checks is
    # one that silently stops checking. `--exclude-heavy` keeps it out of the
    # ordinary run, so what that run pays today is unchanged.
    Check(
        id="step-consumers",
        title="Every step stream carries one consumer, and the claim's own numbers",
        argv=(
            "bash",
            "-c",
            "./compass.sh services start; python3 build/scripts/check_step_consumers.py",
        ),
        classes=("cpp", "services"),
        phase=HEAVY,
        # It starts the fleet, and the smoke before it does the same on a fresh
        # database. Two starts beside each other race.
        after=("ticks-smoke",),
        timeout=300,
        fix="Serve the family with one step_routes table and serve_step_commands,"
            " so the stream carries one durable named '<stream>_consumer'. If it"
            " is the ack wait or the delivery bound that moved, step_claim_lease"
            " is written against them: hold off x (deliveries - 1) must exceed"
            " the lease.",
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
