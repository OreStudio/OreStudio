"""Which of the heavy pull-request checks a change actually needs.

The pull-request gate set has two halves. The first is fast and always runs:
the codegen suite, the drift jobs, and the Python gates over generated
output. The second is the one that compiles and runs the tree -- recreate the
database, configure, build every target, run ctest, and start the services
when the change can affect one -- and it is expensive, so it runs only when
the diff can break it.

This module is the decision. It is deliberately a pure function of the changed
paths, so a case can pin every class, and the workflow's own job is a thin
caller. A path nobody has classified before asks for everything: a change to
something this table does not know is a change this table cannot vouch for,
and the cost of the extra run is smaller than the cost of the break it hides.
"""

from __future__ import annotations

import fnmatch
from dataclasses import dataclass, field
from typing import Iterable


@dataclass(frozen=True)
class ChangeClassification:
    """The heavy checks a diff needs, and why.

    ``cpp`` covers the whole compile-and-test path: the database is recreated,
    the tree is configured and built, and ctest runs. The two are separate
    fields so a future change can skip one of them, but today a build without a
    database cannot run the suites, so ``cpp`` implies ``db``.
    """

    cpp: bool = False
    db: bool = False
    services: bool = False
    reasons: tuple[str, ...] = field(default_factory=tuple)

    @property
    def anything(self) -> bool:
        return self.cpp or self.db or self.services

    def as_github_outputs(self) -> dict[str, str]:
        return {
            "cpp": "true" if self.cpp else "false",
            "db": "true" if self.db else "false",
            "services": "true" if self.services else "false",
        }


# Paths that can change no compiled code and no schema. A change confined to
# these skips the heavy checks entirely.
_DOCUMENTATION = (
    "doc/*",
    "*.md",
    "*.MD",
    "LICENSE*",
    ".gitignore",
)

# A workflow or a generator-marker change alters how the tree is checked, not
# what it contains; the fast gates cover it and the next pull request proves it.
_META = (".github/*",)

# The web client is compiled by its own workflow, which the pull-request check
# set already runs.
_TYPESCRIPT = (
    "projects/ores.web/*",
    "*.ts",
    "*.tsx",
    "package.json",
    "pnpm-lock.yaml",
)

# The project's own tooling: Python, and the org corpus it reads.
_TOOLING = (
    "projects/ores.compass/*",
    "projects/ores.codegen/venv/*",
)

# Everything a C++ change is: sources, headers, and the build description.
_CPP = (
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
)

# Codegen and the models it reads emit C++ that has to compile.
_GENERATED_CPP = (
    "projects/ores.codegen/*",
    "projects/*/modeling/*",
)

# The schema, the populate scripts and the migrations.
_SQL = (
    "projects/ores.sql/*",
    "*.sql",
)

# A service's own sources, the launcher, and the files that describe what runs.
_SERVICES = (
    "projects/*/service/*",
    "projects/ores.service/*",
    "projects/ores.compass/src/compass_services.py",
    "build/config/*",
    "docker/*",
    "compose*.yml",
)


def _matches(path: str, patterns: Iterable[str]) -> bool:
    return any(fnmatch.fnmatch(path, pattern) for pattern in patterns)


def classify(paths: Iterable[str]) -> ChangeClassification:
    """The heavy checks the given changed paths need.

    ``paths`` are repository-relative, as ``git diff --name-only`` prints them.
    """
    cpp = False
    db = False
    services = False
    reasons: list[str] = []

    def note(reason: str) -> None:
        if reason not in reasons:
            reasons.append(reason)

    for path in paths:
        path = path.strip()
        if not path:
            continue

        if _matches(path, _DOCUMENTATION):
            note(f"{path}: documentation; no build, no database")
            continue
        if _matches(path, _TYPESCRIPT):
            note(f"{path}: web client; its own workflow covers it")
            continue
        if _matches(path, _META):
            note(f"{path}: check configuration; the fast gates cover it")
            continue
        if _matches(path, _SERVICES):
            note(f"{path}: a service can start differently; start them")
            services = True
            cpp = True
            continue
        if _matches(path, _SQL):
            note(f"{path}: schema, populate or migration; recreate the database")
            db = True
            cpp = True
            continue
        if _matches(path, _TOOLING):
            note(f"{path}: project tooling; the fast gates cover it")
            continue
        if _matches(path, _GENERATED_CPP):
            note(f"{path}: generated C++ changes with it; build and test")
            cpp = True
            continue
        if _matches(path, _CPP):
            note(f"{path}: compiled code or its build; build and test")
            cpp = True
            continue

        # Nothing above claims this path, so nothing above can vouch for it.
        note(f"{path}: unclassified; run everything")
        cpp = True
        db = True
        services = True

    if cpp:
        db = True

    return ChangeClassification(
        cpp=cpp, db=db, services=services, reasons=tuple(reasons)
    )
