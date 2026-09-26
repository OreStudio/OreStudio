"""Tests for the opt-in first-generation scaffold facet.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_scaffold_is_opt_in.py

A component needs four placeholder files so its CMake targets link and its
test runner passes before any real model exists: the umbrella header, a
stub domain type, its implementation and a stub Catch2 test. They used to
hang off ``ores.cpp.component`` and ``ores.cpp.service-app``, keyed on
``#+component_kind:`` alone, so *any* non-composite component re-emitted
them whenever it was regenerated -- a mature part with hundreds of files,
and any component newly listed in the codegen catalogue. The umbrella
header came back the same way for components that had deleted it as dead
code.

They now live in their own facet, ``ores.cpp.scaffold``, which is
``#+default: disabled``: a component opts in with
``:ores.cpp.scaffold.enabled: true`` while it is being scaffolded and
removes the line with the files.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects" / "ores.codegen"

FLAT_ORG = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000cd
:END:
#+title: ores.demo
#+type: ores.codegen.component
#+name: demo
#+full_name: ores.demo
#+brief: demo fixture
{kind_line}
* Summary

Fixture body.
"""

SERVICE_ORG = FLAT_ORG.replace("#+name: demo", "#+name: demo.service") \
                      .replace("#+full_name: ores.demo", "#+full_name: ores.demo.service")

SCAFFOLD_OUTPUT_SUFFIXES = (
    "domain/stub.hpp",
    "src/domain/stub.cpp",
    "tests/stub_tests.cpp",
)

# The umbrella header is the scaffold's own: include/<component>/<component>.hpp.
# export.hpp is the one header every component keeps, and a service's
# application headers are not scaffolding.
def _is_scaffold_path(output: str) -> bool:
    if output.endswith(SCAFFOLD_OUTPUT_SUFFIXES):
        return True
    parts = output.split("/")
    if "include" not in parts:
        return False
    i = parts.index("include")
    return len(parts) == i + 3 and parts[i + 2] == f"{parts[i + 1]}.hpp"


def _write(path: Path, body: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")


def _units(tmp_path: Path, body: str, *, enabled: bool = False,
           via_drawer: bool = False, address: str = "ores.cpp"):
    """The outputs a component generates at ADDRESS, opted in or not.

    The address is the technical space rather than one facet: the scaffold
    is a sibling of ores.cpp.component, so a facet-scoped address would
    never reach it however the model is configured.

    ``via_drawer`` writes the opt-in into the model's own :PROPERTIES:
    drawer and reads it back, which is how a real component opts in;
    ``enabled`` injects the override directly, which tests the resolver.
    """
    model = tmp_path / "projects" / "ores.demo" / "modeling" / "component_overview.org"
    if via_drawer:
        body = body.replace(":PROPERTIES:\n:ID:",
                            ":PROPERTIES:\n:ores.cpp.scaffold.enabled: true\n:ID:", 1)
    _write(model, body)
    # None lets the resolver read the model's own drawer; a dict overrides it.
    properties = {"ores.cpp.scaffold.enabled": "true"} if enabled else None
    units, _, _ = resolve_targets(
        model, CODEGEN_BASE, address=address, properties=properties)
    return [u["output"] for u in units]


def _scaffold_paths(outputs):
    return [o for o in outputs if _is_scaffold_path(o)]


def test_flat_component_emits_no_scaffold_files_by_default(tmp_path):
    outputs = _units(tmp_path, FLAT_ORG.format(kind_line="#+component_kind: flat"))

    assert _scaffold_paths(outputs) == []


def test_component_without_a_kind_emits_no_scaffold_files_by_default(tmp_path):
    outputs = _units(tmp_path, FLAT_ORG.format(kind_line=""))

    assert _scaffold_paths(outputs) == []


def test_service_component_emits_no_stub_test_by_default(tmp_path):
    outputs = _units(tmp_path, SERVICE_ORG.format(kind_line="#+component_kind: service"))

    assert _scaffold_paths(outputs) == []


def test_the_scaffold_still_keeps_export_and_test_main(tmp_path):
    """Only the scaffolding is opt-in; the library still needs its two files."""
    outputs = _units(tmp_path, FLAT_ORG.format(kind_line="#+component_kind: flat"))

    assert [o for o in outputs if o.endswith(("export.hpp", "tests/main.cpp"))] != []


def test_opting_in_emits_the_whole_scaffold(tmp_path):
    outputs = _units(
        tmp_path, FLAT_ORG.format(kind_line="#+component_kind: flat"), enabled=True)

    assert sorted(_scaffold_paths(outputs)) == sorted(
        "projects/ores.demo/include/ores.demo/domain/stub.hpp "
        "projects/ores.demo/src/domain/stub.cpp "
        "projects/ores.demo/tests/stub_tests.cpp "
        "projects/ores.demo/include/ores.demo/ores.demo.hpp".split())


def test_the_drawer_line_is_what_opts_a_component_in(tmp_path):
    """The documented opt-in is the :PROPERTIES: line, read back from the file."""
    outputs = _units(
        tmp_path, FLAT_ORG.format(kind_line="#+component_kind: flat"), via_drawer=True)

    assert sorted(_scaffold_paths(outputs)) == sorted(
        "projects/ores.demo/include/ores.demo/domain/stub.hpp "
        "projects/ores.demo/src/domain/stub.cpp "
        "projects/ores.demo/tests/stub_tests.cpp "
        "projects/ores.demo/include/ores.demo/ores.demo.hpp".split())


def test_the_service_umbrella_header_is_scaffolding_too(tmp_path):
    outputs = _units(
        tmp_path, SERVICE_ORG.format(kind_line="#+component_kind: service"), enabled=True)

    assert [o for o in outputs if o.endswith("ores.demo.service.hpp")] != []


def test_opting_in_emits_the_service_stub_test(tmp_path):
    outputs = _units(
        tmp_path, SERVICE_ORG.format(kind_line="#+component_kind: service"), enabled=True)

    assert [o for o in outputs if o.endswith("tests/stub_tests.cpp")] != []


def test_a_facet_scoped_address_never_reaches_the_scaffold(tmp_path):
    """The scaffold is its own facet, so ores.cpp.component cannot emit it."""
    outputs = _units(
        tmp_path, FLAT_ORG.format(kind_line="#+component_kind: flat"),
        enabled=True, address="ores.cpp.component")

    assert _scaffold_paths(outputs) == []


def test_no_stub_file_survives_anywhere_in_the_tree():
    """The files this facet emits are scaffolding, and none is checked in."""
    found = sorted(
        str(p.relative_to(REPO_ROOT))
        for pattern in ("**/domain/stub.hpp", "**/domain/stub.cpp", "**/stub_tests.cpp")
        for p in (REPO_ROOT / "projects").glob(pattern)
    )

    assert found == []
