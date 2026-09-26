"""Verify that codegen reads sample component overviews correctly and
produces the expected scaffold shape.

Targets:

- The 2 sample fixtures next to this script:
  - ``sample_flat/component_overview.org`` (flat component)
  - ``sample_composite/component_overview.org`` (group level)
  - ``sample_composite/api/component_overview.org`` (composite .api)
  - ``sample_composite/core/component_overview.org`` (composite .core)

- *Prior art* under ``projects/``:
  - ``projects/ores.nats/modeling/component_overview.org`` — a real flat
    component already in the tree.
  - ``projects/ores.dq/{,api,core,service}/modeling/component_overview.org``
    — a real composite group + three subcomponents.

For each target we check:

1. ``load_org_component_overview_model`` returns the expected
   ``{component: {name, full_name, brief, description}}`` shape.
2. ``resolve_targets`` at ``ores.cpp`` emits exactly the C++ files the
   overview asks for. A component gets the export macros and the test main
   always, and the first-generation scaffold (the umbrella header, the stub
   header and impl, and the stub test) only where its drawer carries
   ``:ores.cpp.scaffold.enabled: true``. The scaffold facet is
   ``#+default: disabled``, so a component that does not opt in never has
   those files recreated, however often it is regenerated. A composite
   group root serves no archetype and emits nothing.

The CMake files are a different address (``ores.cmake.component``) and are
not checked here.

Run::

    python3 projects/ores.codegen/tests/component_scaffold/verify.py

Exits 0 on success; non-zero on any check failure.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402
from codegen.org_loader import load_org_component_overview_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects" / "ores.codegen"

# What ores.cpp emits for every non-composite component.
ALWAYS_OUTPUT_SUFFIXES = (
    "include/{full_name}/export.hpp",
    "tests/main.cpp",
)

# Emitted only where the overview opts in with
# :ores.cpp.scaffold.enabled: true.
SCAFFOLD_OPT_IN_SUFFIXES = (
    "include/{full_name}/{full_name}.hpp",
    "include/{full_name}/domain/stub.hpp",
    "src/domain/stub.cpp",
    "tests/stub_tests.cpp",
)


def _check_loader(label: str, path: Path, required_keys=("name", "full_name", "brief", "description")):
    if not path.is_file():
        return f"FAIL {label}: missing {path}"
    model = load_org_component_overview_model(path)["component"]
    missing = [k for k in required_keys if k not in model]
    if missing:
        return f"FAIL {label}: loader returned no {missing}; got {sorted(model.keys())}"
    brief = model.get("brief") or ""
    return f"OK   {label}: {model['full_name']} (brief={brief[:50]}...)"


def _opted_in(ov: Path) -> bool:
    """Whether the overview's drawer asks for the first-generation scaffold."""
    text = ov.read_text(encoding="utf-8")
    return bool(re.search(r"^:ores\.cpp\.scaffold\.enabled:\s*true\s*$", text, re.M))


def _expected(ov: Path) -> set[str]:
    """The component-relative outputs the overview asks for."""
    full = load_org_component_overview_model(ov)["component"]["full_name"]
    suffixes = ALWAYS_OUTPUT_SUFFIXES
    if _opted_in(ov):
        suffixes += SCAFFOLD_OPT_IN_SUFFIXES
    return {s.format(full_name=full) for s in suffixes}


def _relative(ov: Path, outputs) -> set[str]:
    """Emitted paths are component-root-relative; the resolver states them
    from the repository root."""
    root = ov.parent.parent.relative_to(REPO_ROOT)
    prefix = f"{root}/"
    return {o[len(prefix):] if o.startswith(prefix) else o for o in outputs}


def _check_emitted(label: str, ov: Path):
    """The resolver emits exactly the files the overview asks for, no more."""
    try:
        units, model_type, _ = resolve_targets(ov, CODEGEN_BASE, address="ores.cpp")
    except ValueError as e:
        return f"FAIL {label}: {e}"
    if model_type != "component":
        return f"FAIL {label}: model type is {model_type!r}, not 'component'"

    emitted = _relative(ov, {u["output"] for u in units})
    expected = set() if _is_composite(ov) else _expected(ov)
    missing = sorted(expected - emitted)
    extra = sorted(emitted - expected)
    if missing or extra:
        detail = []
        if missing:
            detail.append(f"missing {missing}")
        if extra:
            detail.append(f"unexpected {extra}")
        return f"FAIL {label}: {'; '.join(detail)}"
    if not expected:
        return f"OK   {label}: composite root, no archetype serves it"
    mode = "opted into the scaffold" if _opted_in(ov) else "no scaffolding"
    return f"OK   {label}: {len(emitted)} file(s), {mode}"


def _is_composite(ov: Path) -> bool:
    """A group root declares #+component_kind: composite and holds no code."""
    text = ov.read_text(encoding="utf-8")
    return bool(re.search(r"^#\+component_kind:\s*composite\s*$", text, re.M))


def main() -> int:
    here = Path(__file__).parent
    fails = 0
    checks = [
        # Fixture: flat, opted in like a newly scaffolded component.
        ("fixture/flat", here / "sample_flat/component_overview.org"),
        # Fixture: composite group + two subs.
        ("fixture/composite-group", here / "sample_composite/component_overview.org"),
        ("fixture/composite-api", here / "sample_composite/api/component_overview.org"),
        ("fixture/composite-core", here / "sample_composite/core/component_overview.org"),
        # Prior art: a real flat component that has outgrown its scaffolding.
        ("prior-art/ores.nats", REPO_ROOT / "projects/ores.nats/modeling/component_overview.org"),
        # Prior art: composite group + three subs.
        ("prior-art/ores.dq (group)", REPO_ROOT / "projects/ores.dq/modeling/component_overview.org"),
        ("prior-art/ores.dq.api", REPO_ROOT / "projects/ores.dq/api/modeling/component_overview.org"),
        ("prior-art/ores.dq.core", REPO_ROOT / "projects/ores.dq/core/modeling/component_overview.org"),
        ("prior-art/ores.dq.service", REPO_ROOT / "projects/ores.dq/service/modeling/component_overview.org"),
    ]
    for label, path in checks:
        line = _check_loader(label, path)
        print(line)
        if line.startswith("FAIL"):
            fails += 1
            continue
        line2 = _check_emitted(label, path)
        print(line2)
        if line2.startswith("FAIL"):
            fails += 1
    print("---")
    print(f"checks failed: {fails}")
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
