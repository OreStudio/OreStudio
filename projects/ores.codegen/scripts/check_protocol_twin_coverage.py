#!/usr/bin/env python3
"""
Check that every generated protocol header has its TypeScript twin, and
that no model opts the TypeScript protocol facet out.

Two rules guard the roll-out of the ``ores.ts.protocol`` facet, which
renders one TypeScript module per NATS protocol from the same model that
renders the C++ protocol header. When the two sides drift apart a UI
reads a payload the service does not send, and no other gate sees it,
because both files are generated artefacts of a model that is itself
valid.

  1. Every protocol header has a TypeScript twin. For every component
     whose TypeScript protocol output is committed under
     ``projects/ores.web/packages/wire-protocol/src/generated/<component>/protocol/``,
     each generated header
     ``projects/<project>/api/include/*/messaging/*_protocol.hpp`` must
     have its ``*_protocol.ts`` twin in that directory.

  2. No model opts out. No ``.org`` model under a ``projects/*/modeling/``
     directory may carry an ``:ores.ts.protocol.enabled:`` key. The key
     silences the facet for a model whose fields reference a type with no
     TypeScript projection, so allowing it anywhere lets a header ship
     without its twin.

The TypeScript projection is mid-roll-out, one component at a time, so
rule 1 is scoped by committed output: a component joins the check the
moment its first twin lands, exactly as it joins the TypeScript side of
check_component_drift.py. Rule 2 is global on purpose: no model may take
the shortcut anywhere.

The check reads the tree and writes nothing.

Usage:
  check_protocol_twin_coverage.py
"""
from __future__ import annotations

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
CODEGEN_DIR = REPO_ROOT / "projects" / "ores.codegen"
sys.path.insert(0, str(CODEGEN_DIR / "src"))
sys.path.insert(0, str(Path(__file__).resolve().parent))

from codegen.manifest import all_components, get_component  # noqa: E402
from codegen.physical_space import load_graph  # noqa: E402
from check_component_drift import _component_dir_names  # noqa: E402

TEMPLATES_DIR = CODEGEN_DIR / "library" / "templates"

# The technical space whose protocol output this check pairs with the C++.
TS_SPACE = "ores.ts"

# The file-name suffix of a TypeScript protocol twin, as the facet's
# archetype declares it in its ``#+output:`` pattern.
PROTOCOL_TS_SUFFIX = "_protocol.ts"

# The generated C++ protocol header, relative to a component's project
# directory.
PROTOCOL_HEADER_GLOB = "api/include/*/messaging/*_protocol.hpp"

# The drawer key that switches the TypeScript protocol facet off for one
# model.
OPT_OUT_KEY = ":ores.ts.protocol.enabled:"


def _rel(path: Path) -> str:
    """``path`` relative to REPO_ROOT, or its string form when outside it."""
    try:
        return str(path.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def _protocol_output_patterns(graph) -> list:
    """Every ``ores.ts`` archetype output pattern that renders a twin."""
    patterns = []
    for facet, archetypes in sorted(graph.facet_archetypes.items()):
        if not facet.startswith(f"{TS_SPACE}."):
            continue
        for archetype in archetypes:
            pattern = archetype.get("output") or ""
            if pattern.endswith(PROTOCOL_TS_SUFFIX) and "{component}" in pattern:
                patterns.append(pattern)
    return patterns


def _committed_protocol_dirs(component: str, graph) -> list:
    """Committed TypeScript protocol directories for one component.

    A directory counts only when it exists and holds a file, the same
    rule ``check_component_drift._committed_ts_facets`` uses to decide
    that a component has joined the TypeScript side of the projection.
    The catalogue name is translated through
    ``check_component_drift._component_dir_names`` because a ``-cpp``
    component generates into the directory its models name.
    """
    committed = []
    for pattern in _protocol_output_patterns(graph):
        for name in _component_dir_names(component):
            directory = pattern.replace("{component}", name).rsplit("/", 1)[0]
            path = REPO_ROOT / directory
            if path.is_dir() and any(path.iterdir()) and path not in committed:
                committed.append(path)
    return committed


def _committed_components(graph) -> list:
    """Triples ``(component, ts_dir, project_dir)`` for the checked components.

    A component is checked when one of its model directory names has
    committed TypeScript protocol output, so the pair is de-duplicated:
    the ``iam`` and ``iam-cpp`` catalogue entries generate into the same
    directories.
    """
    checked = []
    seen = set()
    for component in all_components():
        comp = get_component(component)
        modeling_dir = getattr(comp, "modeling_dir", None)
        if not modeling_dir:
            continue
        project_dir = (REPO_ROOT / modeling_dir).parent
        for ts_dir in _committed_protocol_dirs(component, graph):
            key = (ts_dir, project_dir)
            if key in seen:
                continue
            seen.add(key)
            checked.append((component, ts_dir, project_dir))
    return checked


def _checked_headers(committed: list) -> list:
    """Triples ``(component, header, expected_twin)`` for every covered header."""
    checked = []
    for component, ts_dir, project_dir in committed:
        for header in sorted(project_dir.glob(PROTOCOL_HEADER_GLOB)):
            checked.append((component, header, ts_dir / f"{header.stem}.ts"))
    return checked


def _modeling_org_files() -> list:
    """Every ``.org`` file under a ``projects/*/modeling/`` directory."""
    files = []
    for modeling in sorted((REPO_ROOT / "projects").glob("*/modeling")):
        if modeling.is_dir():
            files.extend(sorted(modeling.rglob("*.org")))
    return files


def _opt_out_hits() -> list:
    """Triples ``(path, line_number, line)`` for every model carrying the key."""
    hits = []
    for path in _modeling_org_files():
        text = path.read_text(encoding="utf-8", errors="replace")
        for lineno, line in enumerate(text.splitlines(), 1):
            if OPT_OUT_KEY in line:
                hits.append((path, lineno, line.strip()))
    return hits


def main() -> int:
    graph = load_graph(TEMPLATES_DIR)
    committed = _committed_components(graph)
    checked = _checked_headers(committed)
    missing = [(component, header, twin)
               for component, header, twin in checked if not twin.is_file()]
    opt_outs = _opt_out_hits()

    failures = 0
    if missing:
        print("Protocol header(s) with no TypeScript twin:", file=sys.stderr)
        for component, header, twin in missing:
            print(f"  {_rel(header)}: expected {_rel(twin)} "
                  f"(component {component!r})", file=sys.stderr)
        failures += 1
    if opt_outs:
        print("Model(s) that opt out of the TypeScript protocol facet:",
              file=sys.stderr)
        for path, lineno, line in opt_outs:
            print(f"  {_rel(path)}:{lineno}: {line}", file=sys.stderr)
        failures += 1

    if failures:
        print("\nProtocol twin coverage is incomplete. Give every protocol "
              "header a TypeScript twin, and remove every "
              f"{OPT_OUT_KEY} key so no model silences the facet.",
              file=sys.stderr)
        return 1

    print(f"Protocol twin coverage intact: all {len(checked)} generated "
          f"protocol header(s) in {len(committed)} committed component(s) "
          "have a TypeScript twin, and no model opts out.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
