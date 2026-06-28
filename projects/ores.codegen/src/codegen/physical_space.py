"""Physical-space address graph: load it and resolve what an entity generates.

The codegen physical space is a graph of org documents under
``library/templates/``, one per node, each named for its MASD address:

    ores                          (root)
      ores.cpp                    (technical space)
        ores.cpp.qt               (facet)
          ores.cpp.qt.controller_header   (archetype, a leaf)

This module loads that graph from the doc frontmatter and computes, for a
given entity and an optional ``--address`` filter, the set of archetypes to
generate:

    generation set = supported set (S_e) ∩ target set (T)

* Supported set (S_e) — what entity *e* can generate. Default: every facet
  whose ``#+model_types`` admits the entity's model type. Narrowed by the
  entity's ``:ores.*.enabled:`` drawer properties, resolved most-specific-wins.
* Target set (T) — what to generate this run. Default: all of S_e. An
  ``--address`` argument restricts T to the facets under that address.

The node name *is* its address, so addresses, ``--address`` values and
``ores.*`` drawer keys are all the same strings — no lookup table.
"""
from __future__ import annotations

import re
from pathlib import Path
from typing import Any

_KW_RE = re.compile(r"^#\+(\w[\w-]*):\s*(.*)$")
_TANGLE_RE = re.compile(r":tangle\s+(\S+\.mustache)")


class Graph:
    """The loaded physical-space graph."""

    def __init__(self) -> None:
        self.ts_facets: dict[str, list[str]] = {}        # ts addr -> [facet addr]
        self.facet_ts: dict[str, str] = {}               # facet addr -> ts addr
        self.facet_model_types: dict[str, list[str]] = {}
        self.facet_archetypes: dict[str, list[dict]] = {}  # facet addr -> [archetype]

    @property
    def facets(self) -> list[str]:
        return list(self.facet_archetypes)

    def facets_under(self, address: str) -> set[str]:
        """Every facet whose address is, or is nested under, ``address``."""
        # Root first: ores.org is typed technical_space, so "ores" is also a
        # key in ts_facets (with an empty list). Checking it here keeps the
        # root expansion correct regardless of how ts_facets is populated.
        if address == "ores":
            return set(self.facet_archetypes)
        if address in self.facet_archetypes:
            return {address}
        if address in self.ts_facets:
            return set(self.ts_facets[address])
        raise ValueError(f"unknown address: {address!r}")


def load_graph(templates_dir: Path) -> Graph:
    """Build the graph from the ``ores.*`` node docs in ``templates_dir``."""
    g = Graph()
    for path in sorted(Path(templates_dir).glob("ores.*.org")):
        text = path.read_text(encoding="utf-8")
        kw: dict[str, str] = {}
        for line in text.splitlines():
            m = _KW_RE.match(line)
            if m:
                kw[m.group(1).lower()] = m.group(2).strip()
            elif line.startswith("* "):
                break
        addr = kw.get("title", path.stem)
        typ = kw.get("type", "")
        if typ == "technical_space":
            g.ts_facets.setdefault(addr, [])
        elif typ == "facet":
            ts = kw.get("facet_group", "")
            g.facet_ts[addr] = ts
            g.ts_facets.setdefault(ts, []).append(addr)
            g.facet_model_types[addr] = kw.get("model_types", "").split()
            g.facet_archetypes.setdefault(addr, [])
        elif typ == "archetype":
            facet = kw.get("facet", "")
            tm = _TANGLE_RE.search(text)
            g.facet_archetypes.setdefault(facet, []).append({
                "address": addr,
                "template": tm.group(1) if tm else "",
                "output": kw.get("output", ""),
                "model_types": kw.get("model_types", "").split()
                or g.facet_model_types.get(facet, []),
            })
    return g


def _enabled_overrides(properties: dict[str, Any]) -> dict[str, bool]:
    """Extract ``ores.*.enabled`` flags from an entity's :PROPERTIES: drawer."""
    out: dict[str, bool] = {}
    for k, v in properties.items():
        m = re.match(r"^(ores(?:\.[\w-]+)*)\.enabled$", k.strip())
        if m:
            out[m.group(1)] = str(v).strip().lower() in ("true", "t", "yes", "1")
    return out


def _is_enabled(facet: str, ts: str, overrides: dict[str, bool]) -> bool:
    """Most-specific-wins resolution of ``ores.*.enabled`` for one facet."""
    if facet in overrides:        # facet-level (e.g. ores.cpp.qt)
        return overrides[facet]
    if ts in overrides:           # technical-space-level (e.g. ores.cpp)
        return overrides[ts]
    if "ores" in overrides:       # root-level
        return overrides["ores"]
    return True                   # default: enabled


def compute_supported_set(properties: dict[str, Any], graph: Graph,
                          model_type: str) -> frozenset[str]:
    """Facets entity *e* can generate: model-type-admissible, minus disables."""
    overrides = _enabled_overrides(properties or {})
    supported = set()
    for facet, mts in graph.facet_model_types.items():
        if mts and model_type not in mts:
            continue
        if _is_enabled(facet, graph.facet_ts.get(facet, ""), overrides):
            supported.add(facet)
    return frozenset(supported)


def compute_target_set(address: str | None, graph: Graph) -> frozenset[str]:
    """Facets selected by an ``--address`` (None ⇒ everything)."""
    if not address:
        return frozenset(graph.facet_archetypes)
    return frozenset(graph.facets_under(address))  # raises on unknown address


def resolve_generation_set(supported: frozenset[str],
                           target: frozenset[str]) -> frozenset[str]:
    """What actually generates = supported ∩ target."""
    return supported & target
