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

# A ``#+default:`` of these strings means the node is off unless explicitly
# enabled (e.g. ores.sql.populate, where each dataset opts in to its archetypes).
_DISABLED_WORDS = ("disabled", "off", "false", "no", "0")


def _parse_default(value: str | None, fallback: bool) -> bool:
    """Resolve a ``#+default:`` keyword to a boolean (fallback if unset)."""
    if value is None or value.strip() == "":
        return fallback
    return value.strip().lower() not in _DISABLED_WORDS


class Graph:
    """The loaded physical-space graph."""

    def __init__(self) -> None:
        self.ts_facets: dict[str, list[str]] = {}        # ts addr -> [facet addr]
        self.facet_ts: dict[str, str] = {}               # facet addr -> ts addr
        self.facet_model_types: dict[str, list[str]] = {}
        self.facet_default: dict[str, bool] = {}         # facet addr -> default enabled
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
            g.facet_default[addr] = _parse_default(kw.get("default"), True)
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
                # component-kind discriminator (empty => serves every kind);
                # _default_raw is resolved against the facet default below.
                "kinds": kw.get("component_kind", "").split(),
                # data-scope: the dataset-relative payload file this archetype
                # renders from (e.g. country_currency.json); empty for entity/
                # component archetypes whose context is the model itself.
                "data_source": kw.get("data_source", ""),
                "_default_raw": kw.get("default"),
            })
    # Second pass: resolve each archetype's default-enabled against its facet's
    # default (nodes can load in any filename order, so do this after the loop).
    for facet, archs in g.facet_archetypes.items():
        facet_default = g.facet_default.get(facet, True)
        for arch in archs:
            arch["default_enabled"] = _parse_default(
                arch.pop("_default_raw", None), facet_default)
    return g


def _enabled_overrides(properties: dict[str, Any]) -> dict[str, bool]:
    """Extract ``ores.*.enabled`` flags from an entity's :PROPERTIES: drawer."""
    out: dict[str, bool] = {}
    for k, v in properties.items():
        m = re.match(r"^(ores(?:\.[\w-]+)*)\.enabled$", k.strip())
        if m:
            out[m.group(1)] = str(v).strip().lower() in ("true", "t", "yes", "1")
    return out


def is_enabled_with_reason(
    address: str, facet: str, ts: str, overrides: dict[str, bool],
    default_enabled: bool = True,
) -> tuple[bool, str | None]:
    """Like :func:`is_enabled`, but also names which key decided it.

    Returns ``(enabled, reason_key)`` — ``reason_key`` is the ``ores.*``
    address whose ``:enabled:`` override matched (most-specific first), or
    ``None`` if the result came from ``default_enabled`` (the node's own
    ``#+default:``) rather than an explicit override.
    """
    for key in (address, facet, ts, "ores"):
        if key and key in overrides:
            return overrides[key], key
    return default_enabled, None


def is_enabled(address: str, facet: str, ts: str,
               overrides: dict[str, bool], default_enabled: bool = True) -> bool:
    """Most-specific-wins resolution of ``ores.*.enabled`` for any node.

    ``address`` is the node being resolved (an archetype or a facet); the
    lookup walks from it up through its facet, technical space, and root,
    returning the first explicit override, else ``default_enabled``. Passing
    ``address == facet`` resolves a facet (the B5 facet-level behaviour).
    """
    return is_enabled_with_reason(address, facet, ts, overrides, default_enabled)[0]


def _is_enabled(facet: str, ts: str, overrides: dict[str, bool]) -> bool:
    """Facet-level resolution (back-compat shim over :func:`is_enabled`)."""
    return is_enabled(facet, facet, ts, overrides, True)


def kind_matches(archetype_kinds: list[str], model_kind: str | None) -> bool:
    """Component-kind filter: an archetype with no declared kinds serves every
    kind; otherwise the model's kind must be one it declares.

    Non-component archetypes declare no kinds and non-component models pass
    ``model_kind=None``, so this is a no-op outside component generation.
    """
    if not archetype_kinds:
        return True
    return model_kind in archetype_kinds


def compute_supported_set(properties: dict[str, Any], graph: Graph,
                          model_type: str) -> frozenset[str]:
    """Facets entity *e* can generate: model-type-admissible, minus disables.

    A facet whose ``#+default:`` is disabled is excluded unless an ``ores.*``
    override re-enables it (opt-in facets, e.g. ores.sql.populate)."""
    overrides = _enabled_overrides(properties or {})
    supported = set()
    for facet, mts in graph.facet_model_types.items():
        if mts and model_type not in mts:
            continue
        ts = graph.facet_ts.get(facet, "")
        default = graph.facet_default.get(facet, True)
        if is_enabled(facet, facet, ts, overrides, default):
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
