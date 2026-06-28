"""Tests for the physical-space address graph resolver (codegen.physical_space).

Run::

    python3 -m pytest projects/ores.codegen/tests/test_physical_space.py

The supported/target/generation-set logic is exercised against a small
synthetic graph (stable, isolated from the real template library); a final
smoke test loads the live graph to confirm load_graph() parses the on-disk
ores.* nodes.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.physical_space import (  # noqa: E402
    Graph,
    _enabled_overrides,
    _parse_default,
    compute_supported_set,
    compute_target_set,
    is_enabled,
    kind_matches,
    load_graph,
    resolve_generation_set,
)


@pytest.fixture
def graph():
    """A two-technical-space synthetic graph.

    ores.cpp  → ores.cpp.domain (domain_entity), ores.cpp.qt (domain_entity)
    ores.sql  → ores.sql.schema (domain_entity, table)
    """
    g = Graph()
    g.ts_facets = {
        "ores.cpp": ["ores.cpp.domain", "ores.cpp.qt"],
        "ores.sql": ["ores.sql.schema"],
    }
    g.facet_ts = {
        "ores.cpp.domain": "ores.cpp",
        "ores.cpp.qt": "ores.cpp",
        "ores.sql.schema": "ores.sql",
    }
    g.facet_model_types = {
        "ores.cpp.domain": ["domain_entity"],
        "ores.cpp.qt": ["domain_entity"],
        "ores.sql.schema": ["domain_entity", "table"],
    }
    g.facet_archetypes = {
        "ores.cpp.domain": [{"address": "ores.cpp.domain.class_header",
                             "template": "d.mustache", "output": "d.hpp",
                             "model_types": ["domain_entity"]}],
        "ores.cpp.qt": [{"address": "ores.cpp.qt.controller_header",
                         "template": "q.mustache", "output": "q.hpp",
                         "model_types": ["domain_entity"]}],
        "ores.sql.schema": [{"address": "ores.sql.schema.create",
                             "template": "s.mustache", "output": "s.sql",
                             "model_types": ["domain_entity", "table"]}],
    }
    return g


# --- supported set (S_e) ---------------------------------------------------

def test_supported_default_is_all_model_type_admissible(graph):
    """No ores.* properties => every facet admitting the model type."""
    assert compute_supported_set({}, graph, "domain_entity") == frozenset(
        {"ores.cpp.domain", "ores.cpp.qt", "ores.sql.schema"})


def test_supported_filters_by_model_type(graph):
    """A table model only supports facets whose model_types include 'table'."""
    assert compute_supported_set({}, graph, "table") == frozenset({"ores.sql.schema"})


def test_disable_technical_space_removes_all_its_facets(graph):
    s = compute_supported_set({"ores.cpp.enabled": "false"}, graph, "domain_entity")
    assert s == frozenset({"ores.sql.schema"})
    assert not any(f.startswith("ores.cpp.") for f in s)


def test_disable_facet_removes_only_that_facet(graph):
    s = compute_supported_set({"ores.cpp.qt.enabled": "false"}, graph, "domain_entity")
    assert "ores.cpp.qt" not in s
    assert "ores.cpp.domain" in s


def test_specificity_facet_override_beats_technical_space(graph):
    """ores.cpp off + ores.cpp.qt on => qt kept, other cpp facets dropped."""
    s = compute_supported_set(
        {"ores.cpp.enabled": "false", "ores.cpp.qt.enabled": "true"},
        graph, "domain_entity")
    assert "ores.cpp.qt" in s
    assert "ores.cpp.domain" not in s


def test_root_disable_removes_everything(graph):
    assert compute_supported_set({"ores.enabled": "false"}, graph, "domain_entity") == frozenset()


def test_enabled_truthy_values(graph):
    """Only recognised truthy strings enable; anything else disables."""
    assert "ores.cpp.qt" in compute_supported_set({"ores.cpp.qt.enabled": "TRUE"}, graph, "domain_entity")
    assert "ores.cpp.qt" not in compute_supported_set({"ores.cpp.qt.enabled": "no"}, graph, "domain_entity")


# --- target set (T) --------------------------------------------------------

def test_target_none_is_all_facets(graph):
    assert compute_target_set(None, graph) == frozenset(graph.facet_archetypes)


def test_target_root_is_all_facets(graph):
    assert compute_target_set("ores", graph) == frozenset(graph.facet_archetypes)


def test_target_technical_space_expands_to_its_facets(graph):
    assert compute_target_set("ores.cpp", graph) == frozenset({"ores.cpp.domain", "ores.cpp.qt"})


def test_target_facet_is_itself(graph):
    assert compute_target_set("ores.cpp.qt", graph) == frozenset({"ores.cpp.qt"})


def test_target_unknown_address_raises(graph):
    with pytest.raises(ValueError):
        compute_target_set("ores.bogus", graph)


# --- generation set (S_e ∩ T) ---------------------------------------------

def test_generation_set_is_intersection(graph):
    supported = compute_supported_set({}, graph, "domain_entity")
    target = compute_target_set("ores.cpp.qt", graph)
    assert resolve_generation_set(supported, target) == frozenset({"ores.cpp.qt"})


def test_generation_set_empty_when_target_outside_supported(graph):
    """Target a technical space the entity has disabled => nothing to generate."""
    supported = compute_supported_set({"ores.cpp.enabled": "false"}, graph, "domain_entity")
    target = compute_target_set("ores.cpp", graph)
    assert resolve_generation_set(supported, target) == frozenset()


# --- layer (b) activation seam: archetype-granularity + kind + default-off -

def test_parse_default():
    assert _parse_default(None, True) is True
    assert _parse_default(None, False) is False
    assert _parse_default("", True) is True            # blank => fallback
    assert _parse_default("disabled", True) is False
    assert _parse_default("off", True) is False
    assert _parse_default("false", True) is False
    assert _parse_default("enabled", False) is True    # any non-disabled word
    assert _parse_default("  Disabled ", True) is False  # case/space-insensitive


def test_enabled_overrides_parses_dotted_and_hyphenated_keys():
    """Activation keys to archetype depth, incl. hyphenated facets, are read."""
    ov = _enabled_overrides({
        "ores.cpp.qt.enabled": "false",
        "ores.cpp.service-app.enabled": "true",
        "ores.sql.populate.country.enabled": "true",
        "ID": "ignore-me",
    })
    assert ov == {
        "ores.cpp.qt": False,
        "ores.cpp.service-app": True,
        "ores.sql.populate.country": True,
    }


def test_is_enabled_specificity_archetype_beats_facet():
    ov = {"ores.a.b.c": False, "ores.a.b": True}
    assert is_enabled("ores.a.b.c", "ores.a.b", "ores.a", ov) is False


def test_is_enabled_facet_beats_technical_space():
    ov = {"ores.a.b": True, "ores.a": False}
    assert is_enabled("ores.a.b", "ores.a.b", "ores.a", ov) is True


def test_is_enabled_falls_back_to_default():
    assert is_enabled("ores.a.b.c", "ores.a.b", "ores.a", {}, default_enabled=False) is False
    assert is_enabled("ores.a.b.c", "ores.a.b", "ores.a", {}, default_enabled=True) is True


def test_is_enabled_root_override():
    assert is_enabled("ores.a.b.c", "ores.a.b", "ores.a", {"ores": False}) is False


def test_kind_matches():
    assert kind_matches([], "service") is True          # no kinds => serves all
    assert kind_matches([], None) is True
    assert kind_matches(["service"], "service") is True
    assert kind_matches(["flat", "api"], "api") is True
    assert kind_matches(["service"], "flat") is False
    assert kind_matches(["service"], None) is False     # declared kind, none on model


def test_supported_set_excludes_default_off_facet(graph):
    """A default-off facet is not supported unless an override re-enables it."""
    graph.facet_default["ores.sql.schema"] = False
    s = compute_supported_set({}, graph, "domain_entity")
    assert "ores.sql.schema" not in s
    assert "ores.cpp.domain" in s


def test_default_off_facet_reenabled_by_override(graph):
    graph.facet_default["ores.sql.schema"] = False
    s = compute_supported_set({"ores.sql.schema.enabled": "true"}, graph, "domain_entity")
    assert "ores.sql.schema" in s


def _write(dirpath, name, body):
    (dirpath / name).write_text(body, encoding="utf-8")


def test_load_graph_parses_default_off_and_kinds(tmp_path):
    _write(tmp_path, "ores.org", "#+title: ores\n#+type: technical_space\n")
    _write(tmp_path, "ores.t.org", "#+title: ores.t\n#+type: technical_space\n")
    _write(tmp_path, "ores.t.f.org",
           "#+title: ores.t.f\n#+type: facet\n#+facet_group: ores.t\n"
           "#+model_types: data\n#+default: disabled\n")
    # archetype inherits the facet's default-off
    _write(tmp_path, "ores.t.f.a.org",
           "#+title: ores.t.f.a\n#+type: archetype\n#+facet: ores.t.f\n"
           "#+output: a.sql\n#+component_kind: service api\n"
           "* Template\n#+begin_src mustache :tangle a.mustache\nx\n#+end_src\n")
    # archetype overrides the facet default back on
    _write(tmp_path, "ores.t.f.b.org",
           "#+title: ores.t.f.b\n#+type: archetype\n#+facet: ores.t.f\n"
           "#+output: b.sql\n#+default: enabled\n"
           "* Template\n#+begin_src mustache :tangle b.mustache\nx\n#+end_src\n")
    g = load_graph(tmp_path)
    assert g.facet_default["ores.t.f"] is False
    archs = {a["address"]: a for a in g.facet_archetypes["ores.t.f"]}
    assert archs["ores.t.f.a"]["kinds"] == ["service", "api"]
    assert archs["ores.t.f.a"]["default_enabled"] is False   # inherits facet
    assert archs["ores.t.f.b"]["default_enabled"] is True     # own override
    assert "_default_raw" not in archs["ores.t.f.a"]          # cleaned up


# --- live graph smoke test -------------------------------------------------

def test_load_graph_parses_live_nodes():
    g = load_graph(REPO_ROOT / "projects/ores.codegen/library/templates")
    assert "ores.cpp" in g.ts_facets
    assert "ores.sql" in g.ts_facets
    assert g.ts_facets["ores.cpp"], "ores.cpp should list facets"
    assert g.facet_archetypes.get("ores.cpp.qt"), "ores.cpp.qt should own archetypes"
    # every archetype carries the data codegen needs
    for arch in g.facet_archetypes["ores.cpp.qt"]:
        assert arch["template"].endswith(".mustache")
        assert arch["output"]


def test_target_root_resolves_all_facets_on_live_graph():
    """Regression: ores.org is typed technical_space, so "ores" is also a
    ts_facets key with an empty list — facets_under must still expand the root
    to every facet (the synthetic fixture omits "ores" and cannot catch this)."""
    g = load_graph(REPO_ROOT / "projects/ores.codegen/library/templates")
    assert compute_target_set("ores", g) == frozenset(g.facet_archetypes)
    assert compute_target_set("ores", g)  # non-empty
