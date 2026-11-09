"""The route inventory rejects a pair that names a route no server answers.

Run::

    python3 -m pytest build/scripts/tests/test_route_pairing.py

Coverage is counted from the pairs, so a pair to a route that does not exist would
count a BFF route as covered when nothing answers it.
"""
import sys
from pathlib import Path
from types import SimpleNamespace

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "build/scripts"))

import route_inventory  # noqa: E402

BFF_ROUTE = ("GET", "/api/roles")
CPP_ROUTE = ("GET", "/api/v1/access/roles")
FOUND = [
    {"server": "bff", "method": "get", "path": BFF_ROUTE[1]},
    {"server": "bff", "method": "post", "path": BFF_ROUTE[1]},
    {"server": "ores.http", "method": "get", "path": CPP_ROUTE[1]},
]


def test_a_pair_of_two_real_routes_is_accepted(monkeypatch):
    monkeypatch.setattr(route_inventory, "PAIRS", {BFF_ROUTE: CPP_ROUTE})
    assert route_inventory.bad_pairs(FOUND) == []


def test_a_pair_to_a_missing_cpp_route_is_named(monkeypatch):
    missing = ("GET", "/api/v1/access/rolez")
    monkeypatch.setattr(route_inventory, "PAIRS", {BFF_ROUTE: missing})
    problems = route_inventory.bad_pairs(FOUND)
    assert len(problems) == 1
    assert "/api/v1/access/rolez" in problems[0]


def test_a_pair_from_a_missing_bff_route_is_named(monkeypatch):
    missing = ("GET", "/api/rolez")
    monkeypatch.setattr(route_inventory, "PAIRS", {missing: CPP_ROUTE})
    problems = route_inventory.bad_pairs(FOUND)
    assert len(problems) == 1
    assert "/api/rolez" in problems[0]


def test_a_bff_route_is_not_accepted_as_the_cpp_target(monkeypatch):
    monkeypatch.setattr(route_inventory, "PAIRS", {BFF_ROUTE: ("POST", BFF_ROUTE[1])})
    assert len(route_inventory.bad_pairs(FOUND)) == 1


def test_coverage_counts_pairs_and_prints_a_gap_as_none(monkeypatch):
    monkeypatch.setattr(route_inventory, "PAIRS", {BFF_ROUTE: CPP_ROUTE})
    monkeypatch.setattr(route_inventory, "decide",
                        lambda route: SimpleNamespace(area="access"))
    lines = route_inventory.coverage(FOUND)
    assert "| access | 2 | 1 | 1 |" in lines
    assert "| total | 2 | 1 | 1 |" in lines
    assert "| POST =/api/roles= | none |" in lines
