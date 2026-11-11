"""The route inventory rejects a pair that names a route no server answers.

Run::

    python3 -m pytest build/scripts/tests/test_route_pairing.py

Coverage is counted from the pairs, so a pair to a route that does not exist would
count a BFF route as covered when nothing answers it.
"""
import ast
import sys
from collections import Counter
from pathlib import Path
from types import SimpleNamespace

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "build/scripts"))

import route_inventory  # noqa: E402
import pytest  # noqa: E402

BFF_ROUTE = ("GET", "/api/roles")
CPP_ROUTE = ("GET", "/api/v1/access/roles")
FOUND = [
    {"server": "bff", "method": "get", "path": BFF_ROUTE[1]},
    {"server": "bff", "method": "post", "path": BFF_ROUTE[1]},
    {"server": "ores.http", "method": "get", "path": CPP_ROUTE[1]},
]


@pytest.fixture(autouse=True)
def no_pairs(monkeypatch):
    """Each test states the pairs and families it reads; the tree's own name real routes."""
    monkeypatch.setattr(route_inventory, "PAIRS", {})
    monkeypatch.setattr(route_inventory, "FAMILIES", {})


def _repeated_keys(source):
    """The keys written more than once in a PAIRS dict literal, which Python drops."""
    for node in ast.walk(ast.parse(source)):
        if isinstance(node, ast.Assign) and node.targets[0].id == "PAIRS":
            keys = Counter(ast.literal_eval(key) for key in node.value.keys)
            return [key for key, count in keys.items() if count > 1]
    raise AssertionError("no PAIRS assignment in the source")


def test_a_repeated_key_is_detected():
    source = 'PAIRS = {("GET", "/a"): 1, ("GET", "/b"): 2, ("GET", "/a"): 3}'
    assert _repeated_keys(source) == [("GET", "/a")]


def test_the_pairing_table_repeats_no_key():
    source = (REPO_ROOT / "build/scripts/route_pairing.py").read_text(encoding="utf-8")
    assert _repeated_keys(source) == []


def test_an_undecided_route_is_counted_under_no_area(monkeypatch):
    monkeypatch.setattr(route_inventory, "PAIRS", {})
    monkeypatch.setattr(route_inventory, "decide", lambda route: None)
    assert "| (none) | 2 | 0 | 2 |" in route_inventory.coverage(FOUND)


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


REGISTRY = """export const RECORDS: readonly RecordResource[] = [
    {
        key: 'currencies',
        keyFields: ['iso_code'],
        writable: true,
        subjects: {
            list: 'a',
        },
    },
    {
        key: 'currency-countries',
        keyFields: ['currency_iso_code', 'country_alpha2_code'],
        writable: false,
        listBy: {
            subject: 'b',
        },
    },
];
"""

FAMILY_FOUND = [
    {"server": "bff", "method": "get", "path": "/api/refdata/:resource"},
    {"server": "bff", "method": "get", "path": "/api/refdata/:resource/key/:key"},
    {"server": "ores.http", "method": "get", "path": "/api/refdata/currencies"},
    {"server": "ores.http", "method": "get", "path": "/api/refdata/currency-countries"},
    {"server": "ores.http", "method": "get", "path": "/api/refdata/currencies/key/{key}"},
]


@pytest.fixture
def registry(tmp_path, monkeypatch):
    (tmp_path / "records.ts").write_text(REGISTRY, encoding="utf-8")
    monkeypatch.setattr(route_inventory, "ROOT", tmp_path)
    return "records.ts:RECORDS"


def test_a_registry_states_what_each_resource_answers(registry):
    resources = route_inventory.registry_resources(registry)
    assert [r["key"] for r in resources] == ["currencies", "currency-countries"]
    assert [r["one_key"] for r in resources] == [True, False]
    assert [r["list_by"] for r in resources] == [False, True]
    assert [r["writable"] for r in resources] == [True, False]


def test_a_family_answered_by_every_resource_is_accepted(registry, monkeypatch):
    monkeypatch.setattr(route_inventory, "FAMILIES", {
        ("GET", "/api/refdata/:resource"):
            (registry, "GET", "/api/refdata/{resource}", "every"),
        ("GET", "/api/refdata/:resource/key/:key"):
            (registry, "GET", "/api/refdata/{resource}/key/{key}", "one_key"),
    })
    assert route_inventory.bad_pairs(FAMILY_FOUND) == []


def test_a_resource_missing_its_family_route_is_named(registry, monkeypatch):
    monkeypatch.setattr(route_inventory, "FAMILIES", {
        ("GET", "/api/refdata/:resource"):
            (registry, "GET", "/api/refdata/{resource}", "every"),
    })
    found = [route for route in FAMILY_FOUND if route["path"] != "/api/refdata/currency-countries"]
    problems = route_inventory.bad_pairs(found)
    assert problems == [
        "GET /api/refdata/:resource: ores.http does not answer GET /api/refdata/currency-countries"]


def test_a_family_whose_registry_is_empty_is_named(tmp_path, monkeypatch):
    (tmp_path / "records.ts").write_text("export const RECORDS = [\n];\n", encoding="utf-8")
    monkeypatch.setattr(route_inventory, "ROOT", tmp_path)
    monkeypatch.setattr(route_inventory, "FAMILIES", {
        ("GET", "/api/refdata/:resource"):
            ("records.ts:RECORDS", "GET", "/api/refdata/{resource}", "every"),
    })
    problems = route_inventory.bad_pairs(FAMILY_FOUND)
    assert problems == ["GET /api/refdata/:resource: records.ts:RECORDS declares no resource"]


def test_coverage_counts_a_family_as_paired(registry, monkeypatch):
    monkeypatch.setattr(route_inventory, "FAMILIES", {
        ("GET", "/api/refdata/:resource"):
            (registry, "GET", "/api/refdata/{resource}", "every"),
    })
    monkeypatch.setattr(route_inventory, "decide",
                        lambda route: SimpleNamespace(area="reference-data"))
    lines = route_inventory.coverage(FAMILY_FOUND)
    assert "| GET =/api/refdata/:resource= | GET /api/refdata/{resource}, one per resource (2) |" in lines
    assert "| reference-data | 2 | 1 | 1 |" in lines

