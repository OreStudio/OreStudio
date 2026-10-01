"""Every coordinate an oresmd URI carries is a declared dimension.

The grammar used to flatten a surface or composite coordinate into one
comma-joined ``point`` string, so the meaning of each token was a fact about the
quote type that no model stated and no test could check. The models now declare
their coordinate keys, each quote type lists the ones it carries, and the URI
names each dimension with its own query key.

This check reads the same models the generator reads and asks, of every
test-case URI:

- does it avoid the retired ``point`` bag;
- does every coordinate it names belong to a quote type the model declares,
  once the asset class's own default is applied; and
- does the resolved quote type declare that coordinate?

A key that is also a declared field -- an IR swap's index tenor, a future's
contract month -- is identity for the class and is present whatever the type
says, so it is not checked against the quote type.

The check pins a floor on the number of URIs it looked at, so a loader that
silently returns nothing fails here instead of passing over an empty set.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_oresmd_coordinates.py
"""
import sys
from pathlib import Path
from urllib.parse import parse_qs, urlsplit

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import load_org_oresmd_quote_type_model  # noqa: E402

MODEL = REPO_ROOT / "projects/ores.marketdata/modeling/oresmd/model.org"

# The quote type an asset class's key defaults to when the URI names none.
# ORE's own key has no type segment for these, so the grammar has a default.
DEFAULTS = {
    "fx": "spot",
    "equity": "spot",
    "commodity": "spot",
    "credit": "cds",
}

# The defaults of type=vol: a surface whose family the older grammar never
# named. These are the four the work gives a quote type to.
VOL_DEFAULTS = {
    "fx": "option",
    "equity": "option",
    "commodity": "option",
    "ir": "swaption",
}

# Rows whose key is expected to be refused are not declarations of anything.
SKIP_TABLES = {"rejection"}

# Query keys that are not coordinates of any asset class: the identity keys ORE
# writes beside a coordinate. A key an asset class declares is judged against
# its own table, not this list.
IDENTITY_KEYS = {
    "type", "quote", "model", "metric", "ccy", "index", "index_spelling",
    "tenor", "second_tenor", "second_ccy", "contract_month", "contract_code",
    "curve_id", "day_count", "settle", "shift", "strip", "role", "source",
    "source_spelling", "name_spelling", "delivery", "second_factor",
}


def _specs():
    return load_org_oresmd_quote_type_model(MODEL)["oresmd_quote_types"]


def _uris(spec):
    """(asset, uri, query keys, resolved quote type) for every positive case."""
    asset = spec["asset_class"]
    for table, rows in (spec.get("test_cases") or {}).items():
        if table in SKIP_TABLES:
            continue
        for row in rows:
            uri = row.get("uri", "")
            if not uri:
                continue
            query = parse_qs(urlsplit(uri).query, keep_blank_values=True)
            keys = set(query)
            negative = row.get("expected") == "nullopt"
            quote = (query.get("quote") or [None])[0]
            if quote is None:
                is_vol = (query.get("type") or ["quote"])[0] == "vol"
                quote = VOL_DEFAULTS.get(asset) if is_vol else DEFAULTS.get(asset)
            yield asset, uri, keys, quote, negative


def _by_type(spec):
    return {qt["enum_name"]: qt for qt in spec["quote_types"]}


def _coordinate_keys(spec):
    return {c["query_key"]: c for c in (spec.get("coordinate_keys") or [])}


def test_the_models_declare_coordinates_to_check():
    specs = _specs()
    assert len(specs) >= 12
    declared = {s["asset_class"]: s.get("coordinate_keys") or [] for s in specs}
    for asset in ("fx", "ir", "equity", "credit", "commodity",
                  "correlation", "inflation", "rating", "shape_profile"):
        assert declared[asset], f"{asset} declares no coordinate"


def test_the_volatility_families_have_a_quote_type_of_their_own():
    # A surface used to be the silent default of type=vol, so its key had no
    # name of its own. Each now has one.
    by_asset = {s["asset_class"]: s for s in _specs()}
    for asset, expected in sorted(VOL_DEFAULTS.items()):
        names = {qt["enum_name"] for qt in by_asset[asset]["quote_types"]}
        assert expected in names, f"{asset} has no '{expected}' quote type"


def test_no_positive_test_case_still_spells_a_point_bag():
    offenders = []
    for spec in _specs():
        for asset, uri, keys, _, _negative in _uris(spec):
            if "point" in keys:
                offenders.append(f"{asset}: {uri}")
    assert not offenders, (
        "the point bag is retired; name each coordinate with its own key:\n"
        + "\n".join(offenders)
    )


def test_every_coordinate_names_a_quote_type_that_declares_it():
    problems = []
    checked = 0
    for spec in _specs():
        by_type = _by_type(spec)
        declared = _coordinate_keys(spec)
        for asset, uri, keys, quote, negative in _uris(spec):
            if negative:
                # A projection row that expects no key is a negative case: the
                # URI deliberately names no quote type.
                continue
            coordinates = {
                k for k in keys
                if k in declared and not declared[k].get("is_field")
            }
            # Field keys are identity for the class and are checked by nothing
            # here; a key that is neither a coordinate nor a known identity key
            # is a name the grammar does not have.
            unknown = keys - set(declared) - IDENTITY_KEYS
            if unknown:
                problems.append(f"{asset}: undeclared query key(s) {sorted(unknown)}. {uri}")
                continue
            if not coordinates:
                continue
            checked += 1
            if quote is None:
                problems.append(f"{asset}: the URI names no quote type and the "
                                f"asset class has no default. {uri}")
                continue
            if quote not in by_type:
                problems.append(f"{asset}: '{quote}' is not a declared quote type. {uri}")
                continue
            carried = set(by_type[quote]["coordinates"])
            for key in sorted(coordinates - carried):
                problems.append(
                    f"{asset}: '{quote}' does not carry the '{key}' coordinate. {uri}")
    assert checked > 30, f"only {checked} coordinate URIs found; the loader or the tables changed"
    assert not problems, "\n".join(problems)
