"""Every coordinate an oresmd URI carries is a declared dimension.

The grammar used to flatten a surface or composite coordinate into one
comma-joined ``point`` string, so the meaning of each token was a fact about
the quote type that no model stated and no test could check. The models now
declare their coordinate keys, and each quote type lists the ones it carries.

This check reads the same models the generator reads and asks two questions of
every test-case URI that still spells a ``point``:

- does the URI name a quote type the model declares, once the asset class's own
  default is applied; and
- does that quote type declare the coordinate dimensions its key carries?

It deliberately does not compare the token count with the declared keys. The
old ``point`` mixes coordinate and identity dimensions -- a capfloor's
``5y,6m,0,0,0.03`` is an expiry, a float tenor, two surface flags and a strike,
of which only the expiry and the strike are coordinates -- and its arity varies
with the key's shape, as a swaption's smile form shows. That mapping is what
the generated parser of the next unit replaces, and asserting it here would
re-state the old grammar rather than check the new declaration.

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

# The silent defaults of type=vol: a surface whose family the old grammar never
# named. These are the four the work gives a quote type to.
VOL_DEFAULTS = {
    "fx": "option",
    "equity": "option",
    "commodity": "option",
    "ir": "swaption",
}

# Rows whose key is expected to be refused are not declarations of anything.
SKIP_TABLES = {"rejection"}


def _specs():
    return load_org_oresmd_quote_type_model(MODEL)["oresmd_quote_types"]


def _point_uris(spec):
    """(asset, uri, tokens, resolved quote type) for every positive point case."""
    asset = spec["asset_class"]
    for table, rows in (spec.get("test_cases") or {}).items():
        if table in SKIP_TABLES:
            continue
        for row in rows:
            uri = row.get("uri", "")
            if "point=" not in uri or row.get("expected") == "nullopt":
                continue
            query = parse_qs(urlsplit(uri).query, keep_blank_values=True)
            raw = (query.get("point") or [""])[0]
            tokens = [t for t in raw.split(",") if t]
            quote = (query.get("quote") or [None])[0]
            if quote is None:
                is_vol = "type=vol" in uri
                quote = VOL_DEFAULTS.get(asset) if is_vol else DEFAULTS.get(asset)
            yield asset, uri, tokens, quote


def _by_type(spec):
    return {qt["enum_name"]: qt for qt in spec["quote_types"]}


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


def test_every_point_uri_names_a_quote_type_that_declares_coordinates():
    problems = []
    checked = 0
    for spec in _specs():
        by_type = _by_type(spec)
        for asset, uri, tokens, quote in _point_uris(spec):
            if not tokens:
                continue
            checked += 1
            if quote is None:
                problems.append(f"{asset}: the URI names no quote type and the "
                                f"asset class has no default. {uri}")
                continue
            if quote not in by_type:
                problems.append(f"{asset}: '{quote}' is not a declared quote type. {uri}")
                continue
            declared = by_type[quote]
            if not declared["coordinates"] and declared["point_keys"]:
                problems.append(
                    f"{asset}: '{quote}' carries {len(tokens)} token(s) but "
                    f"declares no coordinate. {uri}")
                continue
            # The token count is bounded by the keys the old point corresponds
            # to, identity keys included. A key the point carries and the model
            # does not declare is what this catches.
            if len(tokens) > len(declared["point_keys"]):
                problems.append(
                    f"{asset}: '{quote}' point carries {len(tokens)} token(s) "
                    f"but declares {len(declared['point_keys'])} key(s): "
                    f"{declared['point_keys']}. {uri}")
    assert checked > 30, f"only {checked} point URIs found; the loader or the tables changed"
    assert not problems, "\n".join(problems)
