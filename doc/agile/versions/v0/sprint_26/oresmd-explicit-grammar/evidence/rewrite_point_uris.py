#!/usr/bin/env python3
"""One-off: rewrite the oresmd model test URIs from `point=` to named keys."""
import pathlib
import re
import sys
from urllib.parse import parse_qsl, urlencode, urlsplit, urlunsplit

ROOT = pathlib.Path("projects/ores.marketdata/modeling/oresmd")

POINT_KEYS = {
    "fx": {"spot": [], "fwd": ["maturity"], "option": ["expiry", "delta"]},
    "ir": {
        "ir_swap": ["maturity"], "discount": [], "mm": ["maturity"],
        "fra": ["maturity"], "imm_fra": ["maturity"], "basis_swap": ["maturity"],
        "bma_swap": ["maturity"], "cc_basis_swap": ["maturity"],
        "cc_fix_float_swap": ["maturity"], "zero": ["maturity"],
        "mm_future": [], "oi_future": [],
        "capfloor": ["expiry", "tenor", "shift", "strip", "strike"],
        "bond_option": ["expiry", "tenor", "strike"],
    },
    "equity": {
        "spot": [], "dividend": ["maturity"], "fwd": ["maturity"],
        "option": ["expiry", "delta", "premium", "call_put", "strike"],
    },
    "commodity": {
        "spot": [], "fwd": ["maturity"],
        "option": ["expiry", "delta", "premium", "call_put", "strike"],
    },
    "credit": {
        "cds": ["seniority", "restructuring", "tenor"],
        "hazard_rate": ["seniority", "restructuring", "tenor"],
        "recovery_rate": ["seniority", "restructuring"],
        "cds_index": ["tenor", "strike"],
        "index_cds_tranche": ["tenor", "strike"],
        "index_cds_option": ["tenor", "expiry", "strike"],
    },
    "inflation": {
        "zc_swap": ["maturity"], "yy_swap": ["maturity"], "seasonality": ["month"],
        "zc_capfloor": ["expiry", "call_put", "strike"],
        "yy_capfloor": ["expiry", "call_put", "strike"],
        "cf_price": ["expiry", "call_put", "strike"],
    },
    "correlation": {"pairwise": ["expiry", "delta"]},
    "rating": {"transition_probability": ["from", "to"]},
    "shape_profile": {"shape_factor": ["date", "second", "period", "dst"]},
}

# `from` is the query key; the identifier member is spelled from_grade.
QP_NAME = {"from": "from"}

NUMERIC = re.compile(r"^[0-9]+\.[0-9]+$")


def keys_for(asset, quote, tokens):
    """The ordered keys the tokens land on, shape-aware for the swaption."""
    if quote == "swaption":
        if len(tokens) == 4:
            return ["expiry", "tenor", "smile", "strike"]
        return ["expiry", "tenor", "strike"]
    return POINT_KEYS[asset].get(quote, [])


def rewrite(uri):
    parts = urlsplit(uri)
    pairs = parse_qsl(parts.query, keep_blank_values=True)
    lookup = {}
    for k, v in pairs:
        lookup.setdefault(k, v)
    point = lookup.get("point")
    if point is None:
        return uri, False
    asset = parts.netloc
    quote = lookup.get("quote")
    if quote is None:
        if lookup.get("type") == "vol":
            quote = {"ir": "swaption", "fx": "option", "equity": "option",
                     "commodity": "option"}.get(asset)
        else:
            quote = {"fx": "spot", "equity": "spot", "commodity": "spot",
                     "credit": "cds"}.get(asset)
    tokens = [t for t in point.split(",") if t]
    keys = keys_for(asset, quote, tokens)
    if len(keys) < len(tokens):
        return uri, False
    out = []
    for k, v in pairs:
        if k == "point":
            continue
        out.append((k, v))
    for key, token in zip(keys, tokens):
        # A numeric last token is a strike; a moneyness is a delta.
        if key == "delta" and NUMERIC.match(token):
            key = "strike"
        if any(k == key for k, _ in out):
            continue
        out.append((QP_NAME.get(key, key), token))
    return urlunsplit((parts.scheme, parts.netloc, parts.path,
                       urlencode(out), parts.fragment)), True


def main():
    total = 0
    skipped = []
    for path in sorted(ROOT.glob("*_quote_type.org")):
        asset = path.name.removesuffix("_quote_type.org")
        if asset not in POINT_KEYS:
            continue
        lines = path.read_text().splitlines()
        changed = 0
        for i, line in enumerate(lines):
            if "point=" not in line or not line.startswith("|"):
                continue
            out = []
            for cell in line.split("|"):
                if "oresmd://" in cell and "point=" in cell:
                    stripped = cell.strip()
                    new_uri, ok = rewrite(stripped)
                    if not ok:
                        skipped.append(f"{path.name}: {stripped}")
                    else:
                        cell = cell.replace(stripped, new_uri)
                        changed += 1
                out.append(cell)
            lines[i] = "|".join(out)
        if changed:
            path.write_text("\n".join(lines) + "\n")
            total += changed
            print(f"{path.name}: {changed} URI(s)")
    print(f"total {total}")
    for s in skipped:
        print("  skipped:", s, file=sys.stderr)


if __name__ == "__main__":
    main()
