#!/usr/bin/env python3
"""Emit the convention fixture the prototype reads, from the live database.

Reads ores_dq_<kind>_conventions_artefact_tbl for every kind, and writes one
JavaScript object literal with, per kind: the real document count, and the real
rows the page draws. No value is invented.
"""

import json
import subprocess
import sys

DB = "ores_dev_bright_faraday"
ENV = {"PGPASSWORD": "phiaG6zomaeWohP9", "PATH": "/usr/bin:/bin:/usr/local/bin"}

BOILERPLATE = {"dataset_id", "tenant_id", "version", "party_id", "oresmd_uri"}

# Display names, in the order the reporting prototypes list them.
KINDS = [
    ("deposit", "Deposit", "Deposit"),
    ("fra", "FRA", "FRA"),
    ("future", "Future", "Future"),
    ("ois", "OIS", "OIS"),
    ("average_ois", "Average OIS", "AverageOIS"),
    ("swap", "Swap", "Swap"),
    ("tenor_basis_swap", "Tenor basis swap", "TenorBasisSwap"),
    ("tenor_basis_two_swap", "Tenor basis two swap", "TenorBasisTwoSwap"),
    ("cross_currency_basis", "Cross-currency basis", "CrossCurrencyBasis"),
    ("cross_currency_fix_float", "Cross-currency fix-float", "CrossCurrencyFixFloat"),
    ("bond_yield", "Bond yield", "BondYield"),
    ("cds", "CDS", "CDS"),
    ("cms_spread_option", "CMS spread option", "CMS"),
    ("commodity_forward", "Commodity forward", "CommodityForward"),
    ("commodity_future", "Commodity future", "CommodityFuture"),
    ("fx_option", "FX option", "FxOption"),
    ("bma_basis_swap", "BMA basis swap", "BMABasisSwap"),
    ("zero", "Zero", "Zero"),
    ("inflation_swap", "Inflation swap", "InflationSwap"),
    ("intraday_power_load", "Intraday power load", "IntradayPowerLoad"),
    ("ibor_index", "IBOR index", "IborIndex"),
    ("overnight_index", "Overnight index", "OvernightIndex"),
    ("swap_index", "Named swap index", "SwapIndex"),
    ("currency_pair", "Currency pair", "CurrencyPair"),
    ("zero_inflation_index", "Zero inflation index", "ZeroInflationIndex"),
]

# Fields a document carries on the page: the leading ones, which distinguish it.
FIELD_CAP = 12

# How many documents of a kind the page draws in full. A kind above this keeps
# a real sample and states the real total: the fixture never pretends the sample
# is the whole table.
SAMPLE = 40


def psql(sql):
    out = subprocess.run(
        ["psql", "-h", "localhost", "-U", "postgres", "-d", DB, "-Atc", sql],
        env=ENV, capture_output=True, text=True, check=True)
    return out.stdout


def rows(table, key):
    sql = ("select coalesce(json_agg(row_to_json(t)), '[]'::json) from "
           "(select * from %s order by %s) t;" % (table, key))
    return json.loads(psql(sql))


def main():
    payload = []
    for code, name, ore in KINDS:
        key = "pair_code" if code == "currency_pair" else "id"
        art = "ores_dq_%s_conventions_artefact_tbl" % code
        ref = "ores_refdata_%s_conventions_tbl" % code
        total = int(psql("select count(distinct %s) from %s;" % (key, art)).strip())
        raw = rows(art, key)
        docs = []
        for row in raw[:SAMPLE]:
            fields = [[k, v] for k, v in row.items()
                      if k not in BOILERPLATE and v is not None and v != ""][:FIELD_CAP]
            docs.append({
                "id": row[key],
                "uri": row.get("oresmd_uri"),
                "fields": fields,
            })
        party = psql("select count(*) from information_schema.columns "
                     "where table_name='%s' and column_name='party_id';" % ref).strip()
        cols = json.loads(psql(
            "select coalesce(json_agg(json_build_object('name', column_name, "
            "'type', data_type, 'required', is_nullable = 'NO') "
            "order by ordinal_position), '[]'::json) from information_schema.columns "
            "where table_schema = 'public' and table_name = '%s';" % ref))
        cols = [c for c in cols if c["name"] not in BOILERPLATE
                and c["name"] not in ("modified_by", "performed_by",
                                      "change_reason_code", "change_commentary",
                                      "valid_from", "valid_to")]
        payload.append({
            "columns": cols,
            "code": code,
            "name": name,
            "ore": ore,
            "artefactTable": art,
            "refdataTable": ref,
            "partyScoped": party == "1",
            "total": total,
            "docs": docs,
        })
        print("%-26s total=%-5d drawn=%d" % (code, total, len(docs)), file=sys.stderr)

    with open(sys.argv[1], "w", encoding="utf-8") as out:
        out.write(json.dumps(payload, separators=(",", ":"), sort_keys=False))
    print("wrote %s" % sys.argv[1], file=sys.stderr)


if __name__ == "__main__":
    main()
