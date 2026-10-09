#!/usr/bin/env python3
"""Convert the eight rates product models from instrument entities to fact tables.

Unit 3 of the rates family's plan moves the fields every rates product
shares onto the family header, ores.trading.rate_instruments, and leaves
each product as a fact table keyed by the same trade. This script performs
the mechanical part of that move on the eight models, so the edit is one
reviewable artefact rather than eight hand edits.

Run it from the repository root:

    python3 doc/agile/versions/v0/sprint_27/instruments-rates/evidence/convert_rates_products_to_facts.py [--check]

--check renders the result and diffs it against the tree without writing.
"""

from __future__ import annotations

import argparse
import difflib
import re
import sys
from pathlib import Path

MODEL_DIR = Path("projects/ores.trading/modeling")

# The columns every product shares, moved to the header.
SHARED = {
    "trade_type_code",
    "party_id",
    "start_date",
    "maturity_date",
    "end_date",  # the FRA's spelling of the header's maturity_date
    "description",
}

# Product-only columns the family no longer carries (design decision D5:
# trade_booking already holds the netting set, as a uuid foreign key).
PRODUCT_DROP = {"vanilla_swap_instrument": {"netting_set_id"}}

PRODUCTS = [
    "fra_instrument",
    "vanilla_swap_instrument",
    "cap_floor_instrument",
    "swaption_instrument",
    "balance_guaranteed_swap_instrument",
    "callable_swap_instrument",
    "knock_out_swap_instrument",
    "inflation_swap_instrument",
]

CPP_PROSE = """The C++ domain class is a fact table's: the two keys stay flat, the
implicit scaffolding columns carry the version and the tenant, and the
columns below are the product's own fields. The row carries no
=instrument_identity= and no audit member, because it carries neither the
identity nor the party: the header, =ores.trading.rate_instruments=, owns
both and this row joins it by =trade_id=. The SQL schema, DB entity and
column lists are unaffected. The entity templates consume these
annotations; domain and repository profiles regenerate correctly.

"""

FACT_NOTE = """
This row is the family's fact table, not its identity. The header,
=ores.trading.rate_instruments=, holds the trade type code, the party, the
instrument's start and maturity dates and its description; this table holds
only the product's own fields, and joins the header by =trade_id=.
"""

# The includes the flat columns need, by the cpp_type each one requires.
INCLUDE_FOR_TYPE = [
    ("std::chrono::year_month_day", "#include <chrono>"),
    ("std::optional<", "#include <optional>"),
    ("std::string", "#include <string>"),
    ("boost::uuids::uuid", "#include <boost/uuid/uuid.hpp>"),
    ("ores::utility::decimal::decimal", '#include "ores.utility/decimal/decimal.hpp"'),
]

INCLUDE_ORDER = [
    "#include <chrono>",
    "#include <optional>",
    "#include <string>",
    "#include <boost/uuid/uuid.hpp>",
    '#include "ores.utility/decimal/decimal.hpp"',
]


def split_blocks(text: str) -> list[tuple[str, str]]:
    """Split on level-1 headings, keeping the heading line in each block."""
    starts = [m.start() for m in re.finditer(r"^\* ", text, re.M)]
    blocks: list[tuple[str, str]] = []
    if starts and starts[0] > 0:
        blocks.append(("", text[: starts[0]]))
    for i, start in enumerate(starts):
        end = starts[i + 1] if i + 1 < len(starts) else len(text)
        segment = text[start:end]
        blocks.append((segment.split("\n", 1)[0][2:].strip(), segment))
    return blocks


def split_subsections(block: str) -> list[tuple[str, str]]:
    """Split a level-1 block on its level-2 headings."""
    starts = [m.start() for m in re.finditer(r"^\*\* ", block, re.M)]
    pieces: list[tuple[str, str]] = []
    if starts and starts[0] > 0:
        pieces.append(("", block[: starts[0]]))
    for i, start in enumerate(starts):
        end = starts[i + 1] if i + 1 < len(starts) else len(block)
        segment = block[start:end]
        pieces.append((segment.split("\n", 1)[0][3:].strip(), segment))
    return pieces


def heading(block: str) -> str:
    return block.split("\n", 1)[0]


def data_rows(lines: list[str]) -> list[str]:
    """The org-table rows that are neither the header nor the separator."""
    return [
        line
        for line in lines
        if line.startswith("|") and not re.fullmatch(r"[\|\+\- ]+", line)
    ]


def strip_property(block: str, key: str) -> str:
    return re.sub(r"^:%s:[^\n]*\n" % re.escape(key), "", block, flags=re.M)


def rewrite_columns(block: str, product: str) -> str:
    drop = SHARED | PRODUCT_DROP.get(product, set())
    pieces = split_subsections(block)
    out: list[tuple[str, str]] = []
    for name, segment in pieces:
        if name == "":
            out.append((name, segment))
            continue
        if name in drop:
            continue
        if name in ("trade_id", "trade_activity_id"):
            segment = strip_property(segment, "group")
        out.append((name, segment))
    rebuilt = "".join(segment for _, segment in out)
    # The dropped sections leave the blank line that separated them behind.
    rebuilt = re.sub(r"\n{3,}", "\n\n", rebuilt)
    return rebuilt


def collect_flat_types(columns_block: str) -> list[str]:
    return re.findall(r":cpp_type:\s*(\S[^\n]*?)\s*\n", columns_block)


def rewrite_includes(block: str, columns_block: str) -> str:
    types = collect_flat_types(columns_block)
    wanted = {
        include
        for type_name, include in INCLUDE_FOR_TYPE
        if any(type_name in t for t in types)
    }
    body = "\n".join(i for i in INCLUDE_ORDER if i in wanted)
    replacement = "#+begin_src cpp :name includes\n%s\n#+end_src" % body
    return re.sub(
        r"#\+begin_src cpp :name includes\n.*?#\+end_src",
        replacement,
        block,
        flags=re.S,
    )


def rewrite_sql(block: str) -> str:
    pieces = split_subsections(block)
    out: list[tuple[str, str]] = []
    for name, segment in pieces:
        if name == "Flags":
            segment = re.sub(r"^:routed_by:[^\n]*\n", "", segment, flags=re.M)
            segment = re.sub(r"^:routed_column:[^\n]*\n", "", segment, flags=re.M)
            segment = segment.replace(
                ":END:", ":rls_tenant_isolation: true\n:END:"
            )
        elif name == "Checks":
            lines = segment.split("\n")
            kept = [
                line
                for line in lines
                if not (
                    line.startswith("|")
                    and any('"%s"' % column in line for column in SHARED)
                )
            ]
            if len(data_rows(kept)) <= 1:
                continue
            segment = "\n".join(kept)
        elif name == "Indexes":
            lines = segment.split("\n")
            kept = [
                line
                for line in lines
                if not re.match(r"\|\s*party\s*\|", line)
            ]
            if len(data_rows(kept)) <= 1:
                continue
            segment = "\n".join(kept)
        out.append((name, segment))
    rebuilt = "".join(segment for _, segment in out)
    return re.sub(r"\n{3,}", "\n\n", rebuilt)


def rewrite_table_display(segment: str, product: str) -> str:
    """Drop the rows for the moved columns and re-align the table."""
    drop = SHARED | PRODUCT_DROP.get(product, set())
    rows: list[list[str]] = []
    for line in segment.split("\n"):
        if not line.startswith("|") or re.fullmatch(r"[\|\+\- ]+", line):
            continue
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if len(cells) != 2 or not cells[0]:
            continue
        column, header = cells
        if column in drop or column in ("audit.recorded_at", "identity.trade_type_code"):
            continue
        rows.append([column.replace("identity.trade_id", "trade_id"), header])
    if not rows:
        return ""
    for tail in (["modified_by", "Modified By"], ["version", "Version"]):
        if tail[0] not in [row[0] for row in rows]:
            rows.append(tail)
    width = [max(len(row[i]) for row in rows) for i in (0, 1)]
    lines = [
        "| %s | %s |" % (rows[0][0].ljust(width[0]), rows[0][1].ljust(width[1])),
        "|-%s-+-%s-|" % ("-" * width[0], "-" * width[1]),
    ]
    lines += [
        "| %s | %s |" % (row[0].ljust(width[0]), row[1].ljust(width[1]))
        for row in rows[1:]
    ]
    return "** Table display\n\n" + "\n".join(lines) + "\n\n"


def rewrite_cpp(block: str, columns_block: str, product: str) -> str:
    pieces = split_subsections(block)
    out: list[tuple[str, str]] = []
    for name, segment in pieces:
        if name == "":
            # Replace the C1202 paragraph with the fact-table paragraph.
            out.append((name, "* C++\n\n" + CPP_PROSE))
            continue
        if name == "Flags":
            segment = strip_property(segment, "domain_identity_group")
            segment = strip_property(segment, "domain_audit_group")
            segment = re.sub(
                r":subcomponent:\s+api", ":subcomponent:    api", segment
            )
            segment = re.sub(
                r":has_batch_read:\s+true", ":has_batch_read:       true", segment
            )
            segment = re.sub(
                r":generator_facet_name:\s+generators",
                ":generator_facet_name: generators",
                segment,
            )
        elif name == "Domain includes":
            segment = rewrite_includes(segment, columns_block)
        elif name == "Table display":
            segment = rewrite_table_display(segment, product)
        out.append((name, segment))
    rebuilt = "".join(segment for _, segment in out)
    return re.sub(r"\n{3,}", "\n\n", rebuilt)


def convert(text: str, product: str) -> str:
    blocks = split_blocks(text)
    out: list[tuple[str, str]] = []
    columns_block = ""
    for name, segment in blocks:
        if name == "Natural keys":
            continue
        if name == "":
            segment = segment.rstrip("\n") + "\n" + FACT_NOTE + "\n\n"
        if name == "Flags":
            segment = segment.replace(
                ":profile:          trading-instrument",
                ":has_tenant_id:    true",
            )
        elif name == "Columns":
            segment = rewrite_columns(segment, product)
            columns_block = segment
        elif name == "SQL":
            segment = rewrite_sql(segment)
        elif name == "C++":
            segment = rewrite_cpp(segment, columns_block, product)
        out.append((name, segment))
    rebuilt = "".join(segment for _, segment in out)
    rebuilt = rebuilt.replace("v.identity.trade_id", "v.trade_id")
    rebuilt = rebuilt.replace("v.identity.trade_activity_id", "v.trade_activity_id")
    rebuilt = re.sub(r"\n{3,}", "\n\n", rebuilt)
    return rebuilt.rstrip("\n") + "\n"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()

    failed = False
    for product in PRODUCTS:
        path = MODEL_DIR / ("ores.trading.%s.org" % product)
        before = path.read_text()
        after = convert(before, product)
        if args.check:
            diff = list(
                difflib.unified_diff(
                    before.splitlines(True),
                    after.splitlines(True),
                    fromfile=str(path),
                    tofile=str(path) + " (converted)",
                )
            )
            sys.stdout.writelines(diff)
            failed = failed or bool(diff)
        else:
            path.write_text(after)
            print("converted %s" % path)
    return 1 if (args.check and failed) else 0


if __name__ == "__main__":
    raise SystemExit(main())
