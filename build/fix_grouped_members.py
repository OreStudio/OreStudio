#!/usr/bin/env python3
"""Rewrite flat member access on grouped domain types to its member path.

Reads clang errors of the form

    path:LINE:COL: error: no member named 'MEMBER' in 'ores::refdata::domain::TYPE'

and inserts the group prefix at the reported column. A codemod rather than a
hand edit, because the same three types are reached from tests, mappers and
services, and the compiler already knows every site.
"""

import re
import sys
from collections import defaultdict
from pathlib import Path

GROUPS = {
    "convention": {
        "identity": ["id", "version", "party_id", "kind_code", "tenant_id"],
        "payload": ["name", "oresmd_uri"],
        "audit": ["modified_by", "performed_by", "change_reason_code",
                  "change_commentary", "recorded_at"],
    },
    "tenor_basis_swap_pay_receive": {
        "identity": ["id", "version", "party_id", "convention_id", "tenant_id"],
        "legs": ["pay_index", "pay_frequency", "receive_index",
                 "receive_frequency", "pay_is_averaged", "rec_is_averaged"],
        "coupon": ["spread_on_rec", "include_spread", "sub_periods_coupon_type"],
        "audit": ["modified_by", "performed_by", "change_reason_code",
                  "change_commentary", "recorded_at"],
    },
    "tenor_basis_swap_long_short": {
        "identity": ["id", "version", "party_id", "convention_id", "tenant_id"],
        "legs": ["long_index", "long_pay_tenor", "short_index",
                 "short_pay_tenor", "spread_on_short"],
        "coupon": ["include_spread", "sub_periods_coupon_type"],
        "audit": ["modified_by", "performed_by", "change_reason_code",
                  "change_commentary", "recorded_at"],
    },
}

ERROR = re.compile(
    r"(?P<path>[^:\s]+):(?P<line>\d+):(?P<col>\d+): error: no member named "
    r"'(?P<member>\w+)' in '(?P<type>[\w:]+)'")


def member_group(type_name, member):
    short = type_name.rsplit("::", 1)[-1]
    for group, members in GROUPS.get(short, {}).items():
        if member in members:
            return group
    return None


def main():
    edits = defaultdict(list)
    for raw in sys.stdin:
        match = ERROR.search(raw)
        if not match:
            continue
        group = member_group(match["type"], match["member"])
        if group is None:
            continue
        edits[match["path"]].append(
            (int(match["line"]), int(match["col"]), match["member"], group))

    root = Path("build/output/linux-clang-release-ninja").resolve()
    for path, sites in edits.items():
        p = Path(path)
        if not p.is_absolute():
            p = (root / p).resolve()
        lines = p.read_text().split("\n")
        # Apply from the end so an earlier column on the same line still moves.
        for line_no, col, member, group in sorted(sites, reverse=True):
            text = lines[line_no - 1]
            start = col - 1
            end = start + len(member)
            if text[start:end] != member:
                print(f"skip {path}:{line_no}:{col} '{member}' not at column")
                continue
            prefix = f"{group}."
            if text[:start].endswith(prefix):
                continue
            lines[line_no - 1] = text[:start] + f"{group}." + text[start:]
        p.write_text("\n".join(lines))
        print(f"rewrote {len(sites)} site(s) in {p}")


if __name__ == "__main__":
    main()
