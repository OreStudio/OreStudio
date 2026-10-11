#!/usr/bin/env python3
"""Write ':economic:' on every column of the named models, once.

The economic digest used to decide by a field's name which fields the customer
agreed to. This script applies that rule a last time and writes the answer into
the model, where a person can read it and change it. After it has run the rule
is gone from the C++, and a field is economic because its model says so.

The rule it applies is the one the digest carried:

  * these names are identity or audit, so not economic: id, version,
    external_version, economic_digest, trade_type_code, description,
    modified_by, performed_by, change_reason_code, change_commentary,
    recorded_at, valid_from, valid_to;
  * a name ending in _id is a key, so not economic, except the five external
    identifiers security_id, credit_curve_id, reference_curve_id,
    income_curve_id and volatility_curve_id;
  * every other name is economic.

A column that already declares ':economic:' is left alone, so the script can
run again after a model gains a column. A column that reaches no flat member
of the struct (an identity-group column, a sql_only column) is skipped,
because its declaration lives on the group's own model or nowhere.

Usage:
  python3 scripts/declare_economic_fields.py MODEL.org [MODEL.org ...]
  python3 scripts/declare_economic_fields.py --dry-run MODEL.org ...
"""

import argparse
import re
import sys
from pathlib import Path

EXCLUDED_NAMES = frozenset({
    "id", "version", "external_version", "economic_digest", "trade_type_code",
    "description", "modified_by", "performed_by", "change_reason_code",
    "change_commentary", "recorded_at", "valid_from", "valid_to",
})

EXTERNAL_IDENTIFIERS = frozenset({
    "security_id", "credit_curve_id", "reference_curve_id",
    "income_curve_id", "volatility_curve_id",
})

HEADING = re.compile(r"^\*\* (\S+)\s*$")
SECTION = re.compile(r"^\* (.+?)\s*$")
PROPERTY = re.compile(r"^\s*:([A-Za-z0-9_.-]+):\s*(.*?)\s*$")


def is_economic(name: str) -> bool:
    if name in EXCLUDED_NAMES:
        return False
    if name in EXTERNAL_IDENTIFIERS:
        return True
    return not (len(name) > 3 and name.endswith("_id"))


def value_column(drawer: list[str]) -> int:
    """The column where the drawer's first value starts, so the new line
    lines up with the ones above it."""
    for entry in drawer[1:]:
        match = re.match(r"^(:[A-Za-z0-9_.-]+:\s+)\S", entry)
        if match:
            return max(len(match.group(1)), len(":economic: "))
    return len(":economic: ")


def declare(text: str) -> tuple[str, int]:
    """Return the text with ':economic:' added, and how many lines were added."""
    lines = text.split("\n")
    out: list[str] = []
    added = 0
    in_members = False
    i = 0
    while i < len(lines):
        line = lines[i]
        section = SECTION.match(line)
        if section:
            in_members = section.group(1) in ("Columns", "Fields")
        heading = HEADING.match(line)
        if not (in_members and heading):
            out.append(line)
            i += 1
            continue
        out.append(line)
        i += 1
        if i >= len(lines) or lines[i].strip() != ":PROPERTIES:":
            continue
        end = i
        while lines[end].strip() != ":END:":
            end += 1
        drawer = lines[i:end]
        props = {}
        for entry in drawer[1:]:
            match = PROPERTY.match(entry)
            if match:
                props[match.group(1).lower()] = match.group(2)
        skip = (
            "economic" in props
            or props.get("group", "").lower() == "identity"
            or props.get("sql_only", "").lower() == "true"
        )
        out.extend(drawer)
        if not skip:
            name = heading.group(1)
            width = value_column(drawer)
            value = "true" if is_economic(name) else "false"
            out.append(f"{':economic:':<{width}}{value}")
            added += 1
        i = end
    return "\n".join(out), added


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("models", nargs="+", type=Path)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()
    total = 0
    for path in args.models:
        text = path.read_text(encoding="utf-8")
        new, added = declare(text)
        total += added
        print(f"{path.name}: {added} declared")
        if added and not args.dry_run:
            path.write_text(new, encoding="utf-8")
    print(f"total: {total}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
