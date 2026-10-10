#!/usr/bin/env python3
"""Convert a money column in a model to an exact decimal.

A money column belongs as an exact decimal in both the domain type and the
column, per doc/knowledge/architecture/exact_numbers_and_economic_change.org.
Doing that by hand means three edits per column, and missing the third is a
build failure: the column's own `#+begin_src cpp :name generator` block emits
a bare literal, which assigns a double to a decimal in the generated
`*_generator.cpp`.

This does all three. It is the lever the batch needs; run it, regenerate,
build, and delete the column's line from
build/scripts/money_decimal_columns.baseline.

Usage:
  python3 scripts/convert_money_column_to_decimal.py --file <model.org> \\
      --column <name> [--scale 12] [--write]

Without --write it prints the result and changes nothing.
"""

import argparse
import re
import sys
from pathlib import Path

DECIMAL = "ores::utility::decimal::decimal"

# Width, not just scale. numeric(28, 12) holds sixteen integer digits and
# numeric(28, 10) held eighteen, so widening the scale without widening the
# precision narrows the integer part. numeric(38, 12) holds twenty-six.
PRECISION = 38


def convert(text: str, column: str, scale: int,
            to_float: bool = False) -> tuple[str, list[str]]:
    """Returns the converted text and the changes it made.

    @p to_float runs the mirror direction: a continuous quantity leaves the
    exact type for `double precision` and a `double` in the domain, because
    rounding a volatility or a day-count fraction to twelve places states a
    precision the quantity does not have.
    """
    changes = []
    heading = re.compile(rf"^\*\* {re.escape(column)}\s*$", re.MULTILINE)
    m = heading.search(text)
    if not m:
        raise SystemExit(f"no column heading '** {column}' in the file")

    # The column's property block, up to its :END:.
    end = text.find(":END:", m.end())
    if end == -1:
        raise SystemExit(f"column '{column}' has no :END: in its drawer")
    block = text[m.end():end]

    domain = "double" if to_float else DECIMAL
    new_block = block
    if re.search(r"^:type:", block, re.MULTILINE):
        sql_type = "double precision" if to_float else \
            f"numeric({PRECISION}, {scale})"
        rewritten = re.sub(r"^:type:.*$", f":type:          {sql_type}",
                            new_block, count=1, flags=re.MULTILINE)
        if rewritten != new_block:
            changes.append("type")
        new_block = rewritten
    default = "0.0" if to_float else f"{DECIMAL}{{}}"
    rewritten = re.sub(r"^:default_value:[ \t]*[+-]?\d+(\.\d+)?[ \t]*$",
                        f":default_value: {default}",
                        new_block, count=1, flags=re.MULTILINE)
    if rewritten != new_block:
        changes.append("default_value")
    new_block = rewritten

    if re.search(r"^:cpp_type:", block, re.MULTILINE):
        def cpp(match):
            existing = match.group(1).strip()
            if domain in existing:
                return match.group(0)
            if existing.startswith("std::optional<"):
                return f":cpp_type:      std::optional<{domain}>"
            return f":cpp_type:      {domain}"
        before = new_block
        new_block = re.sub(r"^:cpp_type:\s*(.*)$", cpp, new_block, count=1,
                           flags=re.MULTILINE)
        if new_block != before:
            changes.append("cpp_type")
    text = text[:m.end()] + new_block + text[end:]

    # The generator block that follows the column's prose, if any.
    #
    # The search must stop at the next `** ` heading. A pattern that runs to
    # the next generator block in the file instead will find the *neighbour's*
    # generator whenever this column has none, and silently rewrite a column
    # the caller never named.
    m = heading.search(text)
    if m is None:
        raise SystemExit(f"column '{column}' lost its heading")
    following = re.compile(r"^\*\* ", re.MULTILINE).search(text, m.end())
    section_end = following.start() if following else len(text)
    section = text[:section_end]

    gen = None
    begin = re.compile(r"^#\+begin_src cpp :name generator[ \t]*\n",
                       re.MULTILINE).search(section, m.end())
    if begin:
        close = re.compile(r"\n^#\+end_src[ \t]*$", re.MULTILINE).search(
            section, begin.end())
        if close:
            gen = (begin.end(), close.start())
    if gen is None and "cpp_type" in changes and "optional" not in new_block and \
            not to_float:
        # A non-optional double with no generator block to rewrite. The
        # generator cannot be the problem here, so the type change lands on
        # the generated mapper instead, which assigns a double to a decimal
        # and will not compile. Flag it: the fix is per-family and this tool
        # cannot do it.
        changes.append(
            "NO generator block on a non-optional double -- the generated "
            "mapper will need from_double/to_double by hand")
    if gen:
        body = text[gen[0]:gen[1]]
        literal = body.strip()
        if to_float:
            # The mirror: `decimal::from_string("0.05").value()` becomes the
            # bare literal, which is what a double member takes.
            precise = re.fullmatch(
                rf"{re.escape(DECIMAL)}::from_string\(\"([^\"]+)\"\)\.value\(\)",
                literal)
            if precise:
                text = text[:gen[0]] + f" {precise.group(1)}" + text[gen[1]:]
                changes.append("generator")
            elif literal == "std::nullopt" or re.fullmatch(
                    r"[+-]?\d+(\.\d+)?", literal):
                changes.append("generator left alone")
            else:
                changes.append(
                    f"generator NOT rewritten (not a literal: {literal!r})")
        elif DECIMAL in literal:
            pass
        elif re.fullmatch(r"[+-]?\d+(\.\d+)?", literal):
            replacement = f' {DECIMAL}::from_string("{literal}").value()'
            text = text[:gen[0]] + replacement + text[gen[1]:]
            changes.append("generator")
        else:
            # A generator whose body is std::nullopt is already correct for a
            # decimal and needs no rewrite; only anything else is a surprise.
            if literal == "std::nullopt":
                changes.append("generator left alone (nullopt)")
            else:
                changes.append(f"generator NOT rewritten (not a bare literal: {literal!r})")

    return text, changes


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--file", type=Path)
    parser.add_argument("--column")
    parser.add_argument("--baseline", type=Path,
                        default=Path("build/scripts/money_decimal_columns.baseline"),
                        help="batch mode: convert every column this file lists")
    parser.add_argument("--scale", type=int, default=12)
    parser.add_argument("--to-float", action="store_true",
                        help="the mirror direction: a continuous quantity "
                             "leaves the exact type for double precision")
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args()

    if args.file is None:
        if not args.column and args.baseline.is_file():
            return convert_baseline(args)
        parser.error("--file and --column are required unless --baseline is used")

    text = args.file.read_text(encoding="utf-8")
    converted, changes = convert(text, args.column, args.scale, args.to_float)

    print(f"{args.file}: {args.column} -> {', '.join(changes) if changes else 'no change'}")
    if args.write:
        args.file.write_text(converted, encoding="utf-8")
        check_drawers(converted, args.file, args.column)
        print("written")
    else:
        print("(dry run; pass --write to change the file)")
    return 0


def check_drawers(text: str, path: Path, column: str) -> None:
    """A rewrite that ends in `\\s*$` eats the newline before `:END:` and welds
    the drawer closed. The lever has carried that bug twice and grep caught it
    both times, not a test. So the lever now checks its own output."""
    for line in text.splitlines():
        if line.rstrip().endswith(":END:") and not line.strip().startswith(":END:"):
            raise SystemExit(
                f"{path}: '{column}' left a welded drawer: {line!r}")


def convert_baseline(args) -> int:
    """One command over every column the check still flags."""
    entries = []
    for line in args.baseline.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        path, _, column = line.partition("::")
        entries.append((Path(path), column))

    by_file: dict[Path, list[str]] = {}
    for path, column in entries:
        by_file.setdefault(path, []).append(column)

    hand = 0
    for path, columns in sorted(by_file.items()):
        text = path.read_text(encoding="utf-8")
        for column in columns:
            text, changes = convert(text, column, args.scale, args.to_float)
            print(f"{path.name}: {column} -> {', '.join(changes) if changes else 'no change'}")
            if any(c.startswith("NO generator block") for c in changes):
                hand += 1
        if args.write:
            path.write_text(text, encoding="utf-8")
            for column in columns:
                check_drawers(text, path, column)

    print(f"\n{len(entries)} columns over {len(by_file)} files; "
          f"{hand} need a hand-written mapper seam")
    if not args.write:
        print("(dry run; pass --write to change the files)")
    return 0



if __name__ == "__main__":
    sys.exit(main())
