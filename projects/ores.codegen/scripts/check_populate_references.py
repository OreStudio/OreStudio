#!/usr/bin/env python3
"""Check that every name a populate script looks up is a name some populate
script defines.

The seed data under projects/ores.sql/populate/ is a dependency graph carried
in string arguments: ores_dq_datasets_upsert_fn takes the name of a
methodology, ores_dq_tags_upsert_fn takes the name, subject area and domain of
a dataset. Each upsert resolves its names with a SELECT and raises "<thing>
not found" when the row is absent. setup_schema.sql runs psql with
ON_ERROR_STOP=on, so one unresolved name aborts the recreate and skips every
statement after it, including the GRANT block.

The contract is read from the function bodies under create/, not from a table
kept in this file. An upsert that raises on a miss proves the name is
mandatory, and the SELECT above that raise names the table and the columns
that identify the row.

Run::

    python3 projects/ores.codegen/scripts/check_populate_references.py
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SQL_ROOT = REPO_ROOT / "projects" / "ores.sql"
CREATE_DIR = SQL_ROOT / "create"
POPULATE_DIR = SQL_ROOT / "populate"

FUNCTION_RE = re.compile(r"create\s+or\s+replace\s+function\s+(\w+)\s*\(", re.I)
BODY_TAG_RE = re.compile(r"\bas\s+(\$[A-Za-z_]*\$)", re.I)
INSERT_RE = re.compile(r"insert\s+into\s+(\w+)\s*\(", re.I)
VALUES_RE = re.compile(r"\s*values\s*\(", re.I)
RAISE_RE = re.compile(r"raise\s+exception\s+'(?:[^']|'')*not\s+found", re.I)
SELECT_RE = re.compile(r"\bselect\b.*?\bfrom\s+(\w+)\s+where\b(.*?);", re.I | re.S)
COLUMN_PARAM_RE = re.compile(r"(\w+)\s*=\s*(p_\w+)", re.I)

# Tenant and validity columns scope a row rather than identify it, and the
# defining functions do not all take them as parameters.
NON_IDENTIFYING_COLUMNS = {"tenant_id", "valid_from", "valid_to"}


def blank_comments(text):
    """Replace comments with spaces, keeping every offset and line number."""
    chars = list(text)
    i, quote = 0, None
    while i < len(text):
        ch = text[i]
        if quote:
            if ch == quote:
                if i + 1 < len(text) and text[i + 1] == quote:
                    i += 2
                    continue
                quote = None
            i += 1
            continue
        if ch == "'":
            quote = ch
            i += 1
            continue
        if text.startswith("--", i):
            while i < len(text) and text[i] != "\n":
                chars[i] = " "
                i += 1
            continue
        if text.startswith("/*", i):
            while i < len(text) and not text.startswith("*/", i):
                if text[i] != "\n":
                    chars[i] = " "
                i += 1
            for _ in range(2):
                if i < len(text):
                    chars[i] = " "
                    i += 1
            continue
        i += 1
    return "".join(chars)


def split_top(text, sep=","):
    """Split on `sep` at paren depth zero, ignoring separators in strings."""
    parts, depth, quote, current = [], 0, None, []
    i = 0
    while i < len(text):
        ch = text[i]
        if quote:
            if ch == quote:
                if i + 1 < len(text) and text[i + 1] == quote:
                    current.append(ch)
                    i += 2
                    continue
                quote = None
            current.append(ch)
        elif ch in "'\"":
            quote = ch
            current.append(ch)
        elif ch in "([":
            depth += 1
            current.append(ch)
        elif ch in ")]":
            depth -= 1
            current.append(ch)
        elif ch == sep and depth == 0:
            parts.append("".join(current).strip())
            current = []
        else:
            current.append(ch)
        i += 1
    parts.append("".join(current).strip())
    return parts


def read_balanced(text, open_index):
    """Return the text inside the parentheses opened at `open_index`."""
    depth, i, quote = 0, open_index, None
    while i < len(text):
        ch = text[i]
        if quote:
            if ch == quote:
                if i + 1 < len(text) and text[i + 1] == quote:
                    i += 2
                    continue
                quote = None
            i += 1
            continue
        if ch in "'\"":
            quote = ch
        elif ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return text[open_index + 1 : i], i + 1
        i += 1
    raise ValueError("unbalanced parentheses")


def literal_value(argument):
    """The unescaped text of a single-quoted literal, or None."""
    if len(argument) < 2 or not argument.startswith("'") or not argument.endswith("'"):
        return None
    return argument[1:-1].replace("''", "'")


def parse_functions(text):
    """Function name -> (parameter names, body)."""
    functions = {}
    for match in FUNCTION_RE.finditer(text):
        name = match.group(1).lower()
        params_text, after_params = read_balanced(text, match.end() - 1)
        params = [p.split()[0].lower() for p in split_top(params_text) if p.strip()]
        tag_match = BODY_TAG_RE.search(text, after_params)
        if not tag_match:
            continue
        tag = tag_match.group(1)
        end = text.find(tag, tag_match.end())
        if end < 0:
            continue
        functions[name] = (params, text[tag_match.end() : end])
    return functions


def parse_guards(body):
    """[(table, [(column, parameter)])] for each mandatory name lookup."""
    guards = []
    for match in RAISE_RE.finditer(body):
        selects = list(SELECT_RE.finditer(body[: match.start()]))
        if not selects:
            continue
        table = selects[-1].group(1).lower()
        pairs = [
            (column.lower(), parameter.lower())
            for column, parameter in COLUMN_PARAM_RE.findall(selects[-1].group(2))
            if column.lower() not in NON_IDENTIFYING_COLUMNS
        ]
        if pairs:
            guards.append((table, pairs))
    return guards


def parse_inserts(body):
    """Table -> {column: value expression} for INSERTs that name columns."""
    inserts = {}
    for match in INSERT_RE.finditer(body):
        table = match.group(1).lower()
        columns_text, after_columns = read_balanced(body, match.end() - 1)
        values_match = VALUES_RE.match(body, after_columns)
        if not values_match:
            continue
        values_text, _ = read_balanced(body, values_match.end() - 1)
        columns = [c.strip().lower() for c in split_top(columns_text)]
        values = split_top(values_text)
        if len(columns) != len(values):
            continue
        inserts.setdefault(table, {}).update(zip(columns, values))
    return inserts


def parse_calls(text, names):
    """[(function, [arguments], line)] for calls to any of `names`."""
    if not names:
        return []
    alternation = "|".join(re.escape(n) for n in sorted(names, key=len, reverse=True))
    pattern = re.compile(r"\b(" + alternation + r")\s*\(", re.I)
    calls = []
    for match in pattern.finditer(text):
        arguments, _ = read_balanced(text, match.end() - 1)
        calls.append(
            (
                match.group(1).lower(),
                split_top(arguments),
                text.count("\n", 0, match.start()) + 1,
            )
        )
    return calls


def call_tuples(calls, name, positions):
    """[(values, path, line)] for literal arguments at `positions`."""
    tuples = []
    for path, function, arguments, line in calls:
        if function != name:
            continue
        if any(position >= len(arguments) for position in positions):
            continue
        values = [literal_value(arguments[position]) for position in positions]
        if any(value is None for value in values):
            continue
        tuples.append((tuple(values), path, line))
    return tuples


def defining_functions(functions, inserts, table, columns):
    """[(function, [positions])] whose INSERT defines a row of `table`."""
    found = []
    for name, (params, _) in functions.items():
        values = inserts.get(name, {}).get(table)
        if not values:
            continue
        positions = []
        for column in columns:
            expression = values.get(column)
            if expression is None or expression.lower() not in params:
                positions = None
                break
            positions.append(params.index(expression.lower()))
        if positions:
            found.append((name, positions))
    return found


def check(create_dir=CREATE_DIR, populate_dir=POPULATE_DIR):
    """Return a list of (file, line, message) violations."""
    create_text = "\n".join(
        blank_comments(p.read_text()) for p in sorted(create_dir.rglob("*.sql"))
    )
    functions = parse_functions(create_text)
    if not functions:
        raise SystemExit(f"no function definitions found under {create_dir}")

    guards = {name: parse_guards(body) for name, (_, body) in functions.items()}
    inserts = {name: parse_inserts(body) for name, (_, body) in functions.items()}
    guarded_tables = {table for found in guards.values() for table, _ in found}

    # Only these functions matter at a call site: the ones that look a name up,
    # and the ones that write a row into a table another function looks up.
    interesting = {name for name, found in guards.items() if found}
    interesting.update(
        name
        for name, tables in inserts.items()
        if guarded_tables.intersection(tables)
    )

    calls = []
    for path in sorted(populate_dir.rglob("*.sql")):
        for function, arguments, line in parse_calls(
            blank_comments(path.read_text()), interesting
        ):
            calls.append((path, function, arguments, line))

    violations = []
    defining_cache = {}
    for name, (params, _) in sorted(functions.items()):
        for table, pairs in guards[name]:
            reference_positions = [params.index(p) for _, p in pairs if p in params]
            if len(reference_positions) != len(pairs):
                continue
            columns = [column for column, _ in pairs]

            key = (table, tuple(columns))
            if key not in defining_cache:
                defining_cache[key] = defining_functions(
                    functions, inserts, table, columns
                )

            definitions = set()
            for candidate, positions in defining_cache[key]:
                for values, _, _ in call_tuples(calls, candidate, positions):
                    definitions.add(values)

            for values, path, line in call_tuples(calls, name, reference_positions):
                if values not in definitions:
                    violations.append(
                        (
                            path,
                            line,
                            f"{name}({', '.join(p for _, p in pairs)}) looks up "
                            f"{table} ({', '.join(columns)}) but no populate "
                            f"script defines ({', '.join(values)})",
                        )
                    )
    return violations


def main():
    violations = check()
    for path, line, message in violations:
        print(f"{path.relative_to(REPO_ROOT)}:{line}: {message}")
    if violations:
        print(f"\n{len(violations)} unresolved populate reference(s).")
        return 1
    print("populate references resolve.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
