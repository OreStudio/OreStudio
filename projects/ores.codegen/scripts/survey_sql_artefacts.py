#!/usr/bin/env python3
"""
Print the artefact-table census for one SQL product as Markdown.

The DQ component stages imported reference data in a parallel family of
``*_artefact_tbl`` tables, one per published entity. The clean standard's
B01 and B03 items ask what that family is made of before any edit is
made, and the interesting question is not the count. It is whether each
artefact table is a projection of the entity it stages, which decides
whether the table can generate from the entity's model or needs one.

Pairing is by entity name, never by filename or by prefix. A staging
table is ``ores_dq_<entity>_artefact_tbl``, but the table it stages
carries the owning component's prefix: ``ores_refdata_<entity>_tbl``,
``ores_synthetic_<entity>_tbl``, and so on. DQ stages entities that other
components own, so a prefix match would report most of the family as
having no base at all. The search therefore reads every product in the
create tree and matches on the ``_<entity>_tbl`` suffix.

Every artefact table is placed in one of five buckets:

  generated     the file carries the AUTO-GENERATED FILE marker.
  derivable     hand-written, and needs no model change to generate: the
                artefact columns are exactly the base columns minus the
                bitemporal and audit set, plus dataset_id. Any extra
                artefact index is already expressible as an
                ``artefact_indexes`` entry.
  divergent     hand-written, and the artefact columns carry a column the
                base keeps out, or drop one the base has. The artefact
                table is not a pure projection, so a model cannot
                generate it until the difference is resolved.
  foreign       hand-written, and the base table belongs to another
                product. The model to extend is that product's, while the
                output still lands in the dq product.
  orphan        hand-written, with no base table of the matching entity
                name in any product.

Read-only. Nothing is written, no file is created, and no model,
catalogue entry or generated file is touched.

Usage:
  survey_sql_artefacts.py
  survey_sql_artefacts.py --product dq
  survey_sql_artefacts.py --product dq -v
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
CREATE_ROOT = REPO_ROOT / "projects" / "ores.sql" / "create"

# A CREATE TABLE statement, its quoted table name, and its body.
_CREATE_RE = re.compile(
    r"create\s+table\s+(?:if\s+not\s+exists\s+)?\"(?P<table>[^\"]+)\"\s*\((?P<body>.*?)\)\s*;",
    re.IGNORECASE | re.DOTALL,
)

# The exact marker every generated file carries.
_GENERATED_MARKER = "AUTO-GENERATED FILE"

# Columns a bitemporal entity keeps that its staging table does not: the
# audit trail and the validity window. The staging table re-declares
# tenant and version in its own header, and carries dataset_id in place
# of the change-reason pair.
_AUDIT_COLUMNS = frozenset(
    {
        "modified_by",
        "performed_by",
        "change_reason_code",
        "change_commentary",
        "valid_from",
        "valid_to",
    }
)

# Declared by the artefact header itself rather than by the projection.
_ARTEFACT_HEADER_COLUMNS = ("dataset_id", "tenant_id", "version")

_ARTEFACT_SUFFIX = "_artefact_tbl"


def is_generated(text: str) -> bool:
    """True when the file's own header carries the generated marker."""
    return _GENERATED_MARKER in text[:2000]


def strip_sql_comments(text: str) -> str:
    """Drop -- to end-of-line and /* */ comments, so a column list parses."""
    text = re.sub(r"/\*.*?\*/", " ", text, flags=re.DOTALL)
    return re.sub(r"--[^\n]*", " ", text)


def split_top_level(body: str) -> list:
    """Split a CREATE TABLE body on top-level commas only.

    A CREATE TABLE body nests inside parentheses for CHECK, EXCLUDE and
    table constraints. A comma inside those must not split a column, so
    the split tracks the nesting depth.
    """
    parts: list = []
    depth = 0
    current: list = []
    for char in body:
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
        if char == "," and depth == 0:
            parts.append("".join(current))
            current = []
            continue
        current.append(char)
    parts.append("".join(current))
    return parts


def column_names(body: str) -> list:
    """The quoted column names of one CREATE TABLE body, in order.

    A part that opens with a constraint keyword declares no column.
    """
    names: list = []
    for part in split_top_level(body):
        stripped = part.strip()
        if not stripped:
            continue
        first = stripped.split(None, 1)[0].lower()
        if first in ("primary", "unique", "check", "constraint", "exclude", "foreign"):
            continue
        match = re.match(r"\"(?P<name>[^\"]+)\"", stripped)
        if match:
            names.append(match.group("name"))
    return names


def parse_tables(create_dir: Path) -> dict:
    """Every CREATE TABLE in one product, keyed by table name.

    A table declared twice keeps the first declaration and records the
    second file, because a duplicate is itself a finding.
    """
    tables: dict = {}
    for path in sorted(create_dir.glob("*.sql")):
        if not path.is_file():
            continue
        text = path.read_text(encoding="utf-8")
        bare = strip_sql_comments(text)
        for match in _CREATE_RE.finditer(bare):
            table = match.group("table")
            entry = tables.setdefault(
                table,
                {
                    "file": path,
                    "columns": column_names(match.group("body")),
                    "generated": is_generated(text),
                    "duplicates": [],
                },
            )
            if entry["file"] != path:
                entry["duplicates"].append(path.name)
    return tables


def parse_all_products() -> dict:
    """Every CREATE TABLE in every product, keyed by product then table."""
    products: dict = {}
    for create_dir in sorted(p for p in CREATE_ROOT.iterdir() if p.is_dir()):
        products[create_dir.name] = parse_tables(create_dir)
    return products


_ENTITY_PLURAL_RE = re.compile(r"^#\+entity_plural:\s*(?P<v>.+?)\s*$", re.MULTILINE)
_TYPE_RE = re.compile(r"^#\+type:\s*(?P<v>.+?)\s*$", re.MULTILINE)
_COMPONENT_RE = re.compile(r"^#\+component:\s*(?P<v>.+?)\s*$", re.MULTILINE)

# Model metatypes that can carry an artefact table. A message or a
# component describes no table, so it cannot project one.
_ARTEFACT_CARRYING_TYPES = ("ores.codegen.entity", "ores.codegen.lookup_entity", "ores.codegen.domain_entity")


def parse_models() -> dict:
    """Every codegen model that declares an entity_plural, keyed by plural.

    The artefact table's entity is the plural name, so the plural is the
    join key between the SQL tree and the model tree.
    """
    models: dict = {}
    for path in sorted(REPO_ROOT.glob("projects/*/modeling/*.org")):
        text = path.read_text(encoding="utf-8")
        plural = _ENTITY_PLURAL_RE.search(text)
        if not plural:
            continue
        type_match = _TYPE_RE.search(text)
        component_match = _COMPONENT_RE.search(text)
        models.setdefault(
            plural.group("v"),
            {
                "file": path,
                "type": (type_match.group("v") if type_match else "?"),
                "component": (component_match.group("v") if component_match else "?"),
            },
        )
    return models


def entity_of(artefact_table: str) -> str:
    """``ores_dq_currencies_artefact_tbl`` -> ``currencies``."""
    name = artefact_table
    if name.startswith("ores_"):
        name = name[len("ores_"):]
    if name.startswith("dq_"):
        name = name[len("dq_"):]
    return name[: -len(_ARTEFACT_SUFFIX)]


def find_bases(entity: str, products: dict, product: str, artefact_table: str) -> list:
    """Every table in any product that publishes ``entity``.

    A published table is ``ores_<component>_<entity>_tbl``, where the
    component is a single token, so the whole name is deconstructed
    rather than suffix-matched. A suffix match would pair ``tags`` with
    ``image_tags``. The staging table's own product is searched first, so
    a local base is preferred and reported first.
    """
    remainder_expected = f"{entity}_tbl"
    order = [product, *[p for p in sorted(products) if p != product]]
    hits: list = []
    for name in order:
        for table, entry in sorted(products.get(name, {}).items()):
            if table == artefact_table or not table.startswith("ores_"):
                continue
            parts = table[len("ores_"):].split("_", 1)
            if len(parts) != 2 or parts[1] != remainder_expected:
                continue
            hits.append((name, table, entry))
    return hits


def classify(artefact: dict, bases: list, product: str) -> tuple:
    """Return (bucket, detail) for one artefact table.

    A generated table is bucketed as generated whatever its columns say,
    but its detail still names the base. The liveness question does not
    stop at the marker: 13 generated tables have no model, and whether
    the base table exists decides what may be done with them.
    """
    if artefact["generated"]:
        if not bases:
            return "generated", "no table of the matching entity name in any product"
        base_product, base_table, _ = bases[0]
        if base_product == product:
            return "generated", f"base {base_table}"
        return "generated", f"base {base_table} in {base_product}"

    if not bases:
        return "orphan", "no table of the matching entity name in any product"

    local = [b for b in bases if b[0] == product]
    base_product, base_table, base = local[0] if local else bases[0]

    art_cols = list(artefact["columns"])
    expected = [c for c in base["columns"] if c not in _AUDIT_COLUMNS]
    for header in _ARTEFACT_HEADER_COLUMNS:
        if header not in expected:
            expected.insert(0, header)

    extra = [c for c in art_cols if c not in expected]
    missing = [c for c in expected if c not in art_cols]

    notes: list = []
    if base_product != product:
        notes.append(f"base {base_table} in {base_product}")
    if extra:
        notes.append("extra: " + ", ".join(extra))
    if missing:
        notes.append("missing: " + ", ".join(missing))

    if not extra and not missing:
        owner = "" if base_product == product else f"base {base_table} in {base_product}"
        return ("derivable" if base_product == product else "foreign"), owner
    return "divergent", "; ".join(notes)


def markdown_row(cells: list) -> str:
    return "| " + " | ".join(str(c) for c in cells) + " |"


def relative(path: Path) -> str:
    try:
        return str(path.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument(
        "--product",
        default="dq",
        help="SQL product under projects/ores.sql/create (default: dq)",
    )
    ap.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="list every table, not only the findings",
    )
    args = ap.parse_args()

    create_dir = CREATE_ROOT / args.product
    if not create_dir.is_dir():
        print(f"no such product: {create_dir}", file=sys.stderr)
        return 1

    products = parse_all_products()
    tables = products[args.product]
    artefacts = {n: e for n, e in tables.items() if n.endswith(_ARTEFACT_SUFFIX)}

    buckets: dict = {k: [] for k in ("generated", "derivable", "divergent", "foreign", "orphan")}
    for name in sorted(artefacts):
        entry = artefacts[name]
        bases = find_bases(entity_of(name), products, args.product, name)
        bucket, detail = classify(entry, bases, args.product)
        buckets[bucket].append((name, entry, detail))

    print(f"# SQL artefact census: {relative(create_dir)}")
    print()
    print(f"Tables declared: {len(tables)}. Artefact tables: {len(artefacts)}.")
    print()
    print("| bucket | artefact tables | meaning |")
    print("|--------|-----------------|---------|")
    meanings = {
        "generated": "the file already carries the generated marker",
        "derivable": "hand-written, but a pure projection of a base table in this product",
        "divergent": "hand-written, and not a pure projection of its base table",
        "foreign": "a pure projection, but the base table belongs to another product",
        "orphan": "no table of the matching entity name anywhere",
    }
    for bucket in ("generated", "derivable", "divergent", "foreign", "orphan"):
        print(f"| {bucket} | {len(buckets[bucket])} | {meanings[bucket]} |")
    print()

    for bucket in ("divergent", "orphan", "foreign", "derivable", "generated"):
        rows = buckets[bucket]
        if not rows:
            continue
        if bucket == "derivable" and not args.verbose:
            continue
        unattached = [r for r in rows if bucket == "generated" and "no table of the matching" in r[2]]
        if bucket == "generated" and not args.verbose and not unattached:
            continue
        print(f"## {bucket} ({len(rows)})")
        print()
        print("| artefact table | file | detail |")
        print("|----------------|------|--------|")
        for name, entry, detail in rows:
            print(markdown_row([name, relative(entry["file"]), detail or "-"]))
        print()

    duplicates = {n: e for n, e in tables.items() if e["duplicates"]}
    if duplicates:
        print(f"## duplicated declarations ({len(duplicates)})")
        print()
        print("| table | first file | also declared in |")
        print("|-------|------------|------------------|")
        for name, entry in sorted(duplicates.items()):
            print(
                markdown_row(
                    [
                        name,
                        relative(entry["file"]),
                        ", ".join(sorted(set(entry["duplicates"]))),
                    ]
                )
            )
        print()

    models = parse_models()
    print("## models behind each artefact table")
    print()
    print("| entity | artefact bucket | model | type | component |")
    print("|--------|-----------------|-------|------|-----------|")
    for name in sorted(artefacts):
        entity = entity_of(name)
        entry = artefacts[name]
        bases = find_bases(entity, products, args.product, name)
        bucket, _ = classify(entry, bases, args.product)
        model = models.get(entity)
        if model:
            print(
                markdown_row(
                    [
                        entity,
                        bucket,
                        relative(model["file"]),
                        model["type"].replace("ores.codegen.", ""),
                        model["component"],
                    ]
                )
            )
        else:
            print(markdown_row([entity, bucket, "MISSING", "-", "-"]))
    print()

    return 0


if __name__ == "__main__":
    sys.exit(main())
