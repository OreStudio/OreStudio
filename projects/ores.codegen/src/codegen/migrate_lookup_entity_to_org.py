"""
Convert ``*_entity.json`` codegen schema-model files (SQL-only
lookup entities) to literate ``ores.<component>.<name>_lookup_entity.org``
files under each owning component's ``modeling/`` directory.

Lookup entities are bi-temporal lookup tables (with the same DDL
shape as ``*_table.json``, but routed through codegen's "schema"
model_type — JSON root key ``entity``). Codegen runs them through
the ores.sql.schema address today (``sql_schema_table_create.mustache``), but
the JSON carries the full C++/Qt/protocol scalar vocabulary
(``entity_singular_upper``, ``entity_title``, ``component_include``,
...). The org file preserves every scalar verbatim so future
profiles work without re-introducing the JSON.

Usage::

    python3 projects/ores.codegen/src/codegen/migrate_lookup_entity_to_org.py \\
        --input-dir projects/ores.codegen/models/iam \\
        --output-dir projects/ores.iam/modeling \\
        --component iam
"""
import argparse
import json
import sys
import uuid
from datetime import date
from pathlib import Path


# Bookkeeping. Anything in here is rendered as #+key: value frontmatter.
# Anything not in here that's a scalar still goes into frontmatter too —
# this is just the ordering preference.
_FRONTMATTER_ORDER = (
    "product", "schema", "component", "component_include", "component_core",
    "entity_singular", "entity_singular_upper", "entity_singular_short",
    "entity_singular_words",
    "entity_plural", "entity_plural_short",
    "entity_plural_words", "entity_plural_words_cap",
    "entity_title", "entity_title_lower",
    "has_tenant_id", "system_tenant_validation", "has_display_order",
    "has_coding_scheme", "has_image_id", "has_artefact_insert_fn",
)


def _drawer(pairs):
    items = [(k, v) for k, v in pairs if v is not None and v != ""]
    if not items:
        return ""
    width = max(len(k) for k, _ in items)
    lines = [":PROPERTIES:"]
    for k, v in items:
        lines.append(f":{k}:{' ' * (width - len(k) + 1)}{v}")
    lines.append(":END:")
    return "\n".join(lines)


def _fmt_scalar(v):
    if isinstance(v, bool):
        return str(v).lower()
    return str(v)


def _frontmatter_block(entity, fid):
    slug = entity["entity_singular"]
    title = f"ores.{entity['component']}.{slug}"
    desc = f"Codegen lookup-entity model for {entity['entity_plural']}."
    today = date.today().isoformat()
    lines = [
        f"#+title: {title}",
        f"#+description: {desc}",
        f"#+type: ores.codegen.lookup_entity",
        f"#+filetags: :codegen:lookup_entity:literate:",
        f"#+created: {today}",
        f"#+updated: {today}",
    ]
    seen = set()
    for k in _FRONTMATTER_ORDER:
        if k in entity:
            lines.append(f"#+{k}: {_fmt_scalar(entity[k])}")
            seen.add(k)
    # Any other scalar keys (defensive: catch additions we didn't anticipate).
    for k, v in entity.items():
        if k in seen:
            continue
        if isinstance(v, (str, bool, int, float)):
            lines.append(f"#+{k}: {_fmt_scalar(v)}")
    return f":PROPERTIES:\n:ID: {fid}\n:END:\n" + "\n".join(lines)


def _primary_key_section(pk):
    drawer_keys = [
        ("column", pk.get("column")),
        ("type", pk.get("type")),
    ]
    if "is_text" in pk:
        drawer_keys.append(("is_text", str(pk["is_text"]).lower()))
    if "cpp_type" in pk:
        drawer_keys.append(("cpp_type", pk["cpp_type"]))
    return "* Primary key\n" + _drawer(drawer_keys)


def _column_section(col):
    drawer_keys = [("type", col.get("type"))]
    if "nullable" in col:
        drawer_keys.append(("nullable", str(col["nullable"]).lower()))
    if "default" in col:
        drawer_keys.append(("default", col["default"]))
    drawer = _drawer(drawer_keys)
    return f"** {col['name']}\n{drawer}"


def _columns_section(columns):
    return "* Columns\n\n" + "\n\n".join(_column_section(c) for c in columns)


def _org_table(rows, cols):
    if not rows:
        return ""
    widths = [len(c) for c in cols]
    rendered = []
    for row in rows:
        # Explicit None check — `or ""` would clobber legitimate falsy
        # values (0, False) by collapsing them to empty cells.
        cells = [str(row.get(c)) if row.get(c) is not None else "" for c in cols]
        for i, cell in enumerate(cells):
            widths[i] = max(widths[i], len(cell))
        rendered.append(cells)
    header = "| " + " | ".join(c.ljust(widths[i]) for i, c in enumerate(cols)) + " |"
    sep = "|-" + "-+-".join("-" * w for w in widths) + "-|"
    body = ["| " + " | ".join(cell.ljust(widths[i]) for i, cell in enumerate(cells)) + " |"
            for cells in rendered]
    return "\n".join([header, sep] + body)


def _validations_section(validations):
    if not validations:
        return ""
    cols = ["column", "validation_function"]
    return "* Validations\n\n" + _org_table(validations, cols)


def _indexes_section(indexes):
    if not indexes:
        return ""
    parts = ["* Indexes"]
    for idx in indexes:
        drawer_keys = [("columns", idx.get("columns"))]
        if "unique" in idx:
            drawer_keys.append(("unique", str(idx["unique"]).lower()))
        if "current_only" in idx:
            drawer_keys.append(("current_only", str(idx["current_only"]).lower()))
        drawer = _drawer(drawer_keys)
        parts.append(f"\n** {idx['name']}\n{drawer}")
    return "\n".join(parts)


def _artefact_indexes_section(indexes):
    if not indexes:
        return ""
    parts = ["* Artefact indexes"]
    for idx in indexes:
        drawer = _drawer([("columns", idx.get("columns"))])
        parts.append(f"\n** {idx['name']}\n{drawer}")
    return "\n".join(parts)


def convert(model_path, output_dir):
    model = json.loads(Path(model_path).read_text(encoding="utf-8"))
    entity = model["entity"]
    slug = entity["entity_singular"]
    fid = str(uuid.uuid4()).upper()

    parts = [_frontmatter_block(entity, fid), ""]
    description = entity.get("description", "").rstrip()
    if description:
        parts += [description, ""]
    parts += [_primary_key_section(entity["primary_key"]), ""]
    parts += [_columns_section(entity["columns"]), ""]
    vs = _validations_section(entity.get("validations", []))
    if vs:
        parts += [vs, ""]
    ix = _indexes_section(entity.get("indexes", []))
    if ix:
        parts += [ix, ""]
    ax = _artefact_indexes_section(entity.get("artefact_indexes", []))
    if ax:
        parts += [ax, ""]

    out_name = f"ores.{entity['component']}.{slug}_lookup_entity.org"
    out_path = output_dir / out_name
    out_path.write_text("\n".join(parts).rstrip() + "\n", encoding="utf-8")
    return out_path


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--input-dir", required=True, type=Path)
    p.add_argument("--output-dir", required=True, type=Path)
    p.add_argument("--component", required=True)
    args = p.parse_args(argv)

    args.output_dir.mkdir(parents=True, exist_ok=True)
    for json_path in sorted(args.input_dir.glob("*_entity.json")):
        # Skip domain entity files — they have a separate _domain_entity.json suffix.
        if json_path.name.endswith("_domain_entity.json"):
            continue
        out = convert(json_path, args.output_dir)
        print(f"Wrote {out}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
