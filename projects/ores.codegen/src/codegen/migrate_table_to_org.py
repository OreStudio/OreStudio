"""
Convert ``*_table.json`` codegen models to literate
``ores.<component>.<name>_table.org`` files under each owning
component's ``modeling/`` directory.

Table models are bi-temporal reference-data tables (lookup
tables). Codegen runs them only through the ores.sql.schema address today
(one ``*_create.sql`` per table under
``projects/ores.sql/create/<component>/``, plus
``ores_<component>_validate_<entity>_fn`` when
``validation_fn`` is set).

The org file mirrors the JSON one-for-one: frontmatter for the
entity-level scalars; ``* Primary key`` with a property drawer;
``* Columns`` sub-headings (one per column) with their drawers;
``* Validation function`` drawer; ``* Insert trigger ** Validations``
as an org table; ``* Check constraints`` sub-headings when present;
``* Indexes`` sub-headings when present.

Usage::

    python3 projects/ores.codegen/src/codegen/migrate_table_to_org.py \\
        --input-dir projects/ores.codegen/models/refdata \\
        --output-dir projects/ores.refdata/modeling \\
        --component refdata
"""
import argparse
import json
import sys
import uuid
from datetime import date
from pathlib import Path


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


def _frontmatter_block(name, slug, table, fid):
    title = f"ores.{table['component']}.{slug}"
    desc = f"Codegen table model for {table['entity_plural']}."
    today = date.today().isoformat()
    fm = [
        f"#+title: {title}",
        f"#+description: {desc}",
        f"#+type: ores.codegen.table",
        f"#+filetags: :codegen:table:literate:",
        f"#+created: {today}",
        f"#+updated: {today}",
        f"#+product: {table.get('product', 'ores')}",
        f"#+schema: {table.get('schema', 'public')}",
        f"#+component: {table['component']}",
        f"#+entity_singular: {table['entity_singular']}",
        f"#+entity_plural: {table['entity_plural']}",
        f"#+has_tenant_id: {str(bool(table.get('has_tenant_id', True))).lower()}",
        f"#+coding_scheme: {table.get('coding_scheme', 'none')}",
        f"#+image_id: {str(bool(table.get('image_id', False))).lower()}",
    ]
    return f":PROPERTIES:\n:ID: {fid}\n:END:\n" + "\n".join(fm)


def _primary_key_section(pk):
    drawer_keys = [
        ("column", pk.get("column")),
        ("type", pk.get("type")),
    ]
    if "is_text" in pk:
        drawer_keys.append(("is_text", str(pk["is_text"]).lower()))
    return "* Primary key\n" + _drawer(drawer_keys)


def _column_section(col):
    drawer_keys = [
        ("type", col.get("type")),
    ]
    if "nullable" in col:
        drawer_keys.append(("nullable", str(col["nullable"]).lower()))
    if "default" in col:
        drawer_keys.append(("default", col["default"]))
    drawer = _drawer(drawer_keys)
    return f"** {col['name']}\n{drawer}"


def _columns_section(columns):
    return "* Columns\n\n" + "\n\n".join(_column_section(c) for c in columns)


def _validation_fn_section(vfn):
    drawer_keys = [
        ("tenant_scope", vfn.get("tenant_scope")),
    ]
    if "default" in vfn:
        drawer_keys.append(("default", vfn["default"]))
    if "order_by" in vfn:
        drawer_keys.append(("order_by", vfn["order_by"]))
    return "* Validation function\n" + _drawer(drawer_keys)


def _table(rows, cols):
    if not rows:
        return ""
    widths = [len(c) for c in cols]
    rendered = []
    for row in rows:
        cells = [str(row.get(c, "") or "") for c in cols]
        for i, cell in enumerate(cells):
            widths[i] = max(widths[i], len(cell))
        rendered.append(cells)
    header = "| " + " | ".join(c.ljust(widths[i]) for i, c in enumerate(cols)) + " |"
    sep = "|-" + "-+-".join("-" * w for w in widths) + "-|"
    body = ["| " + " | ".join(cell.ljust(widths[i]) for i, cell in enumerate(cells)) + " |"
            for cells in rendered]
    return "\n".join([header, sep] + body)


def _insert_trigger_section(it):
    validations = it.get("validations", [])
    if not validations:
        return ""
    cols = ["column", "validation_function"]
    table_str = _table(validations, cols)
    return "* Insert trigger\n** Validations\n\n" + table_str


def _check_constraints_section(constraints):
    if not constraints:
        return ""
    parts = ["* Check constraints"]
    for i, c in enumerate(constraints, 1):
        name = c.get("name", f"check_{i}")
        drawer = _drawer([("expression", c["expression"])])
        parts.append(f"\n** {name}\n{drawer}")
    return "\n".join(parts)


def _indexes_section(indexes):
    if not indexes:
        return ""
    parts = ["* Indexes"]
    for idx in indexes:
        drawer_keys = [
            ("columns", idx.get("columns")),
        ]
        if "unique" in idx:
            drawer_keys.append(("unique", str(idx["unique"]).lower()))
        if "current_only" in idx:
            drawer_keys.append(("current_only", str(idx["current_only"]).lower()))
        drawer = _drawer(drawer_keys)
        parts.append(f"\n** {idx['name']}\n{drawer}")
    return "\n".join(parts)


def convert(model_path, output_dir):
    model = json.loads(Path(model_path).read_text(encoding="utf-8"))
    table = model["table"]
    slug = table["entity_singular"]
    fid = str(uuid.uuid4()).upper()

    parts = [_frontmatter_block(model_path.stem.replace("_table", ""), slug, table, fid), ""]
    description = table.get("description", "").rstrip()
    if description:
        parts += [description, ""]
    parts += [_primary_key_section(table["primary_key"]), ""]
    parts += [_columns_section(table["columns"]), ""]
    if "validation_fn" in table:
        parts += [_validation_fn_section(table["validation_fn"]), ""]
    it = table.get("insert_trigger")
    if it:
        s = _insert_trigger_section(it)
        if s:
            parts += [s, ""]
    cc = _check_constraints_section(table.get("check_constraints", []))
    if cc:
        parts += [cc, ""]
    ix = _indexes_section(table.get("indexes", []))
    if ix:
        parts += [ix, ""]

    out_name = f"ores.{table['component']}.{slug}_table.org"
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
    written = []
    for json_path in sorted(args.input_dir.glob("*_table.json")):
        out = convert(json_path, args.output_dir)
        written.append(out)
        print(f"Wrote {out}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
