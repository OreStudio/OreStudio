"""
Convert ``*_domain_entity.json`` codegen models to literate
``ores.<component>.<entity>.org`` files matching the org-entity
meta-model (BDE309BA-71DA-4A52-AA10-00F17AFDA476).

Per-entity flow:

  1. Read the JSON model.
  2. Render the org-mode body section by section, copying every field
     the meta-model defines a slot for.
  3. Write to ``<output-dir>/ores.<component>.<entity>.org``.

The script is intentionally one-shot: a single tool run produces the
24 refdata files, the operator hand-massages prose where needed, and
the JSON files are deleted in the same PR. The script lives on for
the other per-component migrations that follow.

Out of scope:

- ``* Custom repository methods``. The block-paste mechanism needs
  per-method UUIDs and prose context that the JSON does not supply.
  Custom methods are hand-authored after the script runs.
- Refdata-cpp drift flags (``service_find_by_uuid``, etc.). Those
  were never in the JSON; they are added in the subsequent refdata-cpp
  drift task.
"""
import argparse
import json
import sys
import uuid
from datetime import date
from pathlib import Path


_META_MODEL_ID = "BDE309BA-71DA-4A52-AA10-00F17AFDA476"


def _drawer(props, indent=""):
    """Render a :PROPERTIES: drawer from a list of (key, value) pairs."""
    if not props:
        return ""
    width = max(len(k) for k, _ in props)
    lines = [f"{indent}:PROPERTIES:"]
    for key, value in props:
        if value is None or value == "":
            continue
        lines.append(f"{indent}:{key}:{' ' * (width - len(key) + 1)}{value}")
    lines.append(f"{indent}:END:")
    return "\n".join(lines)


def _prose(text):
    """Return prose with trailing newlines normalised."""
    if not text:
        return ""
    return text.rstrip()


def _description_and_detail(node):
    """Concatenate description + detail with a paragraph break."""
    parts = []
    if node.get("description"):
        parts.append(_prose(node["description"]))
    if node.get("detail"):
        parts.append(_prose(node["detail"]))
    return "\n\n".join(parts)


def _generator_block(node, name="generator"):
    """Render a babel src block for a generator_expr, or empty."""
    expr = node.get("generator_expr")
    if not expr:
        return ""
    return f"#+begin_src cpp :name {name}\n{expr}\n#+end_src"


_COLUMN_BODY_KEYS = {"description", "detail", "generator_expr"}
_COLUMN_HEADER_KEYS = {"name", "column", "type", "cpp_type"}


def _column_section(node, level):
    """Render a column-like sub-heading (natural key or column)."""
    title = node.get("name") or node.get("column")
    pairs = [("type", node.get("type")), ("cpp_type", node.get("cpp_type"))]
    if "nullable" in node:
        pairs.append(("nullable", str(node["nullable"]).lower()))
    # Forward any other scalar column properties (default, default_value,
    # SQL-side annotations) so the org_loader can re-emit them via the
    # generic column-property pass.
    for k, v in node.items():
        if k in _COLUMN_HEADER_KEYS or k in _COLUMN_BODY_KEYS or k == "nullable":
            continue
        if isinstance(v, (list, dict)):
            continue
        if v is None:
            continue
        if isinstance(v, bool):
            v = "true" if v else "false"
        pairs.append((k, v))
    drawer = _drawer(pairs)
    body_parts = [_description_and_detail(node)]
    gen = _generator_block(node)
    if gen:
        body_parts.append(gen)
    body = "\n\n".join(part for part in body_parts if part)
    pieces = [f"{'*' * level} {title}", drawer]
    if body:
        pieces.append("")
        pieces.append(body)
    return "\n".join(pieces)


def _table(rows, columns):
    """Render an org table from a list of dicts and column ordering."""
    if not rows:
        return ""
    widths = [len(col) for col in columns]
    rendered_rows = []
    for row in rows:
        cells = []
        for i, col in enumerate(columns):
            value = row.get(col, "")
            if isinstance(value, bool):
                value = "true" if value else "false"
            cell = str(value) if value is not None else ""
            cells.append(cell)
            widths[i] = max(widths[i], len(cell))
        rendered_rows.append(cells)
    header = "| " + " | ".join(c.ljust(widths[i]) for i, c in enumerate(columns)) + " |"
    sep = "|-" + "-+-".join("-" * w for w in widths) + "-|"
    body_lines = [
        "| " + " | ".join(cell.ljust(widths[i]) for i, cell in enumerate(cells)) + " |"
        for cells in rendered_rows
    ]
    return "\n".join([header, sep, *body_lines])


def _qt_drawer_keys(qt):
    """Subset of qt keys that land in the drawer, in display order."""
    return [
        "domain_include",
        "domain_class",
        "protocol_include",
        "collection_name",
        "item_var",
        "key_field",
        "key_widget",
        "has_uuid_primary_key",
        "has_pagination",
        "has_history",
        "get_request_class",
        "get_response_class",
        "get_message_type",
        "save_request_class",
        "save_response_class",
        "save_message_type",
        "save_request_item_field",
        "delete_request_class",
        "delete_response_class",
        "delete_message_type",
        "history_request_class",
        "history_response_class",
        "history_message_type",
        "settings_group",
        "window_title",
        "icon",
    ]


def _qt_drawer_pairs(qt):
    pairs = []
    for k in _qt_drawer_keys(qt):
        if k in qt:
            value = qt[k]
            if isinstance(value, bool):
                value = "true" if value else "false"
            pairs.append((k, value))
    return pairs


def _frontmatter(model, slug, today, new_id, description_kw):
    de = model
    lines = [
        ":PROPERTIES:",
        f":ID: {new_id}",
        ":END:",
        f"#+title: ores.{de['component']}.{slug}",
        f"#+description: {description_kw}",
        "#+type: ores.codegen.entity",
        f"#+component: {de['component']}",
        f"#+filetags: :model:entity:{de['component']}:",
    ]
    if "brief" in de:
        lines.append(f"#+brief: {de['brief']}")
    if "entity_singular" in de:
        lines.append(f"#+entity_singular: {de['entity_singular']}")
    if "entity_plural" in de:
        lines.append(f"#+entity_plural: {de['entity_plural']}")
    if "entity_title" in de:
        lines.append(f"#+entity_title: {de['entity_title']}")
    lines.append(f"#+created: {today}")
    lines.append(f"#+updated: {today}")
    return "\n".join(lines)


def _flags_section(de):
    # The meta-model requires schema/product/component. A handful of
    # legacy JSON lookup-table models omitted product; default it to
    # "ores" since every entity in the project belongs to that product.
    pairs = [
        ("schema", de.get("schema") or "public"),
        ("product", de.get("product") or "ores"),
        ("component", de.get("component")),
    ]
    if "has_tenant_id" in de:
        pairs.append(("has_tenant_id", str(de["has_tenant_id"]).lower()))
    if "has_workspace_id" in de:
        pairs.append(("has_workspace_id", str(de["has_workspace_id"]).lower()))
    drawer = _drawer(pairs)
    return f"* Flags\n{drawer}"


_PK_BODY_KEYS = {"description", "detail", "generator_expr"}


def _primary_key_section(de):
    pk = de.get("primary_key", {})
    pairs = [
        ("column", pk.get("column")),
        ("type", pk.get("type")),
        ("cpp_type", pk.get("cpp_type")),
    ]
    # Forward any other scalar properties on the primary key (e.g.
    # skip_uuid_check) so the org_loader can re-emit them. Sequences and
    # mappings stay out of the drawer; they would need section structure.
    for k, v in pk.items():
        if k in {"column", "type", "cpp_type"} or k in _PK_BODY_KEYS:
            continue
        if isinstance(v, (list, dict)):
            continue
        if isinstance(v, bool):
            v = "true" if v else "false"
        pairs.append((k, v))
    drawer = _drawer(pairs)
    body_parts = [_description_and_detail(pk)]
    gen = _generator_block(pk)
    if gen:
        body_parts.append(gen)
    body = "\n\n".join(part for part in body_parts if part)
    pieces = ["* Primary key", drawer]
    if body:
        pieces.append("")
        pieces.append(body)
    return "\n".join(pieces)


def _natural_keys_section(de):
    keys = de.get("natural_keys", []) or []
    lines = ["* Natural keys"]
    if keys:
        for k in keys:
            lines.append("")
            lines.append(_column_section(k, level=2))
    return "\n".join(lines)


def _columns_section(de):
    cols = de.get("columns", []) or []
    lines = ["* Columns"]
    if cols:
        for c in cols:
            lines.append("")
            lines.append(_column_section(c, level=2))
    return "\n".join(lines)


def _sql_section(de):
    sql = de.get("sql", {}) or {}
    pairs = [("tablename", sql.get("tablename"))]
    # Forward additional scalar SQL flags (system_scope, etc.). Array- and
    # mapping-valued fields (indexes, extra_checks, text_code_validations,
    # extra_delete_sets) need richer org structure and an org_loader
    # extension; they are deferred to a follow-up task.
    for k, v in sql.items():
        if k == "tablename":
            continue
        if isinstance(v, (list, dict)):
            continue
        if isinstance(v, bool):
            v = "true" if v else "false"
        pairs.append((k, v))
    drawer = _drawer(pairs)
    return f"* SQL\n\n** Flags\n{drawer}"


def _includes_block(includes):
    if not includes:
        return ""
    body_lines = []
    for inc in includes:
        s = inc.strip()
        if s.startswith("<") and s.endswith(">"):
            body_lines.append(f"#include {s}")
        elif s.startswith('"') and s.endswith('"'):
            body_lines.append(f"#include {s}")
        elif s.startswith("#include"):
            body_lines.append(s)
        else:
            body_lines.append(f"#include {s}")
    body = "\n".join(body_lines)
    return f"#+begin_src cpp :name includes\n{body}\n#+end_src"


def _qt_type_string(field):
    """Render the qt-columns 'type' cell from the is_X family of flags."""
    if field.get("is_string"):
        return "string"
    if field.get("is_int"):
        return "int"
    if field.get("is_timestamp"):
        return "timestamp"
    if field.get("is_uuid"):
        return "uuid"
    if field.get("is_bool"):
        return "bool"
    return ""


def _cpp_section(de):
    cpp = de.get("cpp", {}) or {}
    repo = de.get("repository", {}) or {}
    qt = de.get("qt", {}) or {}

    parts = ["* C++"]

    cpp_flag_pairs = []
    if "component_include" in de:
        cpp_flag_pairs.append(("component_include", de["component_include"]))
    if "component_core" in de:
        cpp_flag_pairs.append(("component_core", de["component_core"]))
    if "has_batch_remove" in de:
        cpp_flag_pairs.append(("has_batch_remove", str(de["has_batch_remove"]).lower()))
    if "has_batch_read" in de:
        cpp_flag_pairs.append(("has_batch_read", str(de["has_batch_read"]).lower()))
    if "generator_facet_name" in de:
        cpp_flag_pairs.append(("generator_facet_name", de["generator_facet_name"]))
    if "list_filter_column" in de:
        cpp_flag_pairs.append(("list_filter_column", de["list_filter_column"]))
    if cpp_flag_pairs:
        parts.append("")
        parts.append(f"** Flags\n{_drawer(cpp_flag_pairs)}")

    repo_pairs = [
        ("entity_singular_short", repo.get("entity_singular_short")),
        ("entity_plural_short", repo.get("entity_plural_short")),
        ("entity_singular_words", repo.get("entity_singular_words")),
        ("entity_plural_words", repo.get("entity_plural_words")),
    ]
    if any(v for _, v in repo_pairs):
        parts.append("")
        parts.append(f"** Repository\n{_drawer(repo_pairs)}")

    domain_includes = ((cpp.get("includes") or {}).get("domain") or [])
    if domain_includes:
        parts.append("")
        parts.append("** Domain includes")
        parts.append("")
        parts.append(_includes_block(domain_includes))

    entity_includes = ((cpp.get("includes") or {}).get("entity") or [])
    if entity_includes:
        parts.append("")
        parts.append("** Entity includes")
        parts.append("")
        parts.append(_includes_block(entity_includes))

    if "iterator_var" in cpp:
        parts.append("")
        parts.append(f"** Conventions\n{_drawer([('iterator_var', cpp['iterator_var'])])}")

    table_display = cpp.get("table_display") or []
    if table_display:
        parts.append("")
        parts.append("** Table display")
        parts.append("")
        parts.append(_table(table_display, ["column", "header"]))

    if qt:
        parts.append("")
        parts.append(f"** Qt\n{_drawer(_qt_drawer_pairs(qt))}")

        detail_fields = qt.get("detail_fields") or []
        if detail_fields:
            parts.append("")
            parts.append("*** Detail fields")
            parts.append("")
            base_cols = ["field", "label", "widget", "type",
                         "is_key", "is_required", "placeholder"]
            # Dynamic-combo fields carry a fixed family of combo_* scalar
            # properties. Include those columns when any field uses them so
            # the table preserves per-field combo wiring. Static-combo
            # `combo_values` (list of {label,value} dicts) needs richer
            # structure — deferred to the SQL/array-fields follow-up task.
            combo_cols = [
                "combo_type", "combo_include", "combo_display", "combo_value",
                "combo_is_uuid", "combo_is_optional", "combo_setter",
                "combo_setter_pascal", "combo_items_member",
            ]
            extra_cols = [c for c in combo_cols
                          if any(c in f for f in detail_fields)]
            parts.append(_table(detail_fields, base_cols + extra_cols))

        qt_columns = qt.get("columns") or []
        if qt_columns:
            normalised = []
            for col in qt_columns:
                row = dict(col)
                row["type"] = _qt_type_string(col)
                normalised.append(row)
            parts.append("")
            parts.append("*** Columns (Qt model)")
            parts.append("")
            parts.append(_table(normalised, ["enum_name", "field", "header", "type", "width"]))

    parts.append("")
    parts.append("** Custom repository methods")

    return "\n".join(parts)


def _description_for_frontmatter(de, slug):
    return (
        f"Codegen entity model for the {de['component']} ={slug}= table. "
        "Drives the C++ domain class, mapper, repository, service, generator, "
        "Qt UI, and SQL schema generation from a single literate source."
    )


def render(model, slug, today, new_id):
    de = model["domain_entity"]
    description_kw = _description_for_frontmatter(de, slug)
    sections = [
        _frontmatter(de, slug, today, new_id, description_kw),
        "",
        _prose(de.get("description") or ""),
        "",
        _flags_section(de),
        "",
        _primary_key_section(de),
        "",
        _natural_keys_section(de),
        "",
        _columns_section(de),
        "",
        _sql_section(de),
        "",
        _cpp_section(de),
        "",
    ]
    out = "\n".join(sections)
    # Normalise multiple blank lines to single, trailing newline guaranteed.
    while "\n\n\n" in out:
        out = out.replace("\n\n\n", "\n\n")
    return out.rstrip() + "\n"


def slug_from_filename(json_path):
    name = json_path.name
    suffix = "_domain_entity.json"
    if not name.endswith(suffix):
        raise ValueError(f"Not a domain_entity JSON: {json_path}")
    return name[: -len(suffix)]


def convert_file(json_path, output_dir, component, today):
    slug = slug_from_filename(json_path)
    with json_path.open(encoding="utf-8") as f:
        model = json.load(f)
    de = model.get("domain_entity")
    if not de:
        raise ValueError(f"Missing 'domain_entity' root: {json_path}")
    actual_component = de.get("component")
    if actual_component != component:
        raise ValueError(
            f"Component mismatch in {json_path.name}: "
            f"file declares {actual_component!r}, expected {component!r}"
        )
    new_id = str(uuid.uuid4()).upper()
    text = render(model, slug, today, new_id)
    out_path = output_dir / f"ores.{component}.{slug}.org"
    out_path.write_text(text, encoding="utf-8")
    return out_path


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--input-dir", required=True,
                   help="Directory holding *_domain_entity.json files.")
    p.add_argument("--output-dir", required=True,
                   help="Directory to write ores.<component>.<entity>.org files. "
                        "Created if missing.")
    p.add_argument("--component", required=True,
                   help="Short component name (refdata, trading, ...). "
                        "Every input file must agree on this.")
    p.add_argument("--only", default="",
                   help="Optional comma-separated list of entity slugs to convert "
                        "(without the _domain_entity.json suffix). Defaults to all.")
    p.add_argument("--dry-run", action="store_true",
                   help="Print planned output paths without writing.")
    args = p.parse_args(argv)

    input_dir = Path(args.input_dir)
    output_dir = Path(args.output_dir)
    if not input_dir.is_dir():
        print(f"error: input dir not found: {input_dir}", file=sys.stderr)
        return 1

    try:
        json_files = sorted(input_dir.glob("*_domain_entity.json"))
        if args.only:
            wanted = {s.strip() for s in args.only.split(",") if s.strip()}
            json_files = [j for j in json_files if slug_from_filename(j) in wanted]
        if not json_files:
            print(f"error: no matching *_domain_entity.json files in {input_dir}",
                  file=sys.stderr)
            return 1

        today = date.today().isoformat()

        if args.dry_run:
            for j in json_files:
                slug = slug_from_filename(j)
                print(output_dir / f"ores.{args.component}.{slug}.org")
            return 0

        output_dir.mkdir(parents=True, exist_ok=True)
        written = []
        for j in json_files:
            out_path = convert_file(j, output_dir, args.component, today)
            written.append(out_path)
            print(out_path)
        print(f"Converted {len(written)} files.", file=sys.stderr)
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
