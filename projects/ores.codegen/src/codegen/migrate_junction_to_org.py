"""
Convert ``*_junction.json`` codegen models to literate
``ores.<component>.<name>_junction.org`` files under each owning
component's ``modeling/`` directory.

Junctions are many-to-many association tables. Codegen runs them only
through the ores.sql.schema address today (one ``*_create.sql`` per junction
under ``projects/ores.sql/create/<component>/``).

The org file mirrors the JSON one-for-one: frontmatter for the
entity-level scalars; ``* Left`` / ``* Right`` sub-sections with
property drawers + body + optional generator babel block for the
two FK columns; ``* Columns`` for any extra columns; ``* SQL``,
``* Repository``, ``* C++`` mirroring the entity meta-model.

Usage::

    python3 projects/ores.codegen/src/codegen/migrate_junction_to_org.py \\
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


def _includes_block(includes):
    if not includes:
        return ""
    body_lines = []
    for inc in includes:
        s = inc.strip()
        if s.startswith("#include"):
            body_lines.append(s)
        else:
            body_lines.append(f"#include {s}")
    return "#+begin_src cpp :name includes\n" + "\n".join(body_lines) + "\n#+end_src"


def _generator_block(node):
    expr = node.get("generator_expr")
    if not expr:
        return ""
    return f"#+begin_src cpp :name generator\n{expr}\n#+end_src"


def _description_detail(node):
    parts = []
    if node.get("description"):
        parts.append(node["description"].rstrip())
    if node.get("detail"):
        parts.append(node["detail"].rstrip())
    return "\n\n".join(parts)


def _fk_section(side_name, node):
    drawer_keys = [
        ("column", node.get("column")),
        ("column_short", node.get("column_short")),
        ("column_title", node.get("column_title")),
        ("type", node.get("type")),
        ("cpp_type", node.get("cpp_type")),
        ("index_comment", node.get("index_comment")),
    ]
    drawer = _drawer(drawer_keys)
    body = _description_detail(node)
    gen = _generator_block(node)
    parts = [f"* {side_name}", drawer]
    if body:
        parts.append("")
        parts.append(body)
    if gen:
        parts.append("")
        parts.append(gen)
    return "\n".join(parts)


def _column_section(col):
    drawer_keys = [
        ("type", col.get("type")),
        ("cpp_type", col.get("cpp_type")),
    ]
    if "nullable" in col:
        drawer_keys.append(("nullable", str(col["nullable"]).lower()))
    if "default" in col:
        drawer_keys.append(("default", col["default"]))
    if "default_value" in col:
        drawer_keys.append(("default_value", col["default_value"]))
    drawer = _drawer(drawer_keys)
    body = _description_detail(col)
    gen = _generator_block(col)
    parts = [f"** {col['name']}", drawer]
    if body:
        parts.append("")
        parts.append(body)
    if gen:
        parts.append("")
        parts.append(gen)
    return "\n".join(parts)


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
    return "\n".join([header, sep, *body])


def render(model, slug, today, new_id):
    j = model["junction"]
    component = j["component"]
    description = (j.get("description") or "").rstrip()

    frontmatter = [
        ":PROPERTIES:",
        f":ID: {new_id}",
        ":END:",
        f"#+title: ores.{component}.{slug}",
        f"#+description: Codegen junction model for {j.get('name', slug)}.",
        "#+type: ores.codegen.junction",
        f"#+component: {component}",
        f"#+filetags: :model:junction:{component}:",
    ]
    if "name" in j:
        frontmatter.append(f"#+name: {j['name']}")
    if "name_singular" in j:
        frontmatter.append(f"#+name_singular: {j['name_singular']}")
    if "name_title" in j:
        frontmatter.append(f"#+name_title: {j['name_title']}")
    if "name_singular_words" in j:
        frontmatter.append(f"#+name_singular_words: {j['name_singular_words']}")
    if "brief" in j:
        frontmatter.append(f"#+brief: {j['brief']}")
    if "product" in j:
        frontmatter.append(f"#+product: {j['product']}")
    if "schema" in j:
        frontmatter.append(f"#+schema: {j['schema']}")
    if "has_tenant_id" in j:
        frontmatter.append(f"#+has_tenant_id: {str(j['has_tenant_id']).lower()}")
    frontmatter.append(f"#+created: {today}")
    frontmatter.append(f"#+updated: {today}")

    sections = ["\n".join(frontmatter), "", description, ""]

    if "left" in j:
        sections.append(_fk_section("Left", j["left"]))
        sections.append("")
    if "right" in j:
        sections.append(_fk_section("Right", j["right"]))
        sections.append("")

    sections.append("* Columns")
    for col in j.get("columns", []) or []:
        sections.append("")
        sections.append(_column_section(col))
    sections.append("")

    sql = j.get("sql", {}) or {}
    sql_pairs = [("tablename", sql.get("tablename"))]
    for k, v in sql.items():
        if k == "tablename":
            continue
        if isinstance(v, (list, dict)):
            continue
        if isinstance(v, bool):
            v = "true" if v else "false"
        sql_pairs.append((k, v))
    sections.append(f"* SQL\n\n** Flags\n{_drawer(sql_pairs)}")
    sections.append("")

    repo = j.get("repository", {}) or {}
    repo_pairs = [
        ("name_singular_short", repo.get("name_singular_short")),
        ("name_short", repo.get("name_short")),
        ("name_singular_words", repo.get("name_singular_words")),
        ("name_words", repo.get("name_words")),
        ("order_column", repo.get("order_column")),
    ]
    sections.append(f"* Repository\n{_drawer(repo_pairs)}")
    sections.append("")

    cpp = j.get("cpp", {}) or {}
    sections.append("* C++")
    sections.append("")
    domain_includes = ((cpp.get("includes") or {}).get("domain") or [])
    if domain_includes:
        sections.append("** Domain includes")
        sections.append("")
        sections.append(_includes_block(domain_includes))
        sections.append("")
    entity_includes = ((cpp.get("includes") or {}).get("entity") or [])
    if entity_includes:
        sections.append("** Entity includes")
        sections.append("")
        sections.append(_includes_block(entity_includes))
        sections.append("")
    if "iterator_var" in cpp:
        sections.append(f"** Conventions\n{_drawer([('iterator_var', cpp['iterator_var'])])}")
        sections.append("")
    td = cpp.get("table_display") or []
    if td:
        sections.append("** Table display")
        sections.append("")
        sections.append(_table(td, ["column", "header"]))

    out = "\n".join(sections)
    while "\n\n\n" in out:
        out = out.replace("\n\n\n", "\n\n")
    return out.rstrip() + "\n"


def slug_from_filename(path):
    name = path.name
    suffix = "_junction.json"
    if not name.endswith(suffix):
        raise ValueError(f"Not a junction JSON: {path}")
    return name[: -len(suffix)]


def convert_file(json_path, output_dir, component, today):
    slug = slug_from_filename(json_path)
    with json_path.open(encoding="utf-8") as f:
        model = json.load(f)
    j = model.get("junction")
    if not j:
        raise ValueError(f"Missing 'junction' root: {json_path}")
    actual = j.get("component")
    if actual != component:
        raise ValueError(
            f"Component mismatch in {json_path.name}: "
            f"file declares {actual!r}, expected {component!r}"
        )
    new_id = str(uuid.uuid4()).upper()
    text = render(model, slug, today, new_id)
    out_path = output_dir / f"ores.{component}.{slug}_junction.org"
    out_path.write_text(text, encoding="utf-8")
    return out_path


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--input-dir", required=True)
    p.add_argument("--output-dir", required=True)
    p.add_argument("--component", required=True)
    args = p.parse_args(argv)

    input_dir = Path(args.input_dir)
    output_dir = Path(args.output_dir)
    if not input_dir.is_dir():
        print(f"error: input dir not found: {input_dir}", file=sys.stderr)
        return 1

    try:
        json_files = sorted(input_dir.glob("*_junction.json"))
        if not json_files:
            print(f"error: no *_junction.json files in {input_dir}",
                  file=sys.stderr)
            return 1
        output_dir.mkdir(parents=True, exist_ok=True)
        today = date.today().isoformat()
        for jp in json_files:
            out = convert_file(jp, output_dir, args.component, today)
            print(out)
        print(f"Converted {len(json_files)} files.", file=sys.stderr)
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
