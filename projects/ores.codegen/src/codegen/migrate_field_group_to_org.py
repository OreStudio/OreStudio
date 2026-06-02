"""
Convert ``*_field_group.json`` codegen models to literate
``ores.<component>.<name>_field_group.org`` files under each
owning component's ``modeling/`` directory.

Field-group models are smaller than entity models — no Qt, no SQL,
no natural keys or columns. They describe a flat C++ struct that
composes into a larger entity via ``rfl::Flatten<T>``. The org file
mirrors the JSON one-for-one: frontmatter for the entity-level
scalars, an Includes named babel block for cpp.includes, and a
* Fields section with one ``** <name>`` sub-heading per field.

Usage:

    python3 projects/ores.codegen/src/codegen/migrate_field_group_to_org.py \
        --input-dir projects/ores.codegen/models/trading \
        --output-dir projects/ores.trading/modeling \
        --component trading
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


def _field_section(field):
    drawer = _drawer([
        ("cpp_type", field.get("cpp_type")),
        ("default_value", field.get("default_value")),
    ])
    parts = [f"** {field['name']}", drawer]
    description = (field.get("description") or "").rstrip()
    if description:
        parts.append("")
        parts.append(description)
    return "\n".join(parts)


def render(model, slug, today, new_id):
    fg = model["field_group"]
    component = fg["component"]
    description = (fg.get("description") or "").rstrip()

    frontmatter = [
        ":PROPERTIES:",
        f":ID: {new_id}",
        ":END:",
        f"#+title: ores.{component}.{slug}",
        f"#+description: Codegen field-group model for {fg.get('cpp', {}).get('namespace', '?')}::{fg.get('entity_singular', slug)}.",
        "#+type: ores.codegen.field_group",
        f"#+component: {component}",
    ]
    if "component_include" in fg:
        frontmatter.append(f"#+component_include: {fg['component_include']}")
    if "product" in fg:
        frontmatter.append(f"#+product: {fg['product']}")
    if "entity_singular" in fg:
        frontmatter.append(f"#+entity_singular: {fg['entity_singular']}")
    if (fg.get("cpp") or {}).get("namespace"):
        frontmatter.append(f"#+namespace: {fg['cpp']['namespace']}")
    if "brief" in fg:
        frontmatter.append(f"#+brief: {fg['brief']}")
    frontmatter.append(f"#+filetags: :model:field_group:{component}:")
    frontmatter.append(f"#+created: {today}")
    frontmatter.append(f"#+updated: {today}")

    sections = ["\n".join(frontmatter), "", description, ""]

    includes = (fg.get("cpp") or {}).get("includes") or []
    if includes:
        sections.append("* Includes")
        sections.append("")
        sections.append(_includes_block(includes))
        sections.append("")

    sections.append("* Fields")
    for field in fg.get("fields", []):
        sections.append("")
        sections.append(_field_section(field))

    out = "\n".join(sections)
    while "\n\n\n" in out:
        out = out.replace("\n\n\n", "\n\n")
    return out.rstrip() + "\n"


def slug_from_filename(path):
    name = path.name
    suffix = "_field_group.json"
    if not name.endswith(suffix):
        raise ValueError(f"Not a field_group JSON: {path}")
    return name[: -len(suffix)]


def convert_file(json_path, output_dir, component, today):
    slug = slug_from_filename(json_path)
    with json_path.open(encoding="utf-8") as f:
        model = json.load(f)
    fg = model.get("field_group")
    if not fg:
        raise ValueError(f"Missing 'field_group' root: {json_path}")
    actual = fg.get("component")
    if actual != component:
        raise ValueError(
            f"Component mismatch in {json_path.name}: "
            f"file declares {actual!r}, expected {component!r}"
        )
    new_id = str(uuid.uuid4()).upper()
    text = render(model, slug, today, new_id)
    out_path = output_dir / f"ores.{component}.{slug}_field_group.org"
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
        json_files = sorted(input_dir.glob("*_field_group.json"))
        if not json_files:
            print(f"error: no *_field_group.json files in {input_dir}",
                  file=sys.stderr)
            return 1
        output_dir.mkdir(parents=True, exist_ok=True)
        today = date.today().isoformat()
        for j in json_files:
            out = convert_file(j, output_dir, args.component, today)
            print(out)
        print(f"Converted {len(json_files)} files.", file=sys.stderr)
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
