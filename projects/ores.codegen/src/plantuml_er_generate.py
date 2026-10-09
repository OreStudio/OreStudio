#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""
PlantUML ER Diagram Generator

Renders PlantUML ER diagram from JSON model using Mustache template.

Modes:
  (default)   Write the rendered diagram to --output.
  --check     Exit non-zero if --output differs from a fresh render; write
              nothing (CI gate).
"""

import argparse
import difflib
import json
import re
import sys
import tempfile
from pathlib import Path

import pystache


def load_model(model_path: Path) -> dict:
    """Load JSON model from file."""
    with open(model_path, 'r', encoding='utf-8') as f:
        return json.load(f)


def load_template(template_path: Path) -> str:
    """Load Mustache template from file."""
    with open(template_path, 'r', encoding='utf-8') as f:
        return f.read()


def render_diagram(model: dict, template: str) -> str:
    """Render the PlantUML diagram using Mustache."""
    renderer = pystache.Renderer(escape=lambda x: x)  # Don't escape HTML entities
    return renderer.render(template, model)


def write_diagram(diagram: str, output_path: Path) -> None:
    """Write the rendered diagram to output_path."""
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(diagram)


def count_differing_lines(current: str, desired: str) -> int:
    """How many lines a regeneration would add, drop or change.

    autojunk is off: the diagram repeats lines such as braces and column
    text often enough that difflib would treat them as noise and report a
    count much larger than the diff really is.
    """
    matcher = difflib.SequenceMatcher(
        None, current.splitlines(), desired.splitlines(), autojunk=False)
    total = 0
    for tag, i1, i2, j1, j2 in matcher.get_opcodes():
        if tag != 'equal':
            total += max(i2 - i1, j2 - j1)
    return total


_GENERATED_AT_RE = re.compile(r"^' Generated: .*$", re.MULTILINE)


def without_generated_at(text: str) -> str:
    """The diagram with its generation stamp blanked out.

    The stamp records when the render ran, so a fresh render never reproduces
    it. Every other line must match, or the diagram is stale.
    """
    return _GENERATED_AT_RE.sub("' Generated: <timestamp>", text)


def group_output_path(output_path: Path, package_name: str) -> Path:
    """The file one group's diagram is written to."""
    return output_path.with_name(
        f"{output_path.stem}.{package_name}{output_path.suffix}")


def _package_by_table(model: dict) -> dict:
    """Which group each table belongs to."""
    return {table['name']: package['name']
            for package in model.get('packages', [])
            for table in package.get('tables', [])}


def render_group(model: dict, template: str, package: dict,
                 index_name: str, source_file: str) -> str:
    """One group's diagram: its tables, and the keys it holds inside itself.

    A key that leaves the group is not drawn -- its other end lives in
    another file -- so the group's note names those tables and the group
    each belongs to, which is where a reader goes next.
    """
    table_names = {table['name'] for table in package.get('tables', [])}
    relationships = model.get('relationships', [])
    inside = [rel for rel in relationships
              if rel['from_table'] in table_names
              and rel['to_table'] in table_names]

    home = _package_by_table(model)
    crossing = {}
    for rel in relationships:
        for near, far in ((rel['from_table'], rel['to_table']),
                          (rel['to_table'], rel['from_table'])):
            if near in table_names and far not in table_names:
                crossing[far] = home.get(far, 'unknown')

    context = {
        'generated_at': model.get('generated_at'),
        'diagram_title': package['name'],
        'group_table_count': len(package.get('tables', [])),
        'index_file': index_name,
        'source_file': source_file,
        'packages': [package],
        'relationships': inside,
        'external_note': '\n'.join(
            f'- {table} ({group})' for table, group in sorted(crossing.items())),
    }
    return render_diagram(context, template)


def render_index(model: dict, template: str, output_path: Path) -> str:
    """The page that names every group, the file it is drawn in, and its tables."""
    packages = []
    for package in model.get('packages', []):
        entry = dict(package)
        entry['table_count'] = len(package.get('tables', []))
        entry['file'] = group_output_path(output_path, package['name']).name
        packages.append(entry)

    context = {
        'generated_at': model.get('generated_at'),
        'source_file': output_path.name,
        'total_tables': sum(len(p.get('tables', []))
                            for p in model.get('packages', [])),
        'package_count': len(packages),
        'packages': packages,
    }
    return render_diagram(context, template)


def check_diagrams(renders: list) -> int:
    """Compare every fresh render with its committed file; write nothing.

    Each render passes through the same writer a real run uses, into a
    temporary directory, so the comparison sees the real bytes -- encoding and
    line endings included -- and the committed diagrams are never touched. The
    generation stamp is the one line excluded, because it records when the
    render ran rather than what it drew. Every stale or missing file is
    reported, not only the first, so one run names all the work.
    """
    stale = []
    for output_path, diagram in renders:
        if not output_path.exists():
            stale.append((output_path, None))
            continue

        with tempfile.TemporaryDirectory() as tmp_dir:
            rendered_path = Path(tmp_dir) / output_path.name
            write_diagram(diagram, rendered_path)
            rendered = rendered_path.read_text(encoding='utf-8')

        committed = output_path.read_text(encoding='utf-8')
        if without_generated_at(rendered) != without_generated_at(committed):
            stale.append((output_path, count_differing_lines(committed, rendered)))

    if not stale:
        print(f"{len(renders)} ER diagram(s) are up to date", file=sys.stderr)
        return 0

    print("stale ER diagram:", file=sys.stderr)
    for output_path, differing in stale:
        if differing is None:
            print(f"  {output_path} (output file does not exist)", file=sys.stderr)
        else:
            plural = "line differs" if differing == 1 else "lines differ"
            print(f"  {output_path} ({differing} {plural})", file=sys.stderr)
    print("\nrun: projects/ores.codegen/plantuml_er_generate.sh", file=sys.stderr)
    return 1


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(
        description='Generate PlantUML ER diagram from JSON model'
    )
    parser.add_argument('--model', '-m', required=True,
                        help='Input JSON model file')
    parser.add_argument('--template', '-t', required=True,
                        help='Mustache template file')
    parser.add_argument('--index-template',
                        help='Mustache template for the index page. With it, '
                             'one diagram is written per group beside --output, '
                             'and --output holds the index.')
    parser.add_argument('--output', '-o', required=True,
                        help='Output PlantUML file')
    parser.add_argument('--check', action='store_true',
                        help='Exit non-zero if any output is stale; write nothing.')

    args = parser.parse_args(argv)

    model_path = Path(args.model)
    template_path = Path(args.template)
    index_template_path = Path(args.index_template) if args.index_template else None
    output_path = Path(args.output)

    if not model_path.exists():
        print(f"Error: Model file not found: {model_path}", file=sys.stderr)
        return 1

    if not template_path.exists():
        print(f"Error: Template file not found: {template_path}", file=sys.stderr)
        return 1

    if index_template_path and not index_template_path.exists():
        print(f"Error: Template file not found: {index_template_path}",
              file=sys.stderr)
        return 1

    # Load inputs
    print(f"Loading model: {model_path}", file=sys.stderr)
    model = load_model(model_path)

    print(f"Loading template: {template_path}", file=sys.stderr)
    template = load_template(template_path)

    # Render the diagrams
    print("Rendering diagram...", file=sys.stderr)
    if index_template_path:
        index_template = load_template(index_template_path)
        renders = []
        for package in model.get('packages', []):
            path = group_output_path(output_path, package['name'])
            renders.append(
                (path, render_group(model, template, package,
                                    output_path.name, path.name)))
        renders.append(
            (output_path, render_index(model, index_template, output_path)))
    else:
        renders = [(output_path, render_diagram(model, template))]

    if args.check:
        return check_diagrams(renders)

    # Write output
    for path, diagram in renders:
        write_diagram(diagram, path)

    print(f"Diagrams written: {len(renders)}", file=sys.stderr)
    print(f"Index: {output_path}", file=sys.stderr)

    # Print stats
    print(f"Packages: {len(model.get('packages', []))}", file=sys.stderr)
    total_tables = sum(len(p.get('tables', [])) for p in model.get('packages', []))
    print(f"Tables: {total_tables}", file=sys.stderr)
    return 0


if __name__ == '__main__':
    sys.exit(main())
