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


def check_diagram(diagram: str, output_path: Path) -> int:
    """Compare a fresh render with the committed output; write nothing.

    The render passes through the same writer a real run uses, into a
    temporary directory, so the comparison sees the real bytes -- encoding and
    line endings included -- and the committed diagram is never touched. The
    generation stamp is the one line excluded, because it records when the
    render ran rather than what it drew.
    """
    if not output_path.exists():
        print("stale ER diagram:", file=sys.stderr)
        print(f"  {output_path} (output file does not exist)", file=sys.stderr)
        print("\nrun: projects/ores.codegen/plantuml_er_generate.sh",
              file=sys.stderr)
        return 1

    with tempfile.TemporaryDirectory() as tmp_dir:
        rendered_path = Path(tmp_dir) / output_path.name
        write_diagram(diagram, rendered_path)
        rendered = rendered_path.read_text(encoding='utf-8')

    committed = output_path.read_text(encoding='utf-8')
    if without_generated_at(rendered) == without_generated_at(committed):
        print(f"{output_path} is up to date", file=sys.stderr)
        return 0

    differing = count_differing_lines(committed, rendered)
    plural = "line differs" if differing == 1 else "lines differ"
    print("stale ER diagram:", file=sys.stderr)
    print(f"  {output_path} ({differing} {plural})", file=sys.stderr)
    print("\nrun: projects/ores.codegen/plantuml_er_generate.sh",
          file=sys.stderr)
    return 1


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(
        description='Generate PlantUML ER diagram from JSON model'
    )
    parser.add_argument('--model', '-m', required=True,
                        help='Input JSON model file')
    parser.add_argument('--template', '-t', required=True,
                        help='Mustache template file')
    parser.add_argument('--output', '-o', required=True,
                        help='Output PlantUML file')
    parser.add_argument('--check', action='store_true',
                        help='Exit non-zero if the output is stale; write nothing.')

    args = parser.parse_args(argv)

    model_path = Path(args.model)
    template_path = Path(args.template)
    output_path = Path(args.output)

    if not model_path.exists():
        print(f"Error: Model file not found: {model_path}", file=sys.stderr)
        return 1

    if not template_path.exists():
        print(f"Error: Template file not found: {template_path}", file=sys.stderr)
        return 1

    # Load inputs
    print(f"Loading model: {model_path}", file=sys.stderr)
    model = load_model(model_path)

    print(f"Loading template: {template_path}", file=sys.stderr)
    template = load_template(template_path)

    # Render diagram
    print("Rendering diagram...", file=sys.stderr)
    diagram = render_diagram(model, template)

    if args.check:
        return check_diagram(diagram, output_path)

    # Write output
    write_diagram(diagram, output_path)

    print(f"Diagram written to: {output_path}", file=sys.stderr)

    # Print stats
    print(f"Packages: {len(model.get('packages', []))}", file=sys.stderr)
    total_tables = sum(len(p.get('tables', [])) for p in model.get('packages', []))
    print(f"Tables: {total_tables}", file=sys.stderr)
    return 0


if __name__ == '__main__':
    sys.exit(main())
