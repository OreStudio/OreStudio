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
"""

import argparse
import json
import sys
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


def main():
    parser = argparse.ArgumentParser(
        description='Generate PlantUML ER diagram from JSON model'
    )
    parser.add_argument('--model', '-m', required=True,
                        help='Input JSON model file')
    parser.add_argument('--template', '-t', required=True,
                        help='Mustache template file')
    parser.add_argument('--output', '-o', required=True,
                        help='Output PlantUML file')

    args = parser.parse_args()

    model_path = Path(args.model)
    template_path = Path(args.template)
    output_path = Path(args.output)

    if not model_path.exists():
        print(f"Error: Model file not found: {model_path}", file=sys.stderr)
        sys.exit(1)

    if not template_path.exists():
        print(f"Error: Template file not found: {template_path}", file=sys.stderr)
        sys.exit(1)

    # Load inputs
    print(f"Loading model: {model_path}", file=sys.stderr)
    model = load_model(model_path)

    print(f"Loading template: {template_path}", file=sys.stderr)
    template = load_template(template_path)

    # Render diagram
    print("Rendering diagram...", file=sys.stderr)
    diagram = render_diagram(model, template)

    # Write output
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(diagram)

    print(f"Diagram written to: {output_path}", file=sys.stderr)

    # Print stats
    print(f"Packages: {len(model.get('packages', []))}", file=sys.stderr)
    total_tables = sum(len(p.get('tables', [])) for p in model.get('packages', []))
    print(f"Tables: {total_tables}", file=sys.stderr)


if __name__ == '__main__':
    main()
