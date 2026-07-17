"""compass uvl — render a UVL feature model to a proper feature-diagram SVG.

Same UVL parsing stack as https://ide.flamapy.org/ (flamapy's UVLReader —
that IDE runs the identical Python package in-browser via Pyodide) but a
feature-diagram-aware renderer instead of a generic graph layout, matching
the standard cardinality-based FODA notation:

    - mandatory child: solid line, filled black circle at the child end.
    - optional child:  solid line, hollow (white) circle at the child end.
    - alternative (xor) group: a small hollow arc spanning the group's
      edges near the parent.
    - or group: a small filled arc spanning the group's edges near the
      parent.

See doc/analysis/gemini_uvl_analysis.org for the earlier Graphviz attempt
this replaced, and the "Formalize the codegen entity knob system as a
proper MASD/MDE feature model" capture for why this exists.

Requires flamapy, only needed for this subcommand:
    projects/ores.compass/venv/bin/pip install flamapy
"""
import argparse
import math
import sys
from pathlib import Path

BOX_W, BOX_H = 150, 40
COL_GAP, ROW_GAP = 30, 90
MARGIN = 40
CIRCLE_R = 6


class LayoutNode:
    def __init__(self, feature, relation_kind):
        self.feature = feature
        self.relation_kind = relation_kind  # 'mandatory' | 'optional' | 'alternative' | 'or'
        self.children = []
        self.x = 0.0
        self.y = 0.0


def _build_tree(feature):
    node = LayoutNode(feature, relation_kind=None)
    for relation in feature.get_relations():
        if relation.is_mandatory():
            kind = 'mandatory'
        elif relation.is_alternative():
            kind = 'alternative'
        elif relation.is_or():
            kind = 'or'
        else:
            kind = 'optional'
        for child in relation.children:
            child_node = _build_tree(child)
            child_node.relation_kind = kind
            node.children.append(child_node)
    return node


def _assign_positions(node, next_x, depth):
    """Post-order layout: leaves get sequential x slots; a parent's x is
    the mean of its children's."""
    node.y = depth * ROW_GAP
    if not node.children:
        node.x = next_x[0] * (BOX_W + COL_GAP)
        next_x[0] += 1
        return
    for child in node.children:
        _assign_positions(child, next_x, depth + 1)
    node.x = sum(c.x for c in node.children) / len(node.children)


def _collect_bounds(node, bounds):
    bounds[0] = min(bounds[0], node.x)
    bounds[1] = max(bounds[1], node.x)
    bounds[2] = max(bounds[2], node.y)
    for child in node.children:
        _collect_bounds(child, bounds)


def _svg_escape(text):
    return text.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')


def _render_boxes(node, out):
    """Pass 1: every node's box + label. Drawn before edges so pass 2's
    connector circles land on top of a box's border instead of being
    half-covered by it."""
    cx = node.x + MARGIN
    cy = node.y + MARGIN
    fill = '#f0f0f5' if getattr(node.feature, 'is_abstract', False) else '#ffffff'
    out.append(
        f'<rect x="{cx - BOX_W/2:.1f}" y="{cy:.1f}" width="{BOX_W}" height="{BOX_H}" '
        f'rx="8" ry="8" fill="{fill}" stroke="#2b2b2b" stroke-width="1.5"/>'
    )
    out.append(
        f'<text x="{cx:.1f}" y="{cy + BOX_H/2 + 4:.1f}" text-anchor="middle" '
        f'font-family="Helvetica" font-size="12">{_svg_escape(node.feature.name)}</text>'
    )
    for child in node.children:
        _render_boxes(child, out)


def _render_edges(node, out):
    """Pass 2: every edge, connector circle, and group arc — drawn after
    all boxes so the circles sit fully visible on top of them."""
    cx = node.x + MARGIN
    cy = node.y + MARGIN

    groups = {}
    for child in node.children:
        groups.setdefault(child.relation_kind, []).append(child)

    for kind, kids in groups.items():
        edge_points = []
        for child in kids:
            ccx = child.x + MARGIN
            ccy = child.y + MARGIN
            parent_bottom = (cx, cy + BOX_H)
            child_top = (ccx, ccy)
            out.append(
                f'<line x1="{parent_bottom[0]:.1f}" y1="{parent_bottom[1]:.1f}" '
                f'x2="{child_top[0]:.1f}" y2="{child_top[1]:.1f}" '
                f'stroke="#4a4a4a" stroke-width="1.4"/>'
            )
            circle_fill = '#2b2b2b' if kind == 'mandatory' else '#ffffff'
            out.append(
                f'<circle cx="{child_top[0]:.1f}" cy="{child_top[1]:.1f}" r="{CIRCLE_R}" '
                f'fill="{circle_fill}" stroke="#2b2b2b" stroke-width="1.4"/>'
            )
            edge_points.append(child_top)

        if kind in ('alternative', 'or') and len(edge_points) >= 2:
            radius = 22
            angles = [
                math.atan2(px - cx, py - (cy + BOX_H))
                for px, py in edge_points
            ]
            a0, a1 = min(angles), max(angles)
            x0 = cx + radius * math.sin(a0)
            y0 = (cy + BOX_H) + radius * math.cos(a0)
            x1 = cx + radius * math.sin(a1)
            y1 = (cy + BOX_H) + radius * math.cos(a1)
            arc_fill = '#2b2b2b' if kind == 'or' else 'none'
            out.append(
                f'<path d="M {x0:.1f} {y0:.1f} A {radius} {radius} 0 0 1 {x1:.1f} {y1:.1f}" '
                f'fill="{arc_fill}" stroke="#2b2b2b" stroke-width="1.4"/>'
            )

    for child in node.children:
        _render_edges(child, out)


def compile_uvl(uvl_path, output_svg_path):
    """Parse a UVL file and write a feature-diagram SVG. Returns 0 on
    success, non-zero on error (missing flamapy, no root feature, ...)."""
    try:
        from flamapy.metamodels.fm_metamodel.transformations import UVLReader
    except ImportError:
        print("❌ flamapy not installed. Run:\n"
              "   projects/ores.compass/venv/bin/pip install flamapy",
              file=sys.stderr)
        return 1

    print(f"[*] Parsing {uvl_path}")
    reader = UVLReader(str(uvl_path))
    feature_model = reader.transform()
    root_feature = feature_model.root
    if not root_feature:
        print("[!] Model has no root feature", file=sys.stderr)
        return 1

    tree = _build_tree(root_feature)
    _assign_positions(tree, next_x=[0], depth=0)

    bounds = [tree.x, tree.x, tree.y]
    _collect_bounds(tree, bounds)
    width = bounds[1] - bounds[0] + BOX_W + 2 * MARGIN
    height = bounds[2] + BOX_H + 2 * MARGIN

    def shift(node, dx):
        node.x -= dx
        for c in node.children:
            shift(c, dx)
    shift(tree, bounds[0] - BOX_W / 2)

    body = []
    _render_boxes(tree, body)
    _render_edges(tree, body)

    svg = (
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width:.0f}" height="{height:.0f}" '
        f'viewBox="0 0 {width:.0f} {height:.0f}">\n'
        + '\n'.join(body) +
        '\n</svg>\n'
    )
    Path(output_svg_path).write_text(svg)
    print(f"[+] Wrote {output_svg_path}")
    return 0


def run(argv, project_root):
    ap = argparse.ArgumentParser(
        prog="compass uvl",
        description="Compile a UVL feature model to a feature-diagram SVG.")
    sub = ap.add_subparsers(dest="subcmd", metavar="SUBCMD")

    sp = sub.add_parser("compile", help="Render a .uvl file to a feature-diagram .svg")
    sp.add_argument("input", help="Path to the .uvl source file")
    sp.add_argument("output", nargs="?", default=None,
                     help="Output .svg path (default: input path with .svg extension)")

    args = ap.parse_args(argv)
    if args.subcmd is None:
        ap.print_help()
        return 0
    if args.subcmd != "compile":
        ap.print_help()
        return 1

    input_path = Path(args.input)
    if not input_path.is_absolute():
        input_path = Path.cwd() / input_path
    output_path = Path(args.output) if args.output else input_path.with_suffix(".svg")

    return compile_uvl(input_path, output_path)
