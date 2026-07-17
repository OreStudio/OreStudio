#!/usr/bin/env python3
"""Render a UVL feature model to a PNG feature-diagram, via FlamaPy + Graphviz.

Experimental: not wired into compass.py. See
doc/analysis/gemini_uvl_analysis.org for the original pipeline sketch this
was adapted from, and the "Formalize the codegen entity knob system as a
proper MASD/MDE feature model" capture for why this exists.

Requires (compass's own venv already has these — see
projects/ores.compass/venv):
    pip install flamapy graphviz
    apt install graphviz   # the native rendering engine, not just bindings

Usage:
    python3 uvl2png.py <input.uvl> <output.png>
"""
import sys
import os
from graphviz import Digraph
from flamapy.metamodels.fm_metamodel.transformations import UVLReader


def render_feature_tree(uvl_path, output_png_path):
    print(f"[*] Parsing {uvl_path}")
    reader = UVLReader(uvl_path)
    feature_model = reader.transform()

    dot = Digraph(comment='Feature Model Tree', format='png')
    dot.attr('graph', rankdir='TB', splines='ortho', nodesep='0.6', ranksep='0.6', bgcolor='#fafafa')
    dot.attr('node', fontname='Helvetica', fontsize='11', shape='box',
             style='rounded,filled', fillcolor='#ffffff', color='#2b2b2b', penwidth='1.5')
    dot.attr('edge', color='#4a4a4a', penwidth='1.2')

    def traverse(feature):
        dot.node(feature.name, feature.name)

        for relation in feature.get_relations():
            is_mandatory = relation.is_mandatory()
            is_alternative = relation.is_alternative()
            is_or = relation.is_or()

            for child in relation.children:
                arrow_head = 'normal'
                edge_style = 'solid'
                edge_label = ''

                if not is_mandatory and not is_alternative and not is_or:
                    arrow_head = 'empty'  # optional
                elif is_alternative:
                    edge_label = 'XOR'
                    edge_style = 'dashed'
                elif is_or:
                    edge_label = 'OR'

                dot.edge(feature.name, child.name, style=edge_style, arrowhead=arrow_head, label=edge_label)
                traverse(child)

    root_feature = feature_model.root
    if root_feature:
        traverse(root_feature)
    else:
        print("[!] Model has no root feature", file=sys.stderr)
        sys.exit(1)

    output_base = os.path.splitext(output_png_path)[0]
    dot.render(output_base, cleanup=True)
    print(f"[+] Wrote {output_base}.png")


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python3 uvl2png.py <input.uvl> <output.png>", file=sys.stderr)
        sys.exit(1)
    render_feature_tree(sys.argv[1], sys.argv[2])
