#!/usr/bin/env python3
"""Reapply ORES_ORE_CORE_EXPORT hand-patch annotations lost on xsdcpp regen.

Reads the previously-committed domain.hpp (old) and the freshly regenerated
domain.hpp (new). For every exported declaration in old, finds the matching
declaration (ignoring whitespace/line-wrapping differences) in new and
prefixes it with the export macro too.
"""
import re
import sys

MACRO = "ORES_ORE_CORE_EXPORT"

old_path, new_path = sys.argv[1], sys.argv[2]

with open(old_path) as f:
    old_text = f.read()
with open(new_path) as f:
    new_text = f.read()

# Find each "ORES_ORE_CORE_EXPORT ... ;" statement (balancing parens for
# multi-line declarations).
snippets = []
i = 0
while True:
    idx = old_text.find(MACRO, i)
    if idx == -1:
        break
    j = idx + len(MACRO)
    # Walk forward to the terminating ';' (declarations here have no nested
    # braces, only parens).
    depth = 0
    k = j
    while k < len(old_text):
        c = old_text[k]
        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
        elif c == ';' and depth == 0:
            k += 1
            break
        k += 1
    snippet = old_text[idx:k]
    snippets.append(snippet)
    i = k

print(f"Found {len(snippets)} exported declarations in old file", file=sys.stderr)

matched = 0
unmatched = []
for snippet in snippets:
    body = snippet[len(MACRO):].strip()
    # Build a whitespace-flexible regex from the body.
    tokens = body.split()
    pattern = r'\s+'.join(re.escape(t) for t in tokens)
    regex = re.compile(pattern)
    m = regex.search(new_text)
    if not m:
        unmatched.append(body)
        continue
    start, end = m.span()
    # Skip if already prefixed (avoid double-annotating).
    prefix_window = new_text[max(0, start-40):start]
    if MACRO in prefix_window and prefix_window.rstrip().endswith(MACRO):
        matched += 1
        continue
    new_text = new_text[:start] + MACRO + " " + new_text[start:]
    matched += 1

with open(new_path, 'w') as f:
    f.write(new_text)

print(f"Matched and annotated {matched}/{len(snippets)}", file=sys.stderr)
if unmatched:
    print(f"\n{len(unmatched)} unmatched (schema removed or changed):", file=sys.stderr)
    for u in unmatched:
        print(f"  - {u[:120]}", file=sys.stderr)
