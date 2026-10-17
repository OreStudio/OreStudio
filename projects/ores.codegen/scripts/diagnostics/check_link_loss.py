#!/usr/bin/env python3
"""Report every link token a part CMakeLists carried at the branch base and the
generated file no longer links.

Two classes of defect are invisible to a compile of a single target:
  - a library dropped outright (its include directory vanishes),
  - a library kept but with its visibility narrowed.
Both are compared here, per target_link_libraries block, against
`git show <base>:<path>`.
"""

import re
import subprocess
import sys

BASE = "ed7e0a3a87"
SECTIONS = {"PUBLIC", "PRIVATE", "INTERFACE", "LINK_PUBLIC", "LINK_PRIVATE"}
NOISE = {"debug", "optimized", "general"}
BLOCK = re.compile(r"target_link_libraries\s*\(", re.I)


def blocks(text):
    """Every target_link_libraries call, as its argument text."""
    out = []
    for m in BLOCK.finditer(text):
        depth, i = 1, m.end()
        while i < len(text) and depth:
            if text[i] == "(":
                depth += 1
            elif text[i] == ")":
                depth -= 1
            i += 1
        out.append(text[m.end():i - 1])
    return out


def parse(block):
    """(section, token) pairs, section carried forward."""
    pairs, section = [], "PUBLIC"
    for tok in block.split():
        if tok in SECTIONS:
            section = tok
        elif tok not in NOISE:
            pairs.append((section, tok))
    return pairs


def is_public(section):
    return section in ("PUBLIC", "INTERFACE", "LINK_PUBLIC")


def main():
    paths = subprocess.run(
        ["git", "ls-tree", "-r", "--name-only", BASE],
        capture_output=True, text=True, check=True).stdout.split()
    paths = [p for p in paths
             if re.search(r"projects/[^/]+/[^/]+/.*CMakeLists\.txt$", p)
             and "/src/" in p or re.search(r"projects/.*/tests/CMakeLists\.txt$", p)]
    losses = narrowings = 0
    for path in paths:
        hand = subprocess.run(["git", "show", f"{BASE}:{path}"],
                              capture_output=True, text=True).stdout
        try:
            generated = open(path).read()
        except FileNotFoundError:
            continue
        hb, gb = blocks(hand), blocks(generated)
        if not hb:
            continue
        hand_pairs = [p for b in hb for p in parse(b)]
        gen_pairs = [p for b in gb for p in parse(b)]
        gen_tokens = {t for _, t in gen_pairs}
        gen_public = {t for s, t in gen_pairs if is_public(s)}
        for section, token in hand_pairs:
            if token.startswith(("${", "$<", "${lib_target_name}")):
                continue
            if token not in gen_tokens:
                print(f"LOST      {path}: {token} ({section})")
                losses += 1
            elif is_public(section) and token not in gen_public:
                print(f"NARROWED  {path}: {token} {section} -> PRIVATE")
                narrowings += 1
    print(f"\nlosses: {losses}  narrowings: {narrowings}", file=sys.stderr)


main()
