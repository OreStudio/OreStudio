#!/usr/bin/env python3
"""
Check that every symbol a model's pasted code block names exists in the tree.

A model can paste hand-written C++ into generated output through an
``:implements`` block. The block reaches the compiler only when its component is
regenerated, and the drift gate regenerates a component only when somebody runs
it, so a block can name a symbol that no longer exists and nothing notices. The
measured case is the ``ores.service`` clean-up: it moved three workflow header
names out of ``ores.service`` into ``ores.workflow.api``, and ``ores.refdata``
keeps a workflow override as a pasted block. The generated handler was hand-edited
to the new names and the model was left naming the deleted ones, so the next
refdata regeneration would have reverted the edit and broken the build.
``check_component_drift.py --all`` was green and CI was green on every head.

The sibling gate, ``check_component_drift.py --sweep``, catches that case because
the model and the generated file disagree. It cannot catch the narrower one where
both agree and are both stale, because the render reproduces the committed bytes.
This check reads the model itself, so it catches both, and it needs no
regeneration: a rename fails at review.

== How a name is resolved

The check reads every header under a ``projects/`` ``include/`` directory and
builds the set of ``(scope, identifier)`` pairs it declares, where the scope is
the namespace, class, enum or struct path the identifier sits in. A pasted
``ores::a::b::name`` resolves when ``(ores, a, b)`` declares ``name``. An export
macro between the keyword and the name is skipped, because the codebase writes
``class ORES_PLATFORM_EXPORT datetime``.

The scope is taken from the braces the identifier sits inside, so a name used
inside a namespace records that namespace even when it is not declared there.
That errs towards resolving, which keeps the check quiet on the tree it runs
against; it is a lint for a rename, not a compiler.

This check reads the tree and writes nothing.

Usage:
  check_paste_block_symbols.py
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROJECTS_DIR = REPO_ROOT / "projects"
SCRIPTS_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPTS_DIR))

from check_test_case_reachability import (  # noqa: E402
    _blank_comments_and_literals,
)

# An :implements block: a cpp source block whose header names the kind it fills.
_PASTE_BLOCK_RE = re.compile(
    r"^#\+begin_src\s+cpp[^\n]*:implements[^\n]*\n(?P<body>.*?)^#\+end_src",
    re.MULTILINE | re.DOTALL,
)

# The block's :name argument, used only to report where a name came from.
_BLOCK_NAME_RE = re.compile(r":name\s+(\S+)")

# A qualified name rooted at the project's own namespace.
_QUALIFIED_RE = re.compile(r"\bores(?:[_a-z0-9]*)?(?:::[A-Za-z_]\w*)+")

_NAMESPACE_RE = re.compile(
    r"namespace\s+([A-Za-z_]\w*(?:::[A-Za-z_]\w*)*)\s*\{"
)

# The export macros sit between the keyword and the name, as in
# ``class ORES_PLATFORM_EXPORT datetime``.
_TYPE_RE = re.compile(
    r"\b(?:class|struct|union|enum(?:\s+class)?)\s+"
    r"(?:[A-Z_][A-Z0-9_]*\s+)*([A-Za-z_]\w*)[^;{]*\{"
)

_IDENTIFIER_RE = re.compile(r"[A-Za-z_]\w*")


def _paste_blocks() -> list[tuple[Path, str, str]]:
    """``(model, block name, body)`` for every :implements block in the tree."""
    blocks = []
    for path in sorted(PROJECTS_DIR.glob("*/modeling/**/*.org")):
        text = path.read_text(encoding="utf-8", errors="replace")
        if ":implements" not in text:
            continue
        for match in _PASTE_BLOCK_RE.finditer(text):
            header = text[match.start():match.start("body")]
            named = _BLOCK_NAME_RE.search(header)
            blocks.append((path, named.group(1) if named else "(unnamed)",
                           match.group("body")))
    return blocks


def _headers() -> list[Path]:
    """Every header under a ``projects/`` include directory."""
    return sorted(p for p in PROJECTS_DIR.rglob("*.hpp") if "include" in p.parts)


def declared_symbols(text: str) -> set[tuple[tuple[str, ...], str]]:
    """``(scope, identifier)`` pairs ``text`` mentions, scope included.

    The scope is the brace path the identifier sits inside. A namespace and a
    class are the same shape here, because a qualified name reads the same way
    through either, and telling a declaration from a use is a compiler's job
    rather than this check's.
    """
    text = _blank_comments_and_literals(text)
    symbols: set[tuple[tuple[str, ...], str]] = set()
    scopes: list[tuple[str, ...]] = []
    scope: tuple[str, ...] = ()
    i = 0
    n = len(text)
    while i < n:
        match = _NAMESPACE_RE.match(text, i)
        if match:
            scope = scope + tuple(match.group(1).split("::"))
            scopes.append(scope)
            i = match.end()
            continue
        match = _TYPE_RE.match(text, i)
        if match:
            symbols.add((scope, match.group(1)))
            scope = scope + (match.group(1),)
            scopes.append(scope)
            i = match.end()
            continue
        char = text[i]
        if char == "{":
            scopes.append(scope)
            i += 1
            continue
        if char == "}":
            if scopes:
                scopes.pop()
            scope = scopes[-1] if scopes else ()
            i += 1
            continue
        match = _IDENTIFIER_RE.match(text, i)
        if match:
            symbols.add((scope, match.group(0)))
            i = match.end()
            continue
        i += 1
    return symbols


def _rel(path: Path) -> str:
    """``path`` relative to REPO_ROOT, or its string form when outside it."""
    try:
        return str(path.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def unresolved(blocks: list, table: set) -> list[tuple[Path, str, str]]:
    """``(model, block name, name)`` for every referenced name the tree lacks."""
    missing = []
    for model, block_name, body in blocks:
        for name in sorted(set(_QUALIFIED_RE.findall(body))):
            parts = name.split("::")
            if (tuple(parts[:-1]), parts[-1]) not in table:
                missing.append((model, block_name, name))
    return missing


def main() -> int:
    blocks = _paste_blocks()
    if not blocks:
        print("no :implements block found under any modeling directory",
              file=sys.stderr)
        return 1

    headers = _headers()
    if not headers:
        print(f"no header found under {PROJECTS_DIR}/*/include", file=sys.stderr)
        return 1

    table: set[tuple[tuple[str, ...], str]] = set()
    for header in headers:
        table |= declared_symbols(header.read_text(encoding="utf-8",
                                                   errors="replace"))

    names = {name for _model, _block, body in blocks
             for name in _QUALIFIED_RE.findall(body)}
    missing = unresolved(blocks, table)
    if not names:
        print(f"no qualified name found in {len(blocks)} :implements block(s); "
              "the pattern this check looks for is a name rooted at ores",
              file=sys.stderr)
        return 1

    if missing:
        print("Pasted block(s) naming a symbol the tree does not declare:",
              file=sys.stderr)
        for model, block_name, name in missing:
            scope = "::".join(name.split("::")[:-1])
            print(f"  {_rel(model)}: block {block_name}: {name}", file=sys.stderr)
            print(f"      nothing in namespace or type {scope} declares "
                  f"{name.split('::')[-1]}", file=sys.stderr)
        print(f"\n{len(missing)} of {len(names)} referenced name(s) do not "
              "resolve. A pasted block is compiled only when its component is "
              "regenerated, so a rename elsewhere leaves it broken in silence. "
              "Point the block at the symbol's new home, or restore the symbol.",
              file=sys.stderr)
        return 1

    print(f"Pasted block symbols resolve: {len(names)} name(s) referenced by "
          f"{len(blocks)} :implements block(s) in "
          f"{len({model for model, _b, _x in blocks})} model(s), against "
          f"{len(table)} declared (scope, identifier) pair(s) in "
          f"{len(headers)} header(s).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
