#!/usr/bin/env python3
"""Check that every registered HTTP route states its authentication position.

The gateway's route builder used to treat "no declaration" as public, because
``domain::route::requires_auth`` defaults to false. That is a silent hole: a
route that never mentions authentication answers anyone. The builder now
refuses such a route at ``build()`` and the router refuses it at
``add_route()``, so the mistake fails loudly at startup.

This gate catches the same mistake in CI, before the server is ever run. It
reads the C++ sources under ``projects/ores.http`` and, for every route added
to the router, requires the builder chain to carry ``.auth_required()``,
``.auth_optional()`` or ``.roles(...)``. A chain that carries none is a
violation.

Production sources only: a test may build an undeclared route on purpose, to
prove the refusal fires.

Run::

    python3 projects/ores.codegen/scripts/check_route_auth_declarations.py
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
HTTP_ROOT = REPO_ROOT / "projects" / "ores.http"

# The builder chain must name one of these. roles() implies a session, so it
# is a declaration too.
DECLARATION_RE = re.compile(r"\.\s*(?:auth_required|auth_optional|roles)\s*\(")

# The route an add_route call registers, for the failure message: the builder
# method that opened the chain and the pattern it stated.
ROUTE_RE = re.compile(r"(?:->|\.)\s*(get|post|put|patch|delete_|head|options)\s*\(\s*\"([^\"]*)\"")

ADD_ROUTE_RE = re.compile(r"(?<=[.>])\s*add_route\s*\(")
VARIABLE_BUILD_RE = re.compile(r"^\s*([A-Za-z_]\w*)\s*\.\s*build\s*\(\s*\)\s*$")
BARE_NAME_RE = re.compile(r"^[A-Za-z_]\w*$")

# A route is built once and registered twice, so the call names the built
# local rather than the builder. That local is an alias for the builder whose
# chain states the position, and the chain is read through the alias.
BUILD_ALIAS_RE = re.compile(
    r"^\s*[A-Za-z_]\w*\s*=\s*([A-Za-z_]\w*)\s*\.\s*build\s*\(\s*\)\s*;")

# A scan that finds almost nothing has broken, whatever the tree says. The
# tree registers far more than this; the floor exists so a regex that stops
# matching fails the gate instead of passing every build.
MINIMUM_ROUTES = 20


def _mask_code(text: str) -> list[bool]:
    """True where a character is code, False inside a comment or a literal."""
    mask = [True] * len(text)
    n = len(text)
    i = 0
    while i < n:
        c = text[i]
        if c == "/" and i + 1 < n and text[i + 1] == "/":
            while i < n and text[i] != "\n":
                mask[i] = False
                i += 1
        elif c == "/" and i + 1 < n and text[i + 1] == "*":
            end = text.find("*/", i + 2)
            end = n if end == -1 else end + 2
            for k in range(i, end):
                mask[k] = False
            i = end
        elif c == '"':
            if i >= 1 and text[i - 1] == "R":
                # A raw string runs to )delim", so its contents are not code.
                paren = text.find("(", i + 1)
                if paren == -1:
                    mask[i] = False
                    i += 1
                    continue
                delim = text[i + 1:paren]
                close = ")" + delim + '"'
                end = text.find(close, paren + 1)
                end = n if end == -1 else end + len(close)
                for k in range(i, end):
                    mask[k] = False
                i = end
            else:
                j = i + 1
                while j < n:
                    if text[j] == "\\":
                        j += 2
                        continue
                    if text[j] == '"':
                        break
                    j += 1
                end = min(j + 1, n)
                for k in range(i, end):
                    mask[k] = False
                i = end
        elif c == "'":
            j = i + 1
            while j < n:
                if text[j] == "\\":
                    j += 2
                    continue
                if text[j] == "'":
                    break
                j += 1
            end = min(j + 1, n)
            for k in range(i, end):
                mask[k] = False
            i = end
        else:
            i += 1
    return mask


def _close_paren(text: str, mask: list[bool], open_index: int) -> int:
    """The index of the ')' matching the '(' at open_index, or -1."""
    depth = 0
    for i in range(open_index, len(text)):
        if not mask[i]:
            continue
        if text[i] == "(":
            depth += 1
        elif text[i] == ")":
            depth -= 1
            if depth == 0:
                return i
    return -1


def _statement_end(text: str, mask: list[bool], start: int) -> int:
    """The index just past the ';' that ends the statement opening at start."""
    paren = brace = bracket = 0
    for i in range(start, len(text)):
        if not mask[i]:
            continue
        c = text[i]
        if c == "(":
            paren += 1
        elif c == ")":
            paren -= 1
        elif c == "{":
            brace += 1
        elif c == "}":
            brace -= 1
        elif c == "[":
            bracket += 1
        elif c == "]":
            bracket -= 1
        elif c == ";" and paren == 0 and brace == 0 and bracket == 0:
            return i + 1
    return len(text)


def _declared(chain: str) -> bool:
    """Whether the builder chain carries an explicit authentication position."""
    return DECLARATION_RE.search(_masked_text(chain)) is not None


def _variable_chain(text: str, mask: list[bool], name: str) -> str | None:
    """The builder chain bound to ``name`` by an assignment, if any."""
    pattern = re.compile(r"(?<![\w.])" + re.escape(name) + r"\s*=")
    for match in pattern.finditer(text):
        if not mask[match.start()]:
            continue
        head = text.rfind("\n", 0, match.start()) + 1
        if "auto" not in text[head:match.start()]:
            continue
        return text[match.start():_statement_end(text, mask, match.start())]
    return None


def _builder_chain(text: str,
                   mask: list[bool],
                   name: str,
                   seen: set[str] | None = None) -> str | None:
    """The builder chain that produces ``name``, following build aliases."""
    seen = set() if seen is None else seen
    if name in seen:
        return None
    seen.add(name)
    chain = _variable_chain(text, mask, name)
    if chain is None:
        return None
    alias = BUILD_ALIAS_RE.match(chain)
    if alias is None:
        return chain
    return _builder_chain(text, mask, alias.group(1), seen)


def check_file(path: Path) -> list[str]:
    """Every add_route call in the file that states no authentication."""
    text = path.read_text(encoding="utf-8")
    mask = _mask_code(text)
    violations: list[str] = []
    for match in ADD_ROUTE_RE.finditer(text):
        if not mask[match.start()]:
            continue
        open_index = text.find("(", match.start())
        if open_index == -1:
            continue
        close_index = _close_paren(text, mask, open_index)
        if close_index == -1:
            continue
        argument = text[open_index + 1:close_index]

        variable = VARIABLE_BUILD_RE.match(argument)
        if variable:
            chain = _builder_chain(text, mask, variable.group(1))
            if chain is None:
                # A route added through a name this file never binds cannot be
                # read here. The router's own refusal still covers it.
                continue
        else:
            name = argument.strip()
            chain = (_builder_chain(text, mask, name)
                     if BARE_NAME_RE.match(name) else None) or argument

        if _declared(chain):
            continue

        line = text.count("\n", 0, match.start()) + 1
        route = ROUTE_RE.search(chain)
        where = f"{route.group(1).upper()} {route.group(2)}" if route else "an unnamed route"
        try:
            shown = path.relative_to(REPO_ROOT)
        except ValueError:
            shown = path
        violations.append(
            f"{shown}:{line}: {where} states no authentication "
            "position; call auth_required() or auth_optional()")
    return violations


def source_files(root: Path = HTTP_ROOT) -> list[Path]:
    """Production sources only; a test may omit a declaration on purpose."""
    files: list[Path] = []
    for extension in ("*.cpp", "*.hpp"):
        files.extend(root.rglob(extension))
    return sorted(path for path in files
                  if "tests" not in path.parts and "build" not in path.parts)


def check(root: Path = HTTP_ROOT) -> list[str]:
    """Every registered route the tree states no authentication position for."""
    violations: list[str] = []
    found = 0
    for path in source_files(root):
        text = path.read_text(encoding="utf-8")
        if ADD_ROUTE_RE.search(_masked_text(text)):
            found += sum(1 for _ in ADD_ROUTE_RE.finditer(_masked_text(text)))
        violations.extend(check_file(path))
    if found < MINIMUM_ROUTES:
        violations.append(
            f"only {found} add_route call(s) found under {root}; the gate scanned "
            "almost nothing, which is a failure of the gate rather than a pass")
    return violations


def _masked_text(text: str) -> str:
    """The text with literals and comments blanked, for a bare add_route count."""
    mask = _mask_code(text)
    return "".join(c if mask[i] else " " for i, c in enumerate(text))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--all", action="store_true",
                        help="accepted for the shared gate loop; the check is always tree-wide")
    parser.parse_args()

    violations = check()
    for violation in violations:
        print(violation)
    if violations:
        print(f"\n{len(violations)} route(s) without an explicit authentication position.")
        return 1
    print("every registered HTTP route states its authentication position.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
