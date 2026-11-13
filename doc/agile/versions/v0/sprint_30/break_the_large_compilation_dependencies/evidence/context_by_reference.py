#!/usr/bin/env python3
"""Take the database context by reference, in the templates and in hand-written code.

The change is mechanical and wide: every function that read a context by value
read a copy, and 21874 of them exist. A hand edit is not repeatable, so this
script does the edit and can be rerun after a rebase.

What it changes, and what it refuses to change:

- A *parameter* named ``ctx`` becomes ``const context&``. The rule needs the
  position, not the spelling: ``context ctx`` followed by ``,`` or ``)`` is a
  parameter, while ``context ctx_;`` is a member and ``context ctx = ...`` is a
  local. Both keep their copy, because the analysis found they need it.
- A *member* keeps its copy. The measured cost of the definition fell from
  6.60 s to 2.85 s once the pool, libpq and the codec chain left the header, and
  a pointer member would buy only the service headers' share of that for 103
  dereference sites and a change in who owns the context.
- ``std::move(ctx)`` cannot survive a ``const&`` parameter. The generated
  constructors copy into the member instead, which is what they did anyway.
- A *model* holds code, not only prose. A ``#+begin_src cpp`` block in a
  ``modeling/*.org`` file is a snippet the generator pastes into a generated
  file, so a parameter in one is a real parameter and ``--models`` takes it by
  reference too. Editing the generated file instead loses the edit at the next
  regeneration, and the drift gate says so.
- An *include* is not a use. A header that only names the context includes
  ``domain/context_fwd.hpp`` and not ``domain/context.hpp``. ``--includes``
  asks the check in ``build/scripts/check_forward_declarable_includes.py`` which
  headers those are, so the fixer and the guard cannot disagree about what
  "only names" means.

Comment lines are skipped: the prose "the context names who a statement acts
for" is a sentence, not a parameter.

Usage:

    python3 context_by_reference.py --templates
    python3 context_by_reference.py --models
    python3 context_by_reference.py --sources
    python3 context_by_reference.py --includes
    python3 context_by_reference.py --report
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path

# Every context parameter name the tree uses, with an optional qualifier. The
# qualifier matters: a first cut matched the tail of "ores::database::context
# ctx" and wrote "ores::database::const context&", which the compiler would have
# rejected in 2038 places. The qualifier is captured and kept in front.
PARAMETER_RE = re.compile(
    r"(?<!\w)((?:\w+::)*)context (ctx|base_ctx|db_ctx|system_ctx)"
    r"(?=\s*(?:[,)]|\{\{))")
MOVE_MEMBER_RE = re.compile(r"\b(\w+)_\(std::move\(\1\)\)")
CONTEXT_NAMES = ("ctx", "base_ctx", "db_ctx", "system_ctx")
MOVE_CALL_RE = re.compile(r"std::move\(ctx\)")
GENERATED_MARKER = "AUTO-GENERATED FILE"

# A coroutine keeps its parameters across a suspension point, so a reference
# there outlives the temporary at the call site. The compiler cannot see it and
# the tests did not reach it: the IAM service aborted with heap corruption and
# valgrind named the borrowed context as the first invalid access. These files
# are the service runners; their context parameters must stay by value.
MUST_COPY_FILES = (
    "ores.service/include/ores.service/service/domain_service_runner.hpp",
    "ores.service/include/ores.service/service/domain_service_runner_impl.hpp",
    "ores.service/include/ores.service/service/signing_service_runner.hpp",
    "ores.service/include/ores.service/service/signing_service_runner_impl.hpp",
)
TEMPLATE_DIR = "projects/ores.codegen/library/templates"

DEFINING_HEADER = "ores.database/domain/context.hpp"
FORWARDING_HEADER = "ores.database/domain/context_fwd.hpp"
INCLUDE_LINE_RE = re.compile(
    r'^([ \t]*)#[ \t]*include[ \t]*([<"][^>"]*[>"])(.*)$')

# Templates that carry the parameter, found by counting. Editing the literate
# source is the edit that lasts; the .mustache beside it is tangled from it.
TEMPLATES = (
    "ores.cpp.repository.repository_header.org",
    "ores.cpp.repository.repository_impl.org",
    "ores.cpp.service.service_header.org",
    "ores.cpp.service.service_impl.org",
    "ores.cpp.nats-handler.nats_handler_header.org",
    "ores.cpp.nats-sub-registrar.nats_registrar_header.org",
    "ores.cpp.nats-sub-registrar.nats_registrar_implementation.org",
    "ores.cpp.trade_type.writer.org",
    "ores.cpp.trade_type.digest.org",
)


def is_comment(line: str) -> bool:
    stripped = line.lstrip()
    return (stripped.startswith("*") or stripped.startswith("//")
            or stripped.startswith("/*"))


def edit_text(text: str, moves: bool) -> tuple[str, int]:
    """Take every context parameter by reference; count the changes."""
    changed = 0
    out = []
    for line in text.splitlines(keepends=True):
        if is_comment(line):
            out.append(line)
            continue
        new_line, count = PARAMETER_RE.subn(
            r"const \1context& \2", line)
        changed += count
        if moves:
            new_line = MOVE_MEMBER_RE.sub(r"\1_(\1)", new_line)
            # A handler receives the context from its registrar; with a
            # const& parameter there is nothing to move.
            for name in CONTEXT_NAMES:
                new_line = new_line.replace(f"std::move({name})", name)
        out.append(new_line)
    return "".join(out), changed


def run_templates(root: Path, moves: bool) -> int:
    total = 0
    for name in TEMPLATES:
        path = root / TEMPLATE_DIR / name
        if not path.is_file():
            print(f"missing template: {path}")
            continue
        text = path.read_text(encoding="utf-8")
        new_text, changed = edit_text(text, moves)
        if changed:
            path.write_text(new_text, encoding="utf-8")
        total += changed
        print(f"{changed:>4}  {name}")
    print(f"template parameters taken by reference: {total}")
    return 0


def run_sources(root: Path, include_generated: bool = False) -> int:
    total = 0
    touched = 0
    for path in sorted(root.glob("projects/**/*")):
        if path.suffix not in (".hpp", ".cpp", ".h", ".ipp"):
            continue
        if "/build/" in str(path) or not path.is_file():
            continue
        text = path.read_text(encoding="utf-8", errors="replace")
        if any(skip in str(path) for skip in MUST_COPY_FILES):
            continue
        if not text or "context ctx" not in text:
            continue
        generated = GENERATED_MARKER in text
        if generated and not include_generated:
            continue
        new_text, changed = edit_text(text, moves=True)
        if changed:
            path.write_text(new_text, encoding="utf-8")
            touched += 1
            total += changed
    kind = "hand-written and generated" if include_generated else "hand-written"
    print(f"{kind} parameters taken by reference: {total} in {touched} files")
    return 0


def include_priority(target: str) -> int:
    """The order clang-format's IncludeCategories gives this include."""
    if target.startswith('"'):
        return 1
    if re.match(r"<Q[A-Z]", target):
        return 2
    if target.startswith("<boost/"):
        return 3
    if re.match(r"<(iphlpapi|ws2tcpip|psapi|tlhelp32|dbghelp|shellapi)\.h>", target):
        return 5
    return 4


def swap_include(text: str) -> tuple[str, bool]:
    """The defining include becomes the forwarding one, in its sorted place."""
    lines = text.splitlines(keepends=True)
    index = None
    for position, line in enumerate(lines):
        match = INCLUDE_LINE_RE.match(line)
        if match and match.group(2).strip('<>"') == DEFINING_HEADER:
            index = position
            break
    if index is None:
        return text, False
    match = INCLUDE_LINE_RE.match(lines[index])
    lines[index] = f'{match.group(1)}#include "{FORWARDING_HEADER}"{match.group(3)}\n'
    start = index
    while start > 0 and INCLUDE_LINE_RE.match(lines[start - 1]):
        start -= 1
    end = index
    while end + 1 < len(lines) and INCLUDE_LINE_RE.match(lines[end + 1]):
        end += 1
    block = sorted(lines[start:end + 1],
                   key=lambda line: (include_priority(
                       INCLUDE_LINE_RE.match(line).group(2)),
                       INCLUDE_LINE_RE.match(line).group(2)))
    lines[start:end + 1] = block
    return "".join(lines), True


def run_models(root: Path) -> int:
    """The same parameter change in the snippets a model pastes into code."""
    total = 0
    touched = 0
    for path in sorted(root.glob("projects/*/modeling/*.org")):
        if not path.is_file():
            continue
        text = path.read_text(encoding="utf-8")
        if "context ctx" not in text:
            continue
        out = []
        changed = 0
        in_cpp = False
        for line in text.splitlines(keepends=True):
            stripped = line.strip().lower()
            if stripped.startswith("#+begin_src"):
                in_cpp = "cpp" in stripped
            elif stripped.startswith("#+end_src"):
                in_cpp = False
            elif in_cpp and not is_comment(line):
                new_line, count = PARAMETER_RE.subn(r"const \1context& \2", line)
                changed += count
                line = MOVE_MEMBER_RE.sub(r"\1_(\1)", new_line)
                for name in CONTEXT_NAMES:
                    line = line.replace(f"std::move({name})", name)
            out.append(line)
        if changed:
            path.write_text("".join(out), encoding="utf-8")
            touched += 1
            total += changed
            print(f"    {path.relative_to(root)}")
    print(f"model parameters taken by reference: {total} in {touched} files")
    return 0


def run_includes(root: Path) -> int:
    """Every header that only names the context takes its declaration.

    The set comes from the check, not from a list kept here: a header the check
    stops reporting must not keep a forwarding include it no longer needs, and a
    header it starts reporting must not be missed because this script had its
    own idea of what a header uses.
    """
    import importlib.util
    import sys

    spec = importlib.util.spec_from_file_location(
        "check_forward_declarable_includes",
        root / "build/scripts/check_forward_declarable_includes.py")
    check = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = check
    spec.loader.exec_module(check)

    offenders = check.find_offenders(root, check.read_declarables(check.LIST_PATH))
    hand_written = [offender for offender in offenders
                    if offender.template is None
                    and offender.decl.name == "ores::database::context"]
    skipped = sorted({offender.path.relative_to(root).as_posix()
                      for offender in offenders if offender.template is not None})

    touched = 0
    for offender in hand_written:
        text = offender.path.read_text(encoding="utf-8")
        new_text, changed = swap_include(text)
        if changed:
            offender.path.write_text(new_text, encoding="utf-8")
            touched += 1
            print(f"    {offender.path.relative_to(root)}")
    print(f"headers that take the declaration instead of the definition: "
          f"{touched}")
    if skipped:
        print("generated templates that still include the definition, and are")
        print("fixed in their archetype rather than here:")
        for entry in skipped:
            print(f"    {entry}")
    return 0


def report(root: Path) -> int:
    """What a plain rename would miss, so it is reviewed by hand."""
    pattern = re.compile(r"\bcontext ([a-z_][a-z_0-9]*)\s*[,)]")
    seen = {}
    for path in sorted(root.glob("projects/**/*")):
        if path.suffix not in (".hpp", ".cpp", ".h", ".ipp") or not path.is_file():
            continue
        if "/build/" in str(path):
            continue
        text = path.read_text(encoding="utf-8", errors="replace")
        if GENERATED_MARKER in text:
            continue
        for line in text.splitlines():
            if is_comment(line):
                continue
            for match in pattern.finditer(line):
                if match.group(1) == "ctx":
                    continue
                seen.setdefault(match.group(1), []).append(str(path))
    print("== files that hold a context across a suspension or a callback")
    for path in sorted(root.glob("projects/**/*.hpp")):
        if not path.is_file() or "/build/" in str(path):
            continue
        text = path.read_text(encoding="utf-8", errors="replace")
        if "awaitable<" in text and "context" in text:
            print(f"    {path}")
    print()
    if not seen:
        print("no context parameter carries a name other than ctx")
    for name, files in sorted(seen.items()):
        print(f"{name}: {len(files)} site(s)")
        for entry in files[:3]:
            print(f"    {entry}")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--templates", action="store_true")
    group.add_argument("--models", action="store_true",
                       help="take the parameter by reference in the code "
                            "snippets a modeling file pastes into code")
    group.add_argument("--sources", action="store_true")
    group.add_argument("--includes", action="store_true",
                       help="swap the context's definition for its declaration "
                            "in every header that only names it")
    parser.add_argument("--include-generated", action="store_true",
                        help="with --sources, also edit generated files that no "
                             "longer regenerate from the current templates")
    group.add_argument("--report", action="store_true")
    args = parser.parse_args()

    root = Path(__file__).resolve().parents[7]
    if args.templates:
        return run_templates(root, moves=True)
    if args.models:
        return run_models(root)
    if args.sources:
        return run_sources(root, args.include_generated)
    if args.includes:
        return run_includes(root)
    return report(root)


if __name__ == "__main__":
    raise SystemExit(main())
