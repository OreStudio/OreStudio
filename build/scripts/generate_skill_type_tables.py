#!/usr/bin/env python3
"""Regenerate the type tables the polymorphic skills dispatch on.

A verb that behaves uniformly across artefact types is one skill plus a
table, and the table is derived rather than typed by hand:

  doc-add   the document types, read from ores.codegen's doc_*.org.mustache
            templates: the scaffold command, the parent-dir default compass
            applies, and the sections the template leaves to be filled.
  doc-show  the read-only compass commands, read from compass.py's own
            subparser registry.

Both regions sit between BEGIN/END markers; everything outside them is
preserved. Never edit a generated region by hand — fix the source (or this
script) and re-run.

Usage: python3 build/scripts/generate_skill_type_tables.py [--check]
  --check  exit non-zero if a table is stale (for CI).
"""
import argparse
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
TEMPLATES = ROOT / "projects" / "ores.codegen" / "library" / "templates"
COMPASS = ROOT / "projects" / "ores.compass" / "src" / "compass.py"
SKILLS = ROOT / "doc" / "llm" / "skills"

ADD_SKILL = SKILLS / "doc-add" / "SKILL.org"
SHOW_SKILL = SKILLS / "doc-show" / "SKILL.org"

SCRIPT = "build/scripts/generate_skill_type_tables.py"
ADD_BEGIN = f"# BEGIN generated types ({SCRIPT})"
ADD_END = "# END generated types"
SHOW_BEGIN = f"# BEGIN generated targets ({SCRIPT})"
SHOW_END = "# END generated targets"

# Types whose scaffold takes flags rather than a parent dir, or whose home is
# fixed by the model tree rather than by the doc tree. They scaffold through
# the same verb, so they belong in the table with their own placement note.
MODEL_TYPES = {
    "entity_org": "projects/ores.<component>/modeling/",
    "field_group": "projects/ores.<component>/modeling/",
    "dataset": "projects/ores.seeder/datasets/<name>/",
    "profile": "projects/modeling/",
    "feature": "projects/modeling/",
    "facet": "projects/ores.codegen/library/templates",
    "facet_group": "projects/ores.codegen/library/templates",
}

# Read-only commands: the ones whose whole job is to show state. This is the
# one hand-maintained input, because compass has no machine-readable
# read-only flag; everything else in the row comes from the parser itself.
READ_ONLY = [
    "show", "search", "list", "where", "fleet", "journal", "timeline",
    "heading", "bearings", "inbox", "next", "deferred", "discarded",
]

# Subcommands that show state under a command that also mutates. The verb is
# uniform; only the target differs.
READ_ONLY_SUB = {
    "sprint status": "the sprint's stories grouped by state",
    "sprint audit": "drift between story files and the sprint table",
    "story status": "one story's tasks and their states",
    "services status": "which services run, which stopped or went missing",
    "client status": "whether the Qt client is up",
    "pr checks": "CI state on a pull request",
    "test results": "the last ctest run",
    "review list": "the review comments on a pull request",
    "env list": "the provisioned worktrees",
    "db sql": "whatever the query returns",
    "skills report": "skill-selection quality over a window",
}


def read(path):
    return path.read_text(encoding="utf-8")


def document_types():
    """(type, scaffold, home, sections) per doc_*.org.mustache template."""
    static_parent = dict(
        re.findall(r'^\s*"([a-z_]+)":\s*"([^"]+)",\s*$',
                   re.search(r"_STATIC_PARENT = \{(.*?)\}", read(COMPASS), re.S).group(1),
                   re.M))
    defaultable = dict(
        re.findall(r'"([a-z_]+)":\s*"([a-z_]+)"',
                   re.search(r"_DEFAULTABLE_PARENT = \{(.*?)\}", read(COMPASS), re.S).group(1)))

    rows = []
    for tpl in sorted(TEMPLATES.glob("doc_*.org.mustache")):
        name = tpl.name[len("doc_"):-len(".org.mustache")]
        text = read(tpl)
        sections = [m.group(1).strip()
                    for m in re.finditer(r"^\* (.+?)\s*$", text, re.M)]
        sections = [re.sub(r"\s{2,}:[\w:]+:$", "", s) for s in sections
                    if ":noexport:" not in s]
        if name in MODEL_TYPES:
            home = MODEL_TYPES[name]
        elif name in static_parent:
            home = static_parent[name]
        elif name in defaultable:
            home = f"the current {defaultable[name]}"
        else:
            home = "--parent-dir"
        rows.append((name, f"compass add {name}", home, sections))
    return rows


def read_only_commands():
    """(command, what it shows) for every read-only compass command."""
    text = read(COMPASS)
    helps = {}
    for m in re.finditer(
            r'subparsers\.add_parser\(\s*"([a-z-]+)"\s*,(.*?)\)\s*\n', text, re.S):
        name, body = m.group(1), m.group(2)
        h = re.search(r'help=((?:\s*"(?:[^"\\]|\\.)*")+)', body)
        if h:
            helps[name] = " ".join(
                x.strip() for x in re.findall(r'"((?:[^"\\]|\\.)*)"', h.group(1)))
    rows = []
    for name in READ_ONLY:
        blurb = helps.get(name, "")
        blurb = blurb.split(";")[0].split("—")[-1].strip().rstrip(".")
        rows.append((f"compass {name}", blurb or name))
    for sub, blurb in READ_ONLY_SUB.items():
        rows.append((f"compass {sub}", blurb))
    return rows


def add_table():
    lines = [ADD_BEGIN, "",
             "One row per document type. The scaffold command, where it lands, "
             "and the sections the template leaves for you to fill.", "",
             "#+ATTR_HTML: :class hug-leading",
             "| Type | Scaffold | Lands in | Sections to fill |",
             "|------+----------+----------+------------------|"]
    for name, scaffold, home, sections in document_types():
        filled = ", ".join(f"={s}=" for s in sections) if sections else "—"
        lines.append(f"| ={name}= | ={scaffold}= | ={home}= | {filled} |")
    lines += ["", ADD_END]
    return "\n".join(lines)


def show_table():
    lines = [SHOW_BEGIN, "",
             "One row per read-only command. Each shows state and changes "
             "nothing, so any of them is safe to run while deciding what to do "
             "next.", "",
             "#+ATTR_HTML: :class hug-leading",
             "| Command | Shows |",
             "|---------+-------|"]
    for cmd, blurb in read_only_commands():
        lines.append(f"| ={cmd}= | {blurb} |")
    lines += ["", SHOW_END]
    return "\n".join(lines)


def splice(path, begin, end, block):
    text = read(path)
    pattern = re.escape(begin) + r".*?" + re.escape(end)
    if not re.search(pattern, text, re.S):
        print(f"❌ markers not found in {path}", file=sys.stderr)
        return None
    return re.sub(pattern, lambda _m: block, text, flags=re.S)


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true",
                    help="exit non-zero if a table is stale")
    args = ap.parse_args()

    stale = False
    for path, begin, end, block in (
            (ADD_SKILL, ADD_BEGIN, ADD_END, add_table()),
            (SHOW_SKILL, SHOW_BEGIN, SHOW_END, show_table())):
        updated = splice(path, begin, end, block)
        if updated is None:
            return 1
        if updated == read(path):
            continue
        if args.check:
            print(f"❌ stale: {path.relative_to(ROOT)}", file=sys.stderr)
            stale = True
        else:
            path.write_text(updated, encoding="utf-8")
            print(f"✅ regenerated {path.relative_to(ROOT)}")
    if stale:
        print("   run: python3 " + SCRIPT, file=sys.stderr)
        return 1
    if args.check:
        print("✅ skill type tables are up to date.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
