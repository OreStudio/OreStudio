#!/usr/bin/env python3
"""Count the parent links the entity models declare, and what a parent's
removal does to each: closes the child through a delete cascade, or leaves it.

Run from the repository root. Prints a summary and, with --list, every link.
It reads the model files with regular expressions, so it is a census and not
a check: the task that adds the check reads the models through codegen.
"""
import collections
import glob
import re
import sys

SQL_TABLES = set()
for f in glob.glob("projects/ores.sql/create/**/*.sql", recursive=True):
    SQL_TABLES.update(re.findall(r'create table if not exists "?([a-z0-9_]+)"?', open(f).read()))


def section(text, title):
    m = re.search(rf"^\* {re.escape(title)}\n(.*?)(?=^\* |\Z)", text, re.S | re.M)
    return m.group(1) if m else ""


def table_of(text):
    m = re.search(r"^:tablename:\s*(\S+)", text, re.M)
    if m:
        return m.group(1)
    plural = re.search(r"^#\+entity_plural:\s*(\S+)", text, re.M)
    comp = re.search(r"^#\+component:\s*(\S+)", text, re.M)
    if plural and comp:
        return f"ores_{comp.group(1)}_{plural.group(1)}_tbl"
    return None


def prop(block, name):
    m = re.search(rf"^:{name}:\s*(\S+)", block, re.M)
    return m.group(1) if m else None


models = sorted(glob.glob("projects/*/modeling/*.org"))
# parent table -> {(child table, child column)} its Delete cascade closes
cascade = collections.defaultdict(set)
links = []
dropped = []
for path in models:
    text = open(path).read()
    if "#+type: ores.codegen.entity" not in text:
        continue
    table = table_of(text)
    for block in re.split(r"^\*\* ", section(text, "Delete cascade"), flags=re.M)[1:]:
        if prop(block, "table"):
            cascade[table].add((prop(block, "table"), prop(block, "column")))
    fk_section = section(text, "Foreign keys")
    blocks = re.split(r"^\*\* ", fk_section, flags=re.M)[1:]
    if len(re.findall(r"^:table:", fk_section, re.M)) != len(blocks):
        dropped.append(path)
    for block in blocks:
        links.append({
            "model": path,
            "child": table,
            "column": block.split("\n")[0].strip(),
            "parent": prop(block, "table"),
            "parent_entity": prop(block, "parent_entity") is not None,
            "bump": prop(block, "bump_parent_version") == "true",
            "orphan": prop(block, "cascade_orphan") == "true",
        })

for link in links:
    link["cascaded"] = (link["child"], link["column"]) in cascade.get(link["parent"], set())

pe = [l for l in links if l["parent_entity"]]
bump_only = [l for l in links if l["bump"] and not l["parent_entity"]]
unresolved = sorted({l["child"] for l in links if l["child"] not in SQL_TABLES}
                    | {l["parent"] for l in links if l["parent"] and l["parent"] not in SQL_TABLES})
cascade_entries = sum(len(v) for v in cascade.values())
cascade_with_fk = sum(1 for p, cs in cascade.items() for c in cs
                      if any((l["child"], l["column"]) == c and l["parent"] == p for l in links))

print(f"entity models with foreign keys      : {len({l['model'] for l in links})}")
print(f"foreign keys declared                : {len(links)}")
print(f"  with :parent_entity:               : {len(pe)}")
print(f"    with :bump_parent_version: true  : {sum(l['bump'] for l in pe)}")
print(f"    closed by the parent's cascade   : {sum(l['cascaded'] for l in pe)}")
print(f"  with :bump_parent_version: true    : {sum(l['bump'] for l in links)}")
print(f"    without :parent_entity:          : {len(bump_only)}")
print(f"  closed by the parent's cascade     : {sum(l['cascaded'] for l in links)}")
print(f"  with :cascade_orphan: true         : {sum(l['orphan'] for l in links)}")
print(f"models with a Delete cascade section : {len(cascade)}")
print(f"  child tables they name             : {cascade_entries}")
print(f"  of which the child declares the FK : {cascade_with_fk}")
print(f"table names not found in the SQL     : {len(unresolved)}")
print(f"models whose links the script missed : {len(dropped)}")
if "--list" in sys.argv:
    for l in links:
        print(f"{l['child']}.{l['column']} -> {l['parent']}"
              f"  parent_entity={l['parent_entity']} bump={l['bump']} cascaded={l['cascaded']}")
    for t in unresolved:
        print("unresolved:", t)
    for p in dropped:
        print("missed links in:", p)
