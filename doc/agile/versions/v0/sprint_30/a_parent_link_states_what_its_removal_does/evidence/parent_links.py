#!/usr/bin/env python3
"""Count the parent links the entity models declare, and what a parent's
removal does to each: closes the child through a delete cascade, or leaves it.

Run from the repository root. Prints a summary and, with --list, every link.
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


models = sorted(glob.glob("projects/*/modeling/*.org"))
cascade = collections.defaultdict(set)
links = []
for path in models:
    text = open(path).read()
    if "#+type: ores.codegen.entity" not in text:
        continue
    child = table_of(text)
    for t in re.findall(r":table:\s*(\S+)", section(text, "Delete cascade")):
        cascade[child].add(t)
    for block in re.split(r"^\*\* ", section(text, "Foreign keys"), flags=re.M)[1:]:
        parent = re.search(r":table:\s*(\S+)", block)
        links.append({
            "model": path,
            "child": child,
            "column": block.split("\n")[0].strip(),
            "parent": parent.group(1) if parent else None,
            "parent_entity": ":parent_entity:" in block,
            "bump": re.search(r":bump_parent_version:\s*true", block) is not None,
            "orphan": re.search(r":cascade_orphan:\s*true", block) is not None,
        })

for link in links:
    link["cascaded"] = link["child"] in cascade.get(link["parent"], set())

pe = [l for l in links if l["parent_entity"]]
unresolved = sorted({l["child"] for l in links if l["child"] not in SQL_TABLES}
                    | {l["parent"] for l in links if l["parent"] and l["parent"] not in SQL_TABLES})
cascade_entries = sum(len(v) for v in cascade.values())
cascade_with_fk = sum(1 for p, cs in cascade.items() for c in cs
                      if any(l["child"] == c and l["parent"] == p for l in links))

print(f"entity models with foreign keys      : {len({l['model'] for l in links})}")
print(f"foreign keys declared                : {len(links)}")
print(f"  with :parent_entity:               : {len(pe)}")
print(f"    with :bump_parent_version: true  : {sum(l['bump'] for l in pe)}")
print(f"    closed by the parent's cascade   : {sum(l['cascaded'] for l in pe)}")
print(f"  closed by the parent's cascade     : {sum(l['cascaded'] for l in links)}")
print(f"  with :cascade_orphan: true         : {sum(l['orphan'] for l in links)}")
print(f"models with a Delete cascade section : {len(cascade)}")
print(f"  child tables they name             : {cascade_entries}")
print(f"  of which the child declares the FK : {cascade_with_fk}")
print(f"table names not found in the SQL     : {len(unresolved)}")
if "--list" in sys.argv:
    for l in links:
        print(f"{l['child']}.{l['column']} -> {l['parent']}"
              f"  parent_entity={l['parent_entity']} bump={l['bump']} cascaded={l['cascaded']}")
    for t in unresolved:
        print("unresolved:", t)
