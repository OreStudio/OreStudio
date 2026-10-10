"""Derive the pending-change entity model of every gated entity model.

An entity model marks itself gated with ``:gated: true`` in its ``* Flags``
drawer. The pending change of such an entity is a table with the entity's own
columns, the same foreign keys and the same lookup validation, plus the request
it belongs to, a line number, the operation and the version the maker read. A
line holds no part: the request holds the parts, chosen by the policy from the
columns its lines change. This script writes that table's model as an
ordinary entity model beside the source, so every existing archetype applies to
it and a change to the source reaches the pending table in the same
regeneration.

The derived model carries no natural keys, no checks and no insert-trigger
rules. Those are the live row's rules, and the dry run runs them by calling the
real write inside a savepoint.

``compass codegen generate`` calls :func:`write_derived` after it generates a
gated model, then generates the derived model. ``scripts/derive_pending_models.py``
runs :func:`main` over every gated model, and its ``--check`` fails when a
derived model is stale.
"""

import argparse
import re
import sys
import uuid
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
NAMESPACE = uuid.UUID("6d1f3a52-8c7b-4e0a-9b64-2f5d8a1c7e90")

# The columns every pending line adds, ahead of the entity's own columns.
LINE_COLUMNS = """\
** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

UUID identifying this line.

One proposed write inside a request.

** request_id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:natural_key: true
:END:

The approval request this line belongs to.

References the approval requests table.

#+begin_src cpp :name generator
ctx.generate_uuid()
#+end_src

** line_no
:PROPERTIES:
:type:        integer
:cpp_type:    int
:natural_key: true
:END:

The place of this line in its request, from one.

#+begin_src cpp :name generator
1
#+end_src

** operation
:PROPERTIES:
:type:     text
:cpp_type: std::string
:END:

What the line proposes: put or delete.

#+begin_src cpp :name generator
std::string("put")
#+end_src

** base_version
:PROPERTIES:
:type:     integer
:cpp_type: int
:END:

The version of the live row the maker read, or zero for a new row.

#+begin_src cpp :name generator
0
#+end_src

"""


FOREIGN_KEYS_HEAD = """\
* Foreign keys

** request_id
:PROPERTIES:
:table:         ores_inbox_approval_requests_tbl
:target_column: id
:nullable:      false
:error_message: Invalid request_id: %. No approval request found with this id.
:END:

The request the line belongs to.

"""


def split_sections(text):
    """Split a model into its front matter and its top-level sections."""
    parts = re.split(r"(?m)^(?=\* )", text)
    return parts[0], parts[1:]


def section_name(section):
    return section.splitlines()[0][2:].strip()


def keyword(front, name):
    m = re.search(rf"(?m)^#\+{name}:\s*(.*)$", front)
    return m.group(1).strip() if m else ""


def is_gated(text):
    _, sections = split_sections(text)
    for s in sections:
        if section_name(s) == "Flags":
            return re.search(r"(?m)^:gated:\s*true\s*$", s) is not None
    return False


def split_columns(section):
    """Split the ``* Columns`` section into its ``**`` columns."""
    body = section.split("\n", 1)[1]
    chunks = re.split(r"(?m)^(?=\*\* )", body)
    return chunks[0], chunks[1:]


def derive_column(chunk):
    """The entity's column as a pending column: no key, no uniqueness."""
    name = chunk.splitlines()[0][3:].strip()
    chunk = re.sub(r"(?m)^:natural_key:.*\n", "", chunk)
    if name == "id":
        chunk = re.sub(r"(?m)^:primary_key:.*\n", "", chunk)
        assert chunk.startswith("** id\n"), "id column heading changed"
        chunk = chunk.replace("** id\n", "** entity_id\n", 1)
    return chunk


def derive(source, text):
    front, sections = split_sections(text)
    component = keyword(front, "component")
    singular = keyword(front, "entity_singular")
    title = keyword(front, "entity_title")
    new_singular = f"{singular}_change"
    new_plural = f"{singular}_changes"
    new_title = f"{title} Change"

    out = []
    out.append(
        ":PROPERTIES:\n"
        f":ID: {str(uuid.uuid5(NAMESPACE, keyword(front, 'title') or source.name)).upper()}\n"
        ":END:\n"
        f"#+title: ores.{component}.{new_singular}\n"
        f"#+description: GENERATED from ores.{component}.{singular} by "
        "derive_pending_models.py. One proposed write to the "
        f"{singular}, held until its request is decided.\n"
        "#+type: ores.codegen.entity\n"
        f"#+component: {component}\n"
        f"#+filetags: :model:entity:{component}:pending:\n"
        f"#+brief: One proposed write to a {singular}, held for its request.\n"
        f"#+entity_singular: {new_singular}\n"
        f"#+entity_plural: {new_plural}\n"
        f"#+entity_title: {new_title}\n"
        "#+created: 2026-10-10\n"
        "#+updated: 2026-10-10\n\n"
        f"Do not edit. Change ores.{component}.{singular} and run\n"
        "derive_pending_models.py.\n\n"
    )

    by_name = {section_name(s): s for s in sections}
    for needed in ("Flags", "Columns", "C++"):
        if needed not in by_name:
            raise SystemExit(f"{source}: gated model has no '* {needed}' section")
    flags = by_name["Flags"]
    flags = re.sub(r"(?m)^:gated:.*\n", "", flags)
    flags, replaced = re.subn(
        r"(?m)^:profile:.*$", ":profile:   uuid-identified-lookup\n:client_read_only: true", flags
    )
    if not replaced:
        raise SystemExit(f"{source}: gated model has no ':profile:' flag")
    out.append(flags if flags.endswith("\n") else flags + "\n")

    _, columns = split_columns(by_name["Columns"])
    out.append("* Columns\n\n")
    out.append(LINE_COLUMNS)
    for chunk in columns:
        out.append(derive_column(chunk))
    out.append("\n")

    sql = "* SQL\n\n** Flags\n:PROPERTIES:\n"
    sql += f":tablename: ores_{component}_{new_plural}_tbl\n:END:\n\n"
    out.append(sql)

    fks = by_name.get("Foreign keys")
    out.append(FOREIGN_KEYS_HEAD)
    if fks:
        body = fks.split("\n", 1)[1].strip("\n")
        body = re.sub(r"(?m)^:(list_by|bump_parent_version|list_by_as_of):.*\n", "", body)
        out.append(body + "\n\n")

    trigger = by_name.get("Insert trigger")
    if trigger and "\n** Validations" in trigger:
        validations = trigger.split("\n** Validations", 1)[1]
        validations = re.split(r"(?m)^\*\* ", validations)[0]
        out.append("* Insert trigger\n\n** Validations" + validations.rstrip("\n") + "\n\n")

    out.append("* C++\n\n** Flags\n:PROPERTIES:\n:subcomponent:    api\n:END:\n\n")
    out.append(
        "** Repository\n:PROPERTIES:\n"
        f":entity_singular_short: {new_singular}\n"
        f":entity_plural_short:   {new_plural}\n"
        f":entity_singular_words: {singular} change\n"
        f":entity_plural_words:   {singular} changes\n:END:\n\n"
    )
    for name in ("Domain includes", "Entity includes"):
        for s in by_name["C++"].split("\n** ")[1:]:
            if s.startswith(name):
                out.append("** " + s.rstrip("\n") + "\n\n")
    return "".join(out)


def pending_path(source, text):
    front, _ = split_sections(text)
    component = keyword(front, "component")
    singular = keyword(front, "entity_singular")
    return source.parent / f"ores.{component}.{singular}_change.org"


def write_derived(source):
    """Write the derived model of a gated model and return its path.

    Returns None when the model is not gated.
    """
    source = Path(source)
    text = source.read_text()
    if not is_gated(text):
        return None
    target = pending_path(source, text)
    target.write_text(derive(source, text))
    return target


def shown(path):
    try:
        return path.resolve().relative_to(ROOT)
    except ValueError:
        return path


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--check", action="store_true")
    ap.add_argument("models", nargs="*")
    args = ap.parse_args()
    models = [Path(m) for m in args.models] or sorted(ROOT.glob("projects/*/modeling/ores.*.org"))
    stale = 0
    for source in models:
        text = source.read_text()
        if not is_gated(text):
            continue
        target = pending_path(source, text)
        derived = derive(source, text)
        if args.check:
            if not target.exists() or target.read_text() != derived:
                print(f"stale: {shown(target)}")
                stale += 1
        else:
            target.write_text(derived)
            print(f"wrote {shown(target)}")
    return 1 if stale else 0
