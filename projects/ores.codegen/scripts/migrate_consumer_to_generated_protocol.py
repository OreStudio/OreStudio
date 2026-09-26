#!/usr/bin/env python3
"""Move a hand-written consumer onto the generated entity protocol.

ores.trading's models generate the canonical entity protocol now, and
the hand-written consumers still speak the consolidated one. The two
differ in three mechanical ways and one structural one:

  save_<x>_request           -> put_<x>_request
  get_<x>s_request           -> list_<x>s_request
  <var>.data.<field>         -> <var>.change.write.<field>
  (!resp || !resp->success)  -> (!resp || resp->result.outcome != ok)

The structural one is a whole-object assignment, ``<var>.data = obj``.
It expands to one assignment per member of the generated ``<x>_write``,
and each member is read from the domain object at the path its field
group puts it under: a member the identity group carries is
``obj.identity.<field>``, an audit member is ``obj.audit.<field>``, and
the rest are flat. The group members and their fields are read from the
generated domain headers, so the expansion follows the generator rather
than a hand-written table.

Read-only unless --write is passed. Run it from the repository root.
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]
API = REPO / "projects/ores.trading/api/include/ores.trading.api"
MSG = API / "messaging"
DOMAIN_DIRS = [API / "domain",
               REPO / "projects/ores.dq/api/include/ores.dq.api/domain"]

OUTCOME_OK = "ores::utility::domain::outcome::ok"


def new_request_name(old: str) -> str | None:
    if old.startswith("save_") and old.endswith("_request"):
        return "put_" + old[len("save_"):]
    m = re.fullmatch(r"get_([a-z0-9_]+?)s_request", old)
    if m:
        return f"list_{m.group(1)}s_request"
    return None


def struct_member_pairs(text: str, name: str) -> list[tuple[str, str]]:
    """(type, member) for each data member of a struct, in order."""
    m = re.search(r"struct " + re.escape(name) + r"\b[^{]*\{(.*?)\n\};", text, re.S)
    if not m:
        return []
    body = re.sub(r"/\*.*?\*/", "", m.group(1), flags=re.S)
    body = re.sub(r"//[^\n]*", "", body)
    pairs: list[tuple[str, str]] = []
    for line in body.splitlines():
        line = line.strip()
        if (not line or line.startswith(("using", "static", "template", "friend"))
                or line.endswith(("{", "}"))):
            continue
        mm = re.match(r"([A-Za-z_:<>,\s*&]+?)\s+([a-z_][a-z0-9_]*)\s*(=[^;]*)?;", line)
        if mm:
            pairs.append((mm.group(1).strip(), mm.group(2)))
    return pairs


def struct_members(text: str, name: str) -> list[str]:
    m = re.search(r"struct " + re.escape(name) + r"\b[^{]*\{(.*?)\n\};", text, re.S)
    if not m:
        return []
    body = re.sub(r"/\*.*?\*/", "", m.group(1), flags=re.S)
    body = re.sub(r"//[^\n]*", "", body)
    names: list[str] = []
    for line in body.splitlines():
        line = line.strip()
        if (not line or line.startswith(("using", "static", "template", "friend"))
                or line.endswith(("{", "}"))):
            continue
        mm = re.match(r"[A-Za-z_:<>,\s*&]+?\s+([a-z_][a-z0-9_]*)\s*(=[^;]*)?;", line)
        if mm:
            names.append(mm.group(1))
    return names


_group_cache: dict[str, list[str]] = {}


def group_fields(type_name: str) -> list[str]:
    bare = type_name.split("::")[-1].strip()
    if bare in _group_cache:
        return _group_cache[bare]
    fields: list[str] = []
    for directory in DOMAIN_DIRS:
        candidate = directory / f"{bare}.hpp"
        if candidate.exists():
            fields = struct_members(candidate.read_text(encoding="utf-8"), bare)
            break
    _group_cache[bare] = fields
    return fields


def optional_fields(entity: str) -> set[str]:
    """The entity's domain members that the group declares optional."""
    header = API / "domain" / f"{entity}.hpp"
    if not header.exists():
        return set()
    optional: set[str] = set()
    for type_name, _ in struct_member_pairs(
            header.read_text(encoding="utf-8"), entity):
        bare = type_name.split("::")[-1]
        for directory in DOMAIN_DIRS:
            candidate = directory / f"{bare}.hpp"
            if not candidate.exists():
                continue
            for field_type, field_name in struct_member_pairs(
                    candidate.read_text(encoding="utf-8"), bare):
                if "optional<" in field_type:
                    optional.add(field_name)
            break
    return optional


def domain_types(entity: str) -> dict[str, str]:
    """The domain member's own type, per field, from the group headers."""
    header = API / "domain" / f"{entity}.hpp"
    if not header.exists():
        return {}
    types: dict[str, str] = {}
    for type_name, member in struct_member_pairs(
            header.read_text(encoding="utf-8"), entity):
        bare = type_name.split("::")[-1]
        fields = struct_member_pairs(
            next((d / f"{bare}.hpp" for d in DOMAIN_DIRS
                  if (d / f"{bare}.hpp").exists()), header).read_text(
                      encoding="utf-8"), bare) \
            if group_fields(bare) else []
        for field_type, field_name in fields:
            types[field_name] = field_type
        if not fields:
            types.setdefault(member, type_name)
    return types


def domain_paths(entity: str) -> dict[str, str]:
    """Field name -> the path it is read at on the domain object.

    A member whose type is a field group carries that group's fields, at
    the member's own name; every other member is flat and reads as
    itself. The type is what identifies a group, which is why the members
    are read as pairs rather than as names.
    """
    header = API / "domain" / f"{entity}.hpp"
    if not header.exists():
        return {}
    paths: dict[str, str] = {}
    for type_name, member in struct_member_pairs(
            header.read_text(encoding="utf-8"), entity):
        fields = group_fields(type_name)
        if fields:
            for field in fields:
                paths[field] = f"{member}.{field}"
        else:
            paths.setdefault(member, member)
    return paths


def generated_requests() -> set[str]:
    """Every request name the generated protocol headers declare."""
    names: set[str] = set()
    for header in MSG.glob("*_protocol.hpp"):
        names.update(re.findall(r"struct ([a-z0-9_]+_request)\b",
                                header.read_text(encoding="utf-8", errors="replace")))
    return names


_request_entity: dict[str, str] = {}


def request_entity(request: str) -> str:
    """The entity a generated request belongs to.

    Read from the header that declares it rather than derived from the
    name: put_many_trades_request names the plural, so stripping the verb
    yields "trades" and no write record answers to it. Each entity's
    protocol header declares exactly one write record, and its stem is
    the entity.
    """
    if not _request_entity:
        for header in MSG.glob("*_protocol.hpp"):
            text = header.read_text(encoding="utf-8", errors="replace")
            writes = re.findall(r"struct ([a-z0-9_]+)_write\b", text)
            if len(writes) != 1:
                continue
            for name in re.findall(r"struct ([a-z0-9_]+_request)\b", text):
                _request_entity[name] = writes[0]
    return _request_entity.get(request, "")


def is_generated(entity: str, generated: set[str]) -> bool:
    """Whether the entity's own protocol is generated rather than consolidated."""
    return bool({f"put_{entity}_request", f"list_{entity}s_request",
                 f"list_{entity}_request", f"delete_{entity}_request"}
                & generated)


def write_members(entity: str) -> list[str]:
    header = MSG / f"{entity}_protocol.hpp"
    if not header.exists():
        return []
    return struct_members(header.read_text(encoding="utf-8"), f"{entity}_write")


DECL = re.compile(r"\b(\w+)_request(?:&|\s+)+(\w+)\s*[;{=,)]")
DATA_FIELD = re.compile(r"\b(\w+)\.data\.")
DATA_OBJECT = re.compile(r"\b(\w+)\.data\s*=\s*([^;]+);")
SUCCESS = re.compile(r"(!?)(\w+)->success\b")
MESSAGE = re.compile(r"\b(\w+)->message\b")


def migrate(text: str, path: Path, report: list[str]) -> str:
    """One linear pass, so a variable name reused with two types is right.

    A consumer declares the same variable name for several request types
    in different functions, so the type in force is the last declaration
    seen, not the first one in the file. A family with no model keeps the
    consolidated request it has today, and its variables take no rewrite.
    """
    generated = generated_requests()
    out: list[str] = []
    var_entity: dict[str, str] = {}
    resp_entity: dict[str, str] = {}
    successes = 0
    renamed = 0
    expanded = 0
    fields = 0
    using_decl = re.compile(r"(using ores::trading::messaging::)(\w+_request)\b")
    for line in text.splitlines(keepends=True):
        # A using declaration names the type once for the whole function.
        for m in list(using_decl.finditer(line)):
            stem = new_request_name(m.group(2))
            if stem and stem in generated:
                line = line.replace(m.group(2), stem)
                renamed += 1
                break
        decl = DECL.search(line)
        if decl:
            stem = decl.group(1)
            old_name = stem + "_request"
            new_name = new_request_name(old_name)
            if new_name and new_name in generated:
                line = line.replace(old_name, new_name)
                renamed += 1
                stem = request_entity(new_name) or stem
                for verb in ("save_", "put_", "list_", "get_", "delete_"):
                    if stem.startswith(verb):
                        stem = stem[len(verb):]
                        break
            else:
                for verb in ("save_", "put_", "list_", "get_", "delete_"):
                    if stem.startswith(verb):
                        stem = stem[len(verb):]
                        break
            var_entity[decl.group(2)] = stem

        obj = DATA_OBJECT.search(line)
        if obj and var_entity.get(obj.group(1)) in {
                v for v in var_entity.values()}:
            var, expr = obj.group(1), obj.group(2).rstrip()
            entity = var_entity.get(var, "")
            members = write_members(entity) if entity else []
            if members and is_generated(entity, generated):
                paths = domain_paths(entity)
                indent = line[:len(line) - len(line.lstrip())]
                ref = expr if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_.]*", expr) \
                    else f"({expr})"
                optional = optional_fields(entity)
                # struct_member_pairs yields (type, name); the lookup is by
                # the wire member's name.
                wire = {name: type_name for type_name, name in struct_member_pairs(
                    (MSG / f"{entity}_protocol.hpp").read_text(encoding="utf-8"),
                    f"{entity}_write")}
                types = domain_types(entity)
                rendered = []
                for m in members:
                    field = paths.get(m, m).split(".")[-1]
                    value = f"{ref}.{paths.get(m, m)}"
                    wire_type = wire.get(m, "")
                    if "optional<" in wire_type:
                        pass
                    elif field in optional:
                        # An optional domain member reaches a non-optional
                        # wire member through its empty value.
                        value += '.value_or("")'
                    elif "domain::" in types.get(field, "") and "string" not in wire_type:
                        # The domain member is an enumeration and the wire
                        # member is text, so the value converts on the way
                        # out, as the generator converts it on the way in.
                        value = f"ores::trading::domain::to_string({value})"
                    rendered.append(f"{indent}{var}.change.write.{m} = {value};\n")
                lines = rendered
                out.extend(lines)
                expanded += 1
                continue

        call = re.search(r"auto\s+(\w+)\s*=\s*nats_call\([^,]+,\s*(\w+),", line)
        if call:
            resp_entity[call.group(1)] = var_entity.get(call.group(2), "")

        def success_sub(m: re.Match) -> str:
            nonlocal successes
            negated, var = m.group(1) == "!", m.group(2)
            entity = resp_entity.get(var, "")
            if entity and is_generated(entity, generated):
                successes += 1
                op = "!=" if negated else "=="
                return f"{var}->result.outcome {op} {OUTCOME_OK}"
            return m.group(0)

        line = SUCCESS.sub(success_sub, line)

        def message_sub(m: re.Match) -> str:
            var = m.group(1)
            entity = resp_entity.get(var, "")
            if entity and is_generated(entity, generated):
                return f"{var}->result.message"
            return m.group(0)

        line = MESSAGE.sub(message_sub, line)

        def field_sub(m: re.Match) -> str:
            nonlocal fields
            var = m.group(1)
            entity = var_entity.get(var, "")
            if entity and is_generated(entity, generated):
                fields += 1
                return f"{var}.change.write."
            return m.group(0)

        line = DATA_FIELD.sub(field_sub, line)
        out.append(line)

    text = "".join(out)
    report.append(f"{path}: {renamed} declaration(s) renamed, "
                  f"{fields} field access(es), {expanded} whole-object site(s), "
                  f"{successes} response check(s)")
    return text


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("files", nargs="+", type=Path)
    ap.add_argument("--write", action="store_true",
                    help="write the files; without it the run reports only")
    args = ap.parse_args()
    report: list[str] = []
    for path in args.files:
        text = path.read_text(encoding="utf-8")
        migrated = migrate(text, path, report)
        changed = migrated != text
        print(f"{'would migrate' if changed else 'unchanged'}: {path}")
        if changed and args.write:
            path.write_text(migrated, encoding="utf-8")
    for line in report:
        print("NOTE " + line)
    return 0


if __name__ == "__main__":
    sys.exit(main())
