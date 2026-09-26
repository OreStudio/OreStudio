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

# A response reached through a request's own response_type: the type is
# dependent, so the call site cannot name it, and the request it answers
# is canonical by the time this marker is set.
CANONICAL = "<canonical>"


def new_request_name(old: str) -> str | None:
    if old.startswith("save_") and old.endswith("_request"):
        return "put_" + old[len("save_"):]
    m = re.fullmatch(r"get_([a-z0-9_]+?)_history_request", old)
    if m:
        return f"list_{m.group(1)}_versions_request"
    m = re.fullmatch(r"get_([a-z0-9_]+?)s_request", old)
    if m:
        return f"list_{m.group(1)}s_request"
    return None


def renamed_type(name: str) -> str | None:
    """The generated name for a consolidated request or response type.

    The response follows its request: save_x_response becomes
    put_x_response, and get_x_history_response becomes
    list_x_versions_response.
    """
    suffix = "response" if name.endswith("_response") else "request"
    request = name[: -len(suffix)] + "request"
    new_request = new_request_name(request)
    if not new_request:
        return None
    return new_request[: -len("request")] + suffix


def struct_member_pairs(text: str, name: str) -> list[tuple[str, str]]:
    """(type, member) for each data member of a struct, in order."""
    m = re.search(r"struct " + re.escape(name) + r"\b[^{]*\{(.*?)\n\};", text, re.S)
    if not m:
        return []
    body = re.sub(r"/\*.*?\*/", "", m.group(1), flags=re.S)
    body = re.sub(r"//[^\n]*", "", body)
    pairs: list[tuple[str, str]] = []
    # A declaration may wrap onto the next line, so the members are read
    # between semicolons rather than line by line.
    for declaration in body.split(";"):
        declaration = " ".join(declaration.split())
        if not declaration or declaration.startswith(("using", "static", "template", "friend")):
            continue
        mm = re.match(r"([A-Za-z_:<>,\s*&]+?)\s+([a-z_][a-z0-9_]*)\s*(=[^;]*)?$", declaration)
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
    for declaration in body.split(";"):
        declaration = " ".join(declaration.split())
        if not declaration or declaration.startswith(("using", "static", "template", "friend")):
            continue
        mm = re.match(r"[A-Za-z_:<>,\s*&]+?\s+([a-z_][a-z0-9_]*)\s*(=[^;]*)?$", declaration)
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
            # A header may also carry a hand-written request beside the
            # entity's own, so a request belongs to the write record only
            # when its name carries the record's stem.
            stems = (writes[0], f"{writes[0]}s")
            for name in re.findall(r"struct ([a-z0-9_]+_request)\b", text):
                if any(stem in name for stem in stems):
                    _request_entity[name] = writes[0]
    return _request_entity.get(request, "")


CANONICAL_VERB = re.compile(r"^(?:put_|list_|get_|delete_)")


def canonical_request(name: str, generated: set[str]) -> bool:
    """Whether a name is one of the generated protocol's own requests."""
    return bool(CANONICAL_VERB.match(name)) and name in generated


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


def write_types(entity: str) -> dict[str, str]:
    """The wire type of each generated write member, keyed by member name."""
    header = MSG / f"{entity}_protocol.hpp"
    if not header.exists():
        return {}
    return {name: type_name for type_name, name in struct_member_pairs(
        header.read_text(encoding="utf-8"), f"{entity}_write")}


def write_value(entity: str, member: str, ref: str) -> str:
    """The expression that fills one generated write member from the domain object.

    Three shapes are read from the generated headers rather than from a
    hand-written table: a wire member that is optional takes the domain
    value as it stands; a domain member that is optional reaches a
    non-optional wire member through its empty value; and a domain member
    that names an enumeration reaches a text wire member through
    to_string.
    """
    member_path = domain_paths(entity).get(member, member)
    field = member_path.split(".")[-1]
    value = f"{ref}.{member_path}"
    wire_type = write_types(entity).get(member, "")
    if "optional<" in wire_type:
        pass
    elif field in optional_fields(entity):
        value += '.value_or("")'
    elif "domain::" in domain_types(entity).get(field, "") and "string" in wire_type:
        # The wire member is text and the to_string returns a view, which
        # a designated initializer cannot copy into a string.
        value = f"std::string(ores::trading::domain::to_string({value}))"
    return value


def response_entity(response: str) -> str:
    """The entity a generated response belongs to, read from its request."""
    name = response.split("::")[-1]
    if not name.endswith("_response"):
        return ""
    request = name[: -len("response")] + "request"
    if not CANONICAL_VERB.match(request):
        return ""
    return request_entity(request)


def is_consolidated(name: str, generated: set[str]) -> bool:
    """Whether a type name is one the generated protocol does not declare."""
    request = name[: -len("response")] + "request" if name.endswith("_response") else name
    return request not in generated


def key_member(entity: str) -> tuple[str, str] | None:
    """The single (type, member) of a generated key struct."""
    header = MSG / f"{entity}_protocol.hpp"
    if not header.exists():
        return None
    members = struct_member_pairs(header.read_text(encoding="utf-8"), f"{entity}_key")
    return members[0] if len(members) == 1 else None


def key_value(key_type: str, value: str) -> str:
    """The caller's text as the key member's own type reads it."""
    moved = re.fullmatch(r"std::move\((\w+)\)", value)
    if moved:
        value = moved.group(1)
    return f"boost::uuids::string_generator()({value})" if "uuid" in key_type else value


def removal_line(entity: str, value: str, indent: str) -> str | None:
    """The statement that names the row a generated delete removes.

    The old consolidated request took a list of ids. The canonical one
    takes a removal key, and the key's own type says whether the caller's
    text converts to a uuid or is the key as it stands.
    """
    member = key_member(entity)
    if not member:
        return None
    key_type, key_name = member
    return f"{indent}req.removal.key.{key_name} = {key_value(key_type, value)};\n"


def list_row_member(entity: str) -> str:
    """The member a generated list response carries its rows in."""
    header = MSG / f"{entity}_protocol.hpp"
    if not header.exists():
        return ""
    text = header.read_text(encoding="utf-8")
    for type_name, member in struct_member_pairs(text, f"list_{entity}s_response"):
        if "vector<" in type_name and member != "versions":
            return member
    return ""


def add_includes(text: str, entities: set[str], generated: set[str]) -> str:
    """Add the generated protocol header of every entity the file now uses.

    The consolidated header keeps only the families with no model, so a
    file that names none of them no longer includes it.
    """
    missing = sorted(
        f"{entity}_protocol.hpp" for entity in entities if entity
        if f'#include "ores.trading.api/messaging/{entity}_protocol.hpp"' not in text)
    consolidated = re.findall(
        r"\b((?:save_|get_)[a-z0-9_]+_(?:request|response))\b", text)
    if not missing and not consolidated:
        return text
    lines = text.splitlines(keepends=True)
    if missing:
        anchor = [i for i, line in enumerate(lines)
                  if line.startswith('#include "ores.trading.api/messaging/')]
        if not anchor:
            anchor = [i for i, line in enumerate(lines) if line.startswith("#include ")]
        if anchor:
            at = anchor[-1] + 1
            lines = lines[:at] + [
                f'#include "ores.trading.api/messaging/{name}"\n' for name in missing] + lines[at:]
    if not consolidated:
        lines = [line for line in lines
                 if line != '#include "ores.trading.api/messaging/instrument_protocol.hpp"\n']
    return "".join(lines)


DECL = re.compile(r"\b(\w+)_request\s*(?:&\s*)?(\w+)\s*[;{=,)]")
TYPE_NAME = re.compile(r"\b((?:save_|get_)[a-z0-9_]+_(?:request|response))\b")
DATA_OBJECT = re.compile(r"\b(\w+)\.data\s*=\s*([^;]+);")
INITIALIZER = re.compile(r"\b(\w+)_request\s*\{\s*\.data\s*=\s*(.+?)\s*\}")
FACTORY = re.compile(r"\b(\w+)_request::from\(([^()]*(?:\([^()]*\))?[^()]*)\)")
IDS = re.compile(r"(\w+)\.ids\s*=\s*\{\s*(.+?)\s*\}\s*;")
ASSIGN = re.compile(r"\b(\w+)\.([a-z0-9_]+)\s*=\s*([^;]+);")
AUTO = re.compile(r"\s*auto\s+(\w+)\s*=\s*")
DO_AUTH = re.compile(r"do_auth_request\s*<\s*([^>]+?)\s*>")
SUBJECT = re.compile(r'(\bout\s*,\s*\w+\s*,\s*)"([^"]*)"(\s*,\s*(\w+)\s*\))')
WRITE_ACCESS = re.compile(r"\b(\w+)\.(?:data|change\.write)\.([A-Za-z_][A-Za-z0-9_.]*)")
RESPONSE_MEMBER = re.compile(r"\b(\w+)->(total_available_count|history|instruments)\b")
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
    used: set[str] = set()
    successes = 0
    renamed = 0
    expanded = 0
    fields = 0
    pending_auto = ""

    def domain_ref(expr: str) -> str:
        """The domain object a request expression carries, as a member path."""
        moved = re.fullmatch(r"std::move\((\w+)\)", expr)
        ref = moved.group(1) if moved else expr
        return ref if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_.]*", ref) else f"({ref})"

    def type_sub(m: re.Match) -> str:
        nonlocal renamed
        name = m.group(1)
        new_name = renamed_type(name)
        if not new_name:
            return name
        request = new_name[: -len("response")] + "request" \
            if new_name.endswith("_response") else new_name
        if not canonical_request(request, generated):
            return name
        renamed += 1
        entity = response_entity(new_name) if name.endswith("_response") \
            else request_entity(new_name)
        if entity:
            used.add(entity)
        return new_name

    def initializer_sub(m: re.Match) -> str:
        """A designated initializer expands to a nested one, not to statements."""
        nonlocal expanded
        name = m.group(1) + "_request"
        new_name = new_request_name(name) or name
        entity = request_entity(new_name)
        if not canonical_request(new_name, generated) or not write_members(entity):
            return m.group(0)
        rendered = ", ".join(f".{member} = {write_value(entity, member, domain_ref(m.group(2)))}"
                             for member in write_members(entity))
        expanded += 1
        used.add(entity)
        return f"{new_name}{{.change = {{.write = {{{rendered}}}}}}}"

    for line in text.splitlines(keepends=True):
        line = TYPE_NAME.sub(type_sub, line)
        # A factory fills a request from a domain object, and the canonical
        # request carries that object's members as its write record.
        line = FACTORY.sub(initializer_sub, line)
        line = INITIALIZER.sub(initializer_sub, line)

        # A declaration names the entity its variable carries, so later
        # member accesses on that variable take the generated path. A
        # consolidated declaration clears the name instead: a variable is
        # declared once per branch, and the branch with no model must not
        # inherit the previous branch's entity.
        for decl in DECL.finditer(line):
            name = decl.group(1) + "_request"
            if canonical_request(name, generated):
                entity = request_entity(name)
                if entity:
                    var_entity[decl.group(2)] = entity
                    used.add(entity)
            else:
                var_entity.pop(decl.group(2), None)

        auto = AUTO.search(line)
        if auto:
            entity = next((request_entity(name) for name in re.findall(
                r"\b((?:put_|list_|get_|delete_)[a-z0-9_]+_request)\b", line)
                if request_entity(name)), "")
            if entity:
                var_entity[auto.group(1)] = entity
                used.add(entity)

        ids = IDS.search(line)
        if ids:
            entity = var_entity.get(ids.group(1), "")
            statement = removal_line(entity, ids.group(2), line[:len(line) - len(line.lstrip())]) \
                if is_generated(entity, generated) else None
            if statement:
                line = statement
                expanded += 1

        # The consolidated request named its row with id, or with the key
        # member's own name; the canonical one carries the entity's key.
        key_field = ASSIGN.search(line)
        if key_field:
            var, member = key_field.group(1), key_field.group(2)
            entity = var_entity.get(var, "")
            key = key_member(entity) if is_generated(entity, generated) else None
            if key and key[1] != "id" and member in ("id", key[1]):
                line = line.replace(
                    key_field.group(0),
                    f"{var}.key.{key[1]} = {key_value(key[0], key_field.group(3))};")
                expanded += 1

        obj = DATA_OBJECT.search(line)
        if obj and is_generated(var_entity.get(obj.group(1), ""), generated):
            var, entity = obj.group(1), var_entity[obj.group(1)]
            indent = line[:len(line) - len(line.lstrip())]
            out.extend(f"{indent}{var}.change.write.{member} = "
                       f"{write_value(entity, member, domain_ref(obj.group(2).rstrip()))};\n"
                       for member in write_members(entity))
            expanded += 1
            continue

        call = re.search(r"auto\s+(\w+)\s*=\s*nats_call\([^,]+,\s*(\w+),", line)
        if call:
            resp_entity[call.group(1)] = var_entity.get(call.group(2), "")

        auth = DO_AUTH.search(line)
        if auth:
            auto = AUTO.search(line)
            var = auto.group(1) if auto else pending_auto
            if var:
                resp_entity[var] = CANONICAL if "::response_type" in auth.group(1) \
                    else response_entity(auth.group(1))
        full_auto = AUTO.fullmatch(line.rstrip("\n"))
        pending_auto = full_auto.group(1) if full_auto else ""

        def generated_response(var: str) -> bool:
            entity = resp_entity.get(var, "")
            return entity == CANONICAL or is_generated(entity, generated)

        def success_sub(m: re.Match) -> str:
            nonlocal successes
            negated, var = m.group(1) == "!", m.group(2)
            if generated_response(var):
                successes += 1
                op = "!=" if negated else "=="
                return f"{var}->result.outcome {op} {OUTCOME_OK}"
            return m.group(0)

        line = SUCCESS.sub(success_sub, line)

        def message_sub(m: re.Match) -> str:
            var = m.group(1)
            if generated_response(var):
                return f"{var}->result.message"
            return m.group(0)

        line = MESSAGE.sub(message_sub, line)

        def subject_sub(m: re.Match) -> str:
            var = m.group(4)
            if is_generated(var_entity.get(var, ""), generated):
                return f"{m.group(1)}std::string({var}.nats_subject){m.group(3)}"
            return m.group(0)

        line = SUBJECT.sub(subject_sub, line)

        def member_sub(m: re.Match) -> str:
            """The generated list response names its rows and its count itself."""
            var, member = m.group(1), m.group(2)
            entity = resp_entity.get(var, "")
            if not generated_response(var):
                return m.group(0)
            if member == "total_available_count":
                return f"{var}->total"
            if member == "history":
                return f"{var}->versions"
            rows = list_row_member(entity) if entity != CANONICAL else ""
            return f"{var}->{rows}" if rows else m.group(0)

        line = RESPONSE_MEMBER.sub(member_sub, line)

        def field_sub(m: re.Match) -> str:
            """A member access takes the write member whose path it names.

            The consolidated request carried the domain object, so its
            members were reached through the field groups. The write
            record is flat where the group is implicit, so the group
            prefix drops on the way in.
            """
            nonlocal fields
            var, path = m.group(1), m.group(2)
            entity = var_entity.get(var, "")
            if not is_generated(entity, generated):
                return m.group(0)
            paths = domain_paths(entity)
            inverse = {known: member for member, known in paths.items()}
            member = inverse.get(path)
            if member is None:
                for known in sorted(inverse, key=len, reverse=True):
                    if path.startswith(known + "."):
                        member = inverse[known] + path[len(known):]
                        break
            if member is None:
                return m.group(0)
            if member != path:
                fields += 1
            return f"{var}.change.write.{member}"

        line = WRITE_ACCESS.sub(field_sub, line)
        out.append(line)

    text = add_includes("".join(out), used, generated)
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
