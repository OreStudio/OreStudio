#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil; python-indent-offset: 4 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""Every ported route answers the shape the BFF answered.

The route inventory pairs each address the BFF answered with the gateway route
that replaced it. Each port proves the payload of its own route by hand, and
nothing compares the shape of every answer with the shape the BFF gave, so a
member the screen reads can be missing while every other check stays green: a
report's runs answered a bare array where the BFF answered {runs: [...]}, and
the screen read the missing member as no rows.

The BFF's shapes are recorded once, into answer_shapes.json beside this script:
a status and a shape tree per pair, with every value stripped. The check
replays the same requests against the gateway and compares the shape -- member
names, nesting and types -- and never a value.

A pair is compared or excused, never silently dropped. An address that names a
path parameter names no record to read and a write would change the data, so
both are recorded as skipped with their reason; a pair the corpus does not name
fails the check, so a route ported later must be captured or excused.

Usage:
  check_answer_shapes.py <repository root> --capture   record the BFF's shapes
  check_answer_shapes.py <repository root>             compare the gateway
"""
import argparse
import http.cookiejar
import json
import pathlib
import re
import sys
import urllib.error
import urllib.request

# The seeded Acme tenant every script signs in to. The same facts live in
# projects/ores.web/packages/harness/src/acme.ts and in
# projects/ores.compass/src/compass_open_as.py, and a change there changes here.
PRINCIPAL = "tenant_admin@acme_corporation"
PASSWORD = "Secure-Password-123"

# The gateway names every request that carries a session in this header, so the
# request cannot be one a page made on the person's behalf.
SESSION_HEADER = {"X-Ores-Request": "1"}

CORPUS_NAME = "answer_shapes.json"
EXCEPTIONS_NAME = "answer_shapes_exceptions.json"


def settings_of(repo: pathlib.Path) -> dict:
    """The ports and spellings the environment states."""
    values = {}
    for line in (repo / ".env").read_text().splitlines():
        line = line.removeprefix("export ").strip()
        if "=" in line and not line.startswith("#"):
            name, value = line.split("=", 1)
            values[name] = value.strip().strip('"')
    return values


def inventory_rows(text: str) -> list:
    """Every route the inventory lists: kind, area, method, path, server."""
    start = text.index("* The inventory")
    rows = []
    for line in text[start:].splitlines():
        if not line.startswith("| "):
            continue
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if len(cells) < 5:
            continue
        if cells[0] in ("Kind",) or set(cells[0]) <= set("-+"):
            continue
        path = cells[3].strip("=")
        if not path.startswith("/"):
            continue
        rows.append(
            {
                "kind": cells[0],
                "area": cells[1],
                "method": cells[2].lower(),
                "path": path,
                "server": cells[4],
            }
        )
    return rows


def normalised(path: str) -> str:
    """The address with each parameter written the same way, through the two spellings."""
    path = re.sub(r":[A-Za-z0-9_]+", "{}", path)
    return re.sub(r"\{[A-Za-z0-9_]+\}", "{}", path)


def pairs_of(rows: list) -> list:
    """Each address two servers answer, with both spellings of it.

    An address the same server states twice is one route: a generated unit and a
    hand-written route share an address on purpose, and the pair is the address.
    """
    answered = {}
    for row in rows:
        if row["server"] not in ("bff", "ores.http"):
            continue
        key = (row["method"], normalised(row["path"]))
        held = answered.setdefault(key, {})
        held.setdefault(row["server"], row["path"])
    pairs = []
    for (method, address), sides in sorted(answered.items()):
        if "bff" not in sides or "ores.http" not in sides:
            continue
        pairs.append({"method": method, "address": address, "bff": sides["bff"]})
    return pairs


def skip_reason(pair: dict) -> str | None:
    """Why a pair cannot be replayed, or nothing when it can be."""
    if "{}" in pair["address"]:
        return "the address names a path parameter, so no request names a record to read"
    if pair["method"] != "get":
        return "the route writes, so replaying it would change the data"
    return None


ID_LIKE = re.compile(r"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")


def shape_of(value):
    """The shape of a value: its member names, nesting and types, and nothing else.

    A record keyed by identifier — the coverage of each series, by series id — is
    one member named for what it holds, not a member per identifier: an
    identifier is a value, and a shape states none.
    """
    if isinstance(value, dict):
        members = {}
        for name in sorted(value):
            members["{}" if ID_LIKE.match(name) else name] = (
                union([members["{}"], shape_of(value[name])]) if
                "{}" in members and ID_LIKE.match(name) else shape_of(value[name]))
        return {"object": {name: members[name] for name in sorted(members)}}
    if isinstance(value, list):
        return {"array": union([shape_of(element) for element in value])}
    if isinstance(value, bool):
        return ["boolean"]
    if isinstance(value, (int, float)):
        return ["number"]
    if isinstance(value, str):
        return ["string"]
    if value is None:
        return ["null"]
    return ["unknown"]


def union(shapes: list):
    """One shape that covers every shape given.

    Nullability and an array with no elements are facts about the data and not
    about the answer, so neither changes the shape: a member that is null in one
    row of a page and a string in the next is a string, and an array no row filled
    states nothing about what an element is.
    """
    # An array no row filled states nothing, and it is not the same fact as a
    # member every row holds as null: the first says nothing about an element,
    # the second says the member is null.
    if not [shape for shape in shapes if shape is not None]:
        return None
    given = [shape for shape in shapes if shape is not None and shape != ["null"]]
    if not given:
        return ["null"]
    parts = []
    for shape in given:
        parts.extend(shape["any_of"] if isinstance(shape, dict) and "any_of" in shape else [shape])

    kinds = {
        "object" if isinstance(part, dict) and "object" in part else
        "array" if isinstance(part, dict) and "array" in part else
        "scalar"
        for part in parts
    }
    if len(kinds) > 1:
        return {"any_of": sorted({json.dumps(part, sort_keys=True): part for part in parts}.values(),
                                 key=lambda part: json.dumps(part, sort_keys=True))}
    if kinds == {"scalar"}:
        names = sorted({name for part in parts for name in part} - {"null"})
        return names or ["null"]
    if kinds == {"object"}:
        members = {}
        for part in parts:
            for name, held in part["object"].items():
                members[name] = union([members[name], held]) if name in members else held
        return {"object": {name: members[name] for name in sorted(members)}}
    return {"array": union([part["array"] for part in parts])}


def differences(expected, actual, path=""):
    """Every place the answer's shape departs from the recorded one."""
    if expected == actual:
        return []
    if isinstance(expected, dict) and isinstance(actual, dict):
        if "any_of" in expected or "any_of" in actual:
            if expected == actual:
                return []
            return [{"path": path or "(root)", "expected": expected, "actual": actual}]
        if "object" in expected and "object" in actual:
            out = []
            for name in sorted(set(expected["object"]) | set(actual["object"])):
                here = f"{path}.{name}" if path else name
                if name not in actual["object"]:
                    out.append({"path": here, "expected": expected["object"][name],
                                "actual": None})
                elif name not in expected["object"]:
                    out.append({"path": here, "expected": None,
                                "actual": actual["object"][name]})
                else:
                    out.extend(differences(expected["object"][name],
                                           actual["object"][name], here))
            return out
        if "array" in expected and "array" in actual:
            # An array no row filled states nothing about what an element is, so
            # the two sides agree on the array and on nothing below it.
            if expected["array"] is None or actual["array"] is None:
                return []
            return differences(expected["array"], actual["array"], path + "[]")
    return [{"path": path or "(root)", "expected": expected, "actual": actual}]


def excused_by(exceptions: list, address: str, difference: dict) -> bool:
    """Whether an exception states this difference and why it stays."""
    for exception in exceptions:
        if exception.get("address") != address:
            continue
        stated = exception.get("path")
        if stated is None:
            return True
        if difference["path"] == stated or difference["path"].startswith(stated + "."):
            return True
        if difference["path"].startswith(stated + "[]"):
            return True
    return False


def session_at(base: str, principal: str, password: str):
    """A callable that answers from a signed-in session on one server.

    The session routes require the two facts a form cannot forge: a header of
    their own and an Origin the deployment states. The origin here is the base
    itself, which is what a page served by this server states.
    """
    jar = http.cookiejar.CookieJar()
    opener = urllib.request.build_opener(urllib.request.HTTPCookieProcessor(jar))

    def call(path: str, body=None, method: str = "GET", timeout: float = 30.0):
        headers = dict(SESSION_HEADER)
        headers["Origin"] = base
        data = None
        if body is not None:
            data = json.dumps(body).encode()
            headers["Content-Type"] = "application/json"
        request = urllib.request.Request(
            base + path, data=data, headers=headers, method=method
        )
        try:
            with opener.open(request, timeout=timeout) as answer:
                return answer.status, answer.read()
        except urllib.error.HTTPError as failure:
            return failure.code, failure.read()

    status, body = call(
        "/api/session", {"username": principal, "password": password}, "POST"
    )
    if status != 200:
        raise RuntimeError(f"Signing in at {base} answered {status}: {body[:200]!r}")
    signed_in = json.loads(body)
    if signed_in.get("outcome") == "party-required":
        parties = signed_in.get("availableParties") or []
        chosen = signed_in.get("defaultPartyId") or (parties[0].get("id") if parties else None)
        status, body = call("/api/session/party", {"partyId": chosen}, "POST")
        if status != 200:
            raise RuntimeError(f"Choosing a party at {base} answered {status}: {body[:200]!r}")
    return call


def read_shape(call, pair: dict, timeout: float) -> dict:
    """The status and shape one server answers a pair with."""
    status, body = call(pair["address"], method=pair["method"].upper(), timeout=timeout)
    try:
        parsed = json.loads(body)
    except json.JSONDecodeError:
        return {
            "status": status,
            "shape": {"text": len(body) > 0},
            "not_json": True,
        }
    return {"status": status, "shape": shape_of(parsed)}


def capture(repo: pathlib.Path, corpus_path: pathlib.Path, base: str, timeout: float) -> int:
    """Record the BFF's answers, once, into the corpus."""
    pairs = pairs_of(inventory_rows((repo / INVENTORY).read_text()))
    call = session_at(base, PRINCIPAL, PASSWORD)
    # A capture keeps what an earlier one recorded when the BFF does not answer
    # this time: a server too slow to answer in the timeout says nothing about the
    # shape of its answer, and rewriting a recorded shape as a failure would take
    # the route out of the comparison for a reason that has nothing to do with it.
    held = {}
    if corpus_path.exists():
        held = json.loads(corpus_path.read_text()).get("routes", {})
    corpus = {}
    for pair in pairs:
        address = f"{pair['method'].upper()} {pair['address']}"
        reason = skip_reason(pair)
        if reason is not None:
            corpus[address] = {"skipped": reason}
            continue
        try:
            corpus[address] = read_shape(call, pair, timeout)
        except Exception as failure:  # a server that does not answer is a fact too
            if "shape" in held.get(address, {}):
                corpus[address] = held[address]
                print(f"  {address}: kept, the BFF did not answer in time")
                continue
            corpus[address] = {"failed": str(failure)[:200]}
        print(f"  {address}: {'skipped' if reason else 'recorded'}")
    # One route to a line: the corpus is read as a list of what each route
    # answered, so a change is a changed line rather than a rearranged tree.
    lines = [json.dumps(corpus[address], sort_keys=True) for address in sorted(corpus)]
    body = ",\n".join(f'    {json.dumps(address)}: {line}'
                      for address, line in zip(sorted(corpus), lines))
    corpus_path.write_text(
        '{\n  "captured_from": ' + json.dumps(base) + ',\n  "routes": {\n' + body +
        '\n  }\n}\n')
    print(f"wrote {corpus_path}")
    return 0


def check(repo: pathlib.Path, corpus_path: pathlib.Path, exceptions_path: pathlib.Path,
          base: str, timeout: float) -> int:
    """Compare the gateway's answers with the recorded shapes."""
    pairs = pairs_of(inventory_rows((repo / INVENTORY).read_text()))
    corpus = json.loads(corpus_path.read_text())["routes"]
    exceptions = []
    if exceptions_path.exists():
        exceptions = json.loads(exceptions_path.read_text()).get("explanations", [])
    call = session_at(base, PRINCIPAL, PASSWORD)

    unknown = []
    compared = 0
    skipped = 0
    excused = 0
    failures = []
    for pair in pairs:
        address = f"{pair['method'].upper()} {pair['address']}"
        recorded = corpus.get(address)
        if recorded is None:
            unknown.append(address)
            continue
        if "skipped" in recorded or "failed" in recorded:
            skipped += 1
            continue
        reason = skip_reason(pair)
        if reason is not None:
            unknown.append(f"{address} ({reason}, and the corpus compares it)")
            continue
        expected = recorded
        try:
            actual = read_shape(call, pair, timeout)
        except Exception as failure:
            failures.append({"address": address, "path": "(request)", "expected": "an answer",
                             "actual": str(failure)[:200]})
            continue
        compared += 1
        found = []
        if expected.get("status") != actual.get("status"):
            found.append({"path": "(status)", "expected": expected.get("status"),
                          "actual": actual.get("status")})
        found.extend(differences(expected.get("shape"), actual.get("shape")))
        for difference in found:
            if excused_by(exceptions, address, difference):
                excused += 1
                continue
            failures.append({"address": address, **difference})

    print(f"{len(pairs)} pairs; {compared} compared, {skipped} skipped, {excused} excused")
    for address in unknown:
        print(f"  the corpus does not name {address}: capture it or excuse it")
    for failure in failures:
        print(f"  {failure['address']} at {failure['path']}: "
              f"the BFF {failure['expected']!r}, the gateway {failure['actual']!r}")
    return 1 if unknown or failures else 0


INVENTORY = "doc/knowledge/architecture/http_route_inventory.org"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("repository", help="the repository root")
    parser.add_argument("--capture", action="store_true",
                        help="record the BFF's answers into the corpus")
    parser.add_argument("--bff", help="the BFF's base address, for a capture")
    parser.add_argument("--gateway", help="the gateway's base address, for a check")
    parser.add_argument("--timeout", type=float, default=30.0,
                        help="how long one request may wait")
    arguments = parser.parse_args()

    repo = pathlib.Path(arguments.repository).resolve()
    here = pathlib.Path(__file__).resolve().parent
    corpus_path = here / CORPUS_NAME
    exceptions_path = here / EXCEPTIONS_NAME
    settings = settings_of(repo)
    if arguments.capture:
        base = arguments.bff or f"http://127.0.0.1:{settings.get('ORES_BFF_PORT', '20402')}"
        return capture(repo, corpus_path, base, arguments.timeout)
    base = arguments.gateway or f"http://127.0.0.1:{settings.get('ORES_HTTP_PORT', '20401')}"
    return check(repo, corpus_path, exceptions_path, base, arguments.timeout)


if __name__ == "__main__":
    sys.exit(main())
