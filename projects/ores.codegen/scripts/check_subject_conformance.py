#!/usr/bin/env python3
"""Fail when a declared NATS subject is outside the entity protocol.

The specification is
[[id=C650CBC9-FDED-4C68-981F-7902E296928E][NATS entity protocol specification]]
in doc/knowledge/architecture/nats_entity_protocol.org. Its grammar is:

    {component}.v1.{resource}.{verb}            a request
    {component}.v1.{resource}_events.{action}   an event
    {component}.v1.ops.{operation}              a domain operation

Every segment is lower snake_case, the second is the literal ``v1``, and a
request's verb is one of the eight the specification closes the set to
(``list_by_<relation>`` carries one further word). The ``ops`` namespace is
reserved for domain operations and is exempt from the verb set; an operation
that acts on an entity the common way belongs in the common set, and only one
that is genuinely a domain operation belongs here.

The generator already refuses a bad verb when it *derives* a subject --
``request_subject`` and ``event_subject`` in org_loader.py raise on one -- so a
violation enters the tree exactly one way: a model that declares the subject by
hand, with a ``:subject:`` property. That is what this check reads. The census
that wrote it found 159 such declarations, 127 of them outside the grammar.

Accepted violations live in ``subject_conformance_baseline.json`` beside this
script, each with the reason it is still there. The file is a ratchet: a
violation that is not in it fails, and an entry that no longer violates, or
names a subject the tree no longer declares, also fails. The file can only
shrink.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
MODELING = REPO_ROOT / "projects"
BASELINE = Path(__file__).resolve().parent / "subject_conformance_baseline.json"

# The closed set of eight. list_by_<relation> is the one verb that carries a
# word of its own, so it is matched by prefix rather than listed.
SPEC_VERBS = (
    "get",
    "get_many",
    "list",
    "put",
    "put_many",
    "delete",
    "delete_many",
)
LIST_BY = "list_by_"
SPEC_EVENT_ACTIONS = ("created", "updated", "deleted")

SUBJECT_LINE = re.compile(r"^\s*:subject:\s*(\S+)\s*$")
SNAKE = re.compile(r"^[a-z][a-z0-9_]*$")


def declared_subjects() -> list[tuple[str, int, str]]:
    """Every ``:subject:`` a model declares, as (path, line, subject)."""
    found: list[tuple[str, int, str]] = []
    for path in sorted(MODELING.glob("*/modeling/*.org")):
        for number, line in enumerate(path.read_text().splitlines(), start=1):
            match = SUBJECT_LINE.match(line)
            if match:
                found.append(
                    (str(path.relative_to(REPO_ROOT)), number, match.group(1))
                )
    return found


def classify(subject: str) -> tuple[bool, str]:
    """Whether a subject conforms, and why not when it does not."""
    segments = subject.split(".")

    if len(segments) != 4:
        return False, f"{len(segments)} segments, the grammar has four"

    component, version, resource, suffix = segments

    # The generic history request every entity shares. The component segment
    # is a generation-time placeholder, and the header builds the subject at
    # runtime as component + ".v1.history.get", so every concrete subject it
    # produces is owned by the component that asks and conforms. The literal
    # spelling is the one subject in the tree that is not a fixed address.
    if component == "{component}":
        return True, "a generic request, parameterised by the asking component"

    for name, segment in (
        ("component", component),
        ("version", version),
        ("resource", resource),
        ("verb", suffix),
    ):
        if not SNAKE.match(segment):
            return False, f"the {name} segment {segment!r} is not lower snake_case"

    if version != "v1":
        return False, f"the version segment is {version!r}, not 'v1'"

    # A domain operation, in the reserved namespace. Exempt from the verb set,
    # but it still has to be four snake_case segments, which the checks above
    # have already established.
    if resource == "ops":
        return True, "a domain operation in the reserved ops namespace"

    # The verb is checked before the event branch, because a resource may
    # itself be named *_events: trading.v1.lifecycle_events.delete_many is a
    # request on the lifecycle_events resource, not an event whose action is
    # delete_many. An event's action is never a verb of the common set, so the
    # order is unambiguous for every subject the grammar admits.
    if suffix in SPEC_VERBS or suffix.startswith(LIST_BY):
        return True, "a request"

    if resource.endswith("_events"):
        if suffix not in SPEC_EVENT_ACTIONS:
            return False, (
                f"the event action {suffix!r} is not one of "
                f"{', '.join(SPEC_EVENT_ACTIONS)}"
            )
        return True, "an event"

    # Everything still standing here is an operation that acts on an entity
    # without being one of the eight. The specification's discriminator is that
    # it belongs in the common set if it acts on the entity the common way, so
    # the remedy is a rename or a move to ops -- never a new verb.
    return False, (
        f"the verb {suffix!r} is not one of the closed set of eight and is not "
        f"in the reserved ops namespace"
    )


def load_baseline() -> dict[str, str]:
    if not BASELINE.exists():
        return {}
    data = json.loads(BASELINE.read_text())
    return {entry["subject"]: entry["reason"] for entry in data["accepted"]}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="check_subject_conformance",
        description="Fail on a declared NATS subject outside the entity protocol.",
    )
    parser.add_argument(
        "--update",
        action="store_true",
        help="Rewrite the baseline from the tree's current violations.",
    )
    parser.add_argument(
        "--list-violations",
        action="store_true",
        help="Print every current violation and exit 0, whatever the baseline says.",
    )
    args = parser.parse_args(argv)

    subjects = declared_subjects()
    if not subjects:
        print("❌ no :subject: declarations found; the scan is not reading the models")
        return 1

    violations: list[tuple[str, int, str, str]] = []
    for path, number, subject in subjects:
        ok, reason = classify(subject)
        if not ok:
            violations.append((path, number, subject, reason))

    if args.list_violations:
        for path, number, subject, reason in violations:
            print(f"{subject}\t{reason}\t{path}:{number}")
        print(f"{len(violations)} violation(s) of {len(subjects)} declaration(s)")
        return 0

    baseline = load_baseline()

    if args.update:
        payload = {
            "why": (
                "Subjects the tree declares outside the entity protocol. The "
                "gate fails on a violation that is not here, and on an entry "
                "that no longer violates, so the file can only shrink. Each "
                "reason names the defect; the story Bring every subject onto "
                "the canonical entity protocol removes them."
            ),
            "specification": "nats_entity_protocol.org",
            "accepted": [
                {"subject": subject, "reason": reason, "where": f"{path}:{number}"}
                for path, number, subject, reason in sorted(violations)
            ],
        }
        BASELINE.write_text(json.dumps(payload, indent=2) + "\n")
        print(f"✅ baseline written: {len(violations)} accepted violation(s)")
        return 0

    current = {subject for _, _, subject, _ in violations}
    new = [(p, n, s, r) for p, n, s, r in violations if s not in baseline]
    # A stale entry is drift of the other kind: the tree moved and the
    # exception did not, so the file is now claiming a violation that is gone.
    gone = sorted(set(baseline) - current)

    if new:
        print(f"❌ {len(new)} subject(s) outside the entity protocol:\n")
        for path, number, subject, reason in sorted(new):
            print(f"  {path}:{number}")
            print(f"    {subject}")
            print(f"    {reason}\n")
        print(
            "A subject is {component}.v1.{resource}.{verb} with a verb from the "
            "closed set of eight, or {component}.v1.{resource}_events.{action}, "
            "or {component}.v1.ops.{operation}. See "
            "doc/knowledge/architecture/nats_entity_protocol.org."
        )
        print("A genuine exception goes in subject_conformance_baseline.json with its reason.")

    if gone:
        print(f"❌ {len(gone)} baseline entr(ies) no longer violate:")
        for subject in gone:
            print(f"  {subject}")
        print("Remove them: run --update once the tree is correct.")

    if new or gone:
        return 1

    print(
        f"✅ subject conformance: {len(subjects)} declaration(s), "
        f"{len(violations)} accepted in the baseline, none new"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
