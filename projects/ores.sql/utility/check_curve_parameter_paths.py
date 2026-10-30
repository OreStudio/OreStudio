#!/usr/bin/env python3
"""Every curve parameter's path is one the ORE schema declares.

The contract keys a curve leaf by an XPath and every reader resolves that path
against the document. A path that no document can hold describes a leaf that
cannot be drawn and cannot be guarded: the store and the page both do nothing
with it, and neither says so.

The check walks each ``/CurveConfiguration/...`` path of the committed artefact
through ``curveconfig.xsd`` from the root element, and fails where a parent does
not declare the child. A path may only continue through a complex type.

One path is real and not in the schema: ORE reads ``Interpolation`` of a yield
volatility, and its ``yieldVolatility`` type declares no such element. An
exemption names the ORE source line that reads it, so the reader can check the
claim rather than take an exemption on trust.

Usage: check_curve_parameter_paths.py [--artefact <file>] [--schema <file>]
"""

import argparse
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
ARTEFACT = os.path.join(
    ROOT, "projects", "ores.sql", "populate", "reporting",
    "reporting_ore_parameters_artefact_populate.sql")
SCHEMA = os.path.join(ROOT, "external", "ore", "xsd", "curveconfig.xsd")
EXEMPTIONS = os.path.join(HERE, "curve_schema_exemptions.json")

ROOT_ELEMENT = "CurveConfiguration"
PREFIX = "/" + ROOT_ELEMENT + "/"

XPATH = re.compile(r"'(/CurveConfiguration/[A-Za-z0-9_/]+)'")
COMPLEX = re.compile(
    r'<xs:complexType\s+name="(\w+)">(.*?)\n  </xs:complexType>', re.S)
INCLUDE = re.compile(r'<xs:include\s+schemaLocation="([^"]+)"')
DECLARED = re.compile(r'<xs:element\b([^>]*?)(/?)>')


def text_of(path):
    with open(path, encoding="utf-8") as handle:
        return handle.read()


def complex_types(path):
    """Every complex type the schema and its includes declare, by name.

    An included schema declares the simple types the curve schema refers to, such
    as a currency or a calendar; the walk only needs to know which names continue
    into children, so the complex types of every file are collected and anything
    else ends a path.
    """
    found = {}
    seen = set()
    pending = [path]
    while pending:
        current = pending.pop()
        if current in seen or not os.path.exists(current):
            continue
        seen.add(current)
        body = text_of(current)
        for match in COMPLEX.finditer(body):
            found[match.group(1)] = match.group(2)
        for include in INCLUDE.findall(body):
            pending.append(os.path.join(os.path.dirname(current), include))
    return found


def children_of(body):
    """The elements a complex type declares, as name -> type name or None."""
    children = {}
    for match in DECLARED.finditer(body):
        attributes = match.group(1)
        name = re.search(r'\bname="(\w+)"', attributes)
        if name is None:
            continue
        kind = re.search(r'\btype="([\w.-]+)"', attributes)
        children[name.group(1)] = kind.group(1) if kind is not None else None
    return children


def root_type(path):
    match = re.search(r'<xs:element\s+type="(\w+)"\s+name="' + ROOT_ELEMENT + r'"',
                      text_of(path))
    if match is None:
        raise SystemExit(f"{path} declares no {ROOT_ELEMENT} element")
    return match.group(1)


def paths(artefact):
    return sorted(set(XPATH.findall(text_of(artefact))))


def walked(types, start, xpath):
    """The segment of a path where a parent does not declare its child, or None."""
    segments = [segment for segment in xpath[len(PREFIX) - 1:].split("/") if segment]
    current = start
    for index, segment in enumerate(segments):
        body = types.get(current)
        if body is None:
            return "/".join(segments[:index]), current, segment
        children = children_of(body)
        if segment not in children:
            return "/".join(segments[:index]), current, segment
        if index == len(segments) - 1:
            return None
        kind = children[segment]
        if kind is None or kind not in types:
            return "/".join(segments[:index + 1]), kind or segment, segments[index + 1]
        current = kind
    return None


def exemptions():
    if not os.path.exists(EXEMPTIONS):
        return []
    with open(EXEMPTIONS, encoding="utf-8") as handle:
        return json.load(handle).get("rules", [])


def exempt(rules, segments):
    """Whether a rule states why this path is real though a type omits it.

    A rule names the parent path, with =*= for a segment that varies, and the
    elements the ORE code reads there. It carries the ORE source line, so a
    reader checks the claim instead of trusting the exemption.
    """
    for rule in rules:
        pattern = rule["path"]
        if len(segments) != len(pattern) + 1:
            continue
        if any(want != "*" and want != have for want, have in zip(pattern, segments)):
            continue
        if segments[-1] in rule.get("elements", []):
            return True
    return False


def used(rules, all_segments):
    """The rules at least one path of the artefact still needs.

    A rule no path needs is stale, and a stale exemption hides the next defect
    that lands under the path it names.
    """
    return [rule for rule in rules
            if any(exempt([rule], segments) for segments in all_segments)]


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--artefact", default=ARTEFACT)
    parser.add_argument("--schema", default=SCHEMA)
    args = parser.parse_args()

    types = complex_types(args.schema)
    start = root_type(args.schema)
    rules = exemptions()
    found = paths(args.artefact)
    segments_of = {xpath: [part for part in xpath[len(PREFIX) - 1:].split("/") if part]
                   for xpath in found}
    problems = []
    for xpath in found:
        failure = walked(types, start, xpath)
        if failure is None:
            continue
        if exempt(rules, segments_of[xpath]):
            continue
        parent, kind, child = failure
        problems.append(f"{xpath}: {parent or ROOT_ELEMENT} of type {kind}"
                        f" does not declare {child}")
    for rule in rules:
        if rule not in used(rules, segments_of.values()):
            problems.append(f"the exemption for {rule['path']} names no path of the"
                            " artefact; retire it")
    for problem in problems:
        print(problem)
    print(f"{len(found)} path(s) checked against the schema,"
          f" {len(rules)} exemption(s), {len(problems)} problem(s)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
