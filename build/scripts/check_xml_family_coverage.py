#!/usr/bin/env python3
"""Fail when an ORE document family is unclassified, or classified wrongly.

Every root element the example corpus carries is one family, and every family
is in exactly one of three states:

  supported  A generated table reads and writes it, and the round-trip harness
             proves it. This is the destination.
  migrating  Something in the product reads it: a mapper, a store, a service.
             The family moves to a table in its own migration task.
  unused     Nothing in the product reads it. Nothing is migrated, and no XML
             family suite may name it, because such a test would prove a
             capability no feature has. The family leaves with xsdcpp.

The three states are a decision, not a measurement, so the families are named
here with the evidence for the state. What this check measures is whether the
tree still agrees with the decision: a supported family must have a table, an
unused family must have no table and no XML suite that names it, and no root
element may be missing from the list. That is what keeps "seven families to
delete" from quietly becoming "seven families nobody got to".
"""

import os
import re
import subprocess
import sys

# family -> (state, evidence). The evidence says why the state is what it is,
# and it is what a later reader checks when the family's fate is questioned.
FAMILIES = {
    # Supported: a generated table binds the root element and the harness
    # proves every value survives the writing.
    "CalendarAdjustments": ("supported", "importer and exporter read it through the codec"),
    "Conventions": ("supported", "importer reads it"),
    "CounterpartyInformation": ("supported", "table written and proved; nothing reads it yet"),
    "CurrencyConfig": ("supported", "importer reads it"),
    "IborFallbackConfig": ("supported", "table written and proved; nothing reads it yet"),
    "NettingSetDefinitions": ("supported", "table written and proved; nothing reads the ORE XML"),
    "TodaysMarket": ("supported", "todays_market_mapper reads it"),
    # Migrating: product code reads the family, so a mapper still does the
    # work and a table is owed.
    "ORE": ("migrating", "run_store and the run configuration service read it"),
    "Portfolio": ("migrating", "importer and exporter read trades and their instruments"),
    "PricingEngines": ("migrating", "pricing_engine_mapper, reached from the run document"),
    "CurveConfiguration": ("migrating", "curve_configuration_mapper reads it"),
    "CreditSimulation": (
        "migrating",
        "the configuration type is seeded and stored; credit_simulation_mapper "
        "is its only XML reader, reached from the run configuration",
    ),
    "StressTesting": (
        "migrating",
        "the analytics stress test store is modelled on stresstest.xml and "
        "stress_test_mapper is its only importer",
    ),
    # Unused: nothing in the product reads the family. Its generated types
    # leave when xsdcpp does, and no XML family suite may name it.
    "Simulation": ("unused", "no product reader"),
    "SensitivityAnalysis": ("unused", "no product reader"),
    "ReferenceData": ("unused", "no product reader"),
    "ScriptLibrary": ("unused", "no product reader"),
    "CollateralBalances": ("unused", "no product reader"),
    "SIMMCalibrationData": ("unused", "no product reader"),
    "CrossAssetModel": ("unused", "no product reader"),
}

CORPUS = "external/ore/examples"
GENERATED = "projects/ores.ore/core/src/xml/generated"

# The suites that test a document family. A file elsewhere may carry an ORE
# document as a fixture -- the scanner's own tests do -- and that says nothing
# about a family being supported, so the scan is limited to these.
XML_SUITE = re.compile(r"(^|/)xml_\w+_tests\.cpp$")


def corpus_roots():
    """The root element of every document in the example corpus."""
    roots = set()
    for base, _, files in os.walk(CORPUS):
        for name in files:
            if not name.endswith(".xml"):
                continue
            with open(os.path.join(base, name), encoding="utf-8", errors="replace") as handle:
                text = handle.read()
            # A comment or a processing instruction may name something that
            # looks like an element, so they go before the search.
            text = re.sub(r"<!--.*?-->", "", text, flags=re.S)
            text = re.sub(r"<\?.*?\?>", "", text, flags=re.S)
            found = re.search(r"<([A-Za-z_][\w.:-]*)", text)
            if found:
                roots.add(found.group(1))
    return roots


def generated_roots():
    """The families a generated table binds."""
    roots = set()
    pattern = re.compile(r'document_type doc\{"([A-Za-z_][\w.:-]*)"')
    for name in os.listdir(GENERATED):
        if not name.endswith(".cpp"):
            continue
        with open(os.path.join(GENERATED, name), encoding="utf-8") as handle:
            roots.update(pattern.findall(handle.read()))
    return roots


def xml_suites():
    """Every XML family suite in every component, and what it says."""
    texts = {}
    for entry in os.listdir("projects"):
        component = os.path.join("projects", entry)
        if not os.path.isdir(component):
            continue
        for base, _, files in os.walk(component):
            if "tests" not in base.split(os.sep):
                continue
            for name in files:
                path = os.path.join(base, name)
                if not XML_SUITE.search(path):
                    continue
                with open(path, encoding="utf-8") as handle:
                    texts[path] = handle.read()
    return texts


def classify(roots, tabled, suites):
    """Every way the tree can disagree with FAMILIES."""
    problems = []
    for name in sorted(roots - set(FAMILIES)):
        problems.append(
            f"{name} is a corpus root element that nobody classified: add it to "
            f"FAMILIES in {os.path.basename(__file__)}"
        )
    for name in sorted(set(FAMILIES) - roots):
        problems.append(f"{name} is classified but the corpus carries no document with that root")

    for name, (state, _evidence) in sorted(FAMILIES.items()):
        if state == "supported" and name not in tabled:
            problems.append(f"{name} is called supported but no generated table binds it")
        if state == "migrating" and name in tabled:
            problems.append(f"{name} is called migrating but a table already binds it")
        if state == "unused" and name in tabled:
            problems.append(f"{name} is called unused but a table binds it")

    for path, text in sorted(suites.items()):
        for name, (state, _evidence) in sorted(FAMILIES.items()):
            if state == "unused" and re.search(r"\b" + re.escape(name) + r"\b", text):
                problems.append(
                    f"{path} names {name}, which is called unused: a test would prove "
                    f"a capability no feature has"
                )
    return problems


def main():
    root = subprocess.run(
        ("git", "rev-parse", "--show-toplevel"), capture_output=True, text=True, check=False
    ).stdout.strip()
    if not root:
        print("not a git checkout; nothing to check")
        return 0
    os.chdir(root)

    roots = corpus_roots()
    problems = classify(roots, generated_roots(), xml_suites())

    if problems:
        print("The XML family census and the tree disagree:")
        for problem in problems:
            print(f"  - {problem}")
        return 1

    states = [state for state, _evidence in FAMILIES.values()]
    print(
        f"the census holds: {states.count('supported')} supported, "
        f"{states.count('migrating')} migrating, {states.count('unused')} unused, "
        f"over {len(roots)} corpus root elements"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
