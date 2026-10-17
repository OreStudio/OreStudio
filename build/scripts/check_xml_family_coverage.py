#!/usr/bin/env python3
"""Fail when an ORE document family is unclassified, or classified wrongly.

Every root element the example corpus carries is one family, and every family
is in exactly one of three states:

  supported  A generated table reads and writes it, and the round-trip harness
             proves it. This is the destination.
  migrating  Product code imports it, so a mapper still does the work. The
             family moves to a table in its own migration task.
  unused     No product code reads it. Nothing is migrated, and nothing may
             test it, because the test would prove a capability no feature
             has. The family leaves with xsdcpp.

The three states are a decision, not a measurement, so the families are named
here rather than inferred. What this check measures is whether the tree still
agrees with the decision: a family called supported must have a table, a
family called unused must have no test that names it, and no root element may
be missing from the list. That is what keeps "eight families to delete" from
quietly becoming "eight families nobody got to".
"""

import os
import re
import subprocess
import sys

# The families with a generated table. The accessor's root element name is
# the family's name.
SUPPORTED = (
    "CalendarAdjustments",
    "Conventions",
    "CounterpartyInformation",
    "CurrencyConfig",
    "IborFallbackConfig",
    "NettingSetDefinitions",
    "TodaysMarket",
)

# The families product code still imports through a mapper. Each name is the
# mapper or entry point that reads it, which is the evidence that the work is
# owed rather than dropped.
MIGRATING = {
    "ORE": "run_store and the run configuration service read the run document",
    "Portfolio": "importer and exporter read trades and their instruments",
    "PricingEngines": "pricing_engine_mapper, reached from the run document",
    "CurveConfiguration": "curve_configuration_mapper, reached from the store",
    "CreditSimulation": "credit_simulation_mapper",
}

# The families no product code reads. Nothing migrates them and nothing may
# test them; their generated types leave when xsdcpp does.
UNUSED = (
    "Simulation",
    "SensitivityAnalysis",
    "ReferenceData",
    "ScriptLibrary",
    "CollateralBalances",
    "SIMMCalibrationData",
    "CrossAssetModel",
    "StressTesting",
)

CORPUS = "external/ore/examples"
GENERATED = "projects/ores.ore/core/src/xml/generated"
TESTS = "projects/ores.ore/core/tests"


def git(*args):
    return subprocess.run(
        ("git",) + args, capture_output=True, text=True, check=False
    ).stdout.strip()


def corpus_roots():
    """The root element of every document in the example corpus."""
    roots = set()
    for base, _, files in os.walk(CORPUS):
        for name in files:
            if not name.endswith(".xml"):
                continue
            with open(os.path.join(base, name), encoding="utf-8", errors="replace") as handle:
                text = handle.read(4096)
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


def test_files():
    """The test suite and what each file says."""
    texts = {}
    for name in os.listdir(TESTS):
        if not name.endswith(".cpp"):
            continue
        with open(os.path.join(TESTS, name), encoding="utf-8") as handle:
            texts[name] = handle.read()
    return texts


def main():
    root = git("rev-parse", "--show-toplevel")
    if not root:
        print("not a git checkout; nothing to check")
        return 0
    os.chdir(root)

    problems = []
    roots = corpus_roots()
    tabled = generated_roots()

    known = set(SUPPORTED) | set(MIGRATING) | set(UNUSED)
    for name in sorted(roots - known):
        problems.append(
            f"{name} is a corpus root element that nobody classified: add it to "
            f"SUPPORTED, MIGRATING or UNUSED in {os.path.basename(__file__)}"
        )
    for name in sorted(known - roots):
        problems.append(f"{name} is classified but the corpus carries no document with that root")

    for name in SUPPORTED:
        if name not in tabled:
            problems.append(f"{name} is called supported but no generated table binds it")

    for name in MIGRATING:
        if name in tabled:
            problems.append(f"{name} is called migrating but a table already binds it")

    for name in UNUSED:
        if name in tabled:
            problems.append(f"{name} is called unused but a table binds it")

    for name, text in sorted(test_files().items()):
        for family in UNUSED:
            if re.search(r"\\b" + family + r"\\b", text):
                problems.append(
                    f"{TESTS}/{name} names {family}, which is called unused: "
                    f"a test proves a capability no feature has"
                )

    if problems:
        print("The XML family census and the tree disagree:")
        for problem in problems:
            print(f"  - {problem}")
        return 1

    print(f"the census holds: {len(SUPPORTED)} supported, {len(MIGRATING)} migrating, "
          f"{len(UNUSED)} unused, over {len(roots)} corpus root elements")
    return 0


if __name__ == "__main__":
    sys.exit(main())
