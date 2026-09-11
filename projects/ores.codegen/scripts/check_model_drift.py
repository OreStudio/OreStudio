#!/usr/bin/env python3
"""
Report entities whose declared features contradict the profile they bind to.

A model binds to a profile (``:profile: uuid-identified-lookup``) rather
than authoring each feature individually, which is a promise that the
entity has that profile's exact feature combination. The promise is not
checked anywhere: a model can name a profile and then declare a feature
the profile fixes to the opposite value, and codegen will honour the
model's own value silently.

This is the third kind of codegen drift. Template drift is guarded by
template-drift.yml and artefact drift by codegen-drift.yml; this guards
the model against the profile it claims to instantiate.

An assignment whose value begins with ``required`` fixes that the
feature must be set rather than which value it takes — a parent entity
name is necessarily specific to the entity adopting the profile. Those
are checked for presence only.

Only features the model states itself are compared. A feature the model
leaves unstated is supplied by the profile and cannot contradict it, so
silence is never drift.

A model may bind to several profiles at once (``:profile: simple-lookup,
artefact-staging-only``), in which case every one of them is checked. Two
profiles that fix the same feature to different values are themselves
reported, because the binding cannot be satisfied either way.

Usage:
  check_model_drift.py            report drift and exit non-zero on any
  check_model_drift.py --summary  also print the binding counts
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROFILE_DIR = REPO_ROOT / "projects" / "modeling"

# Bindings known to contradict their profile, kept as named exceptions so
# the check can enforce everything else today rather than waiting for
# these to be settled. This mirrors the registry in
# check_component_drift.py: a gate that reports a standing failure is a
# gate people learn to ignore. An entry earns its place only with the
# reason it cannot simply be corrected, and removing entries is the
# point of the list.
#
# report_definition is uuid-keyed AND workspace-scoped.
# uuid-identified-lookup fixes has_workspace_id=false and
# workspace-scoped-lookup fixes has_uuid_primary_key=false, so no
# profile in the catalogue describes it. Re-binding would move the false
# promise rather than remove it; the combination needs either a profile
# of its own or a decision that it should not exist.
#
# result states has_change_reason_cache=false and documents it as
# "overriding the profile default". The profile does not offer a
# default: its Assignments section says it fixes the value. The two
# disagree about what binding means, which is a larger question than a
# binding correction.
#
# report_instance binds fk-scoped-child and states no
# parent_entity_singular, nor any parent foreign key. Either it is not
# a child entity or the binding is incomplete.
KNOWN_MODEL_DRIFT = {
    ("projects/ores.reporting/modeling/ores.reporting.report_definition.org",
     "has_workspace_id"),
    ("projects/ores.compute/modeling/ores.compute.result.org",
     "has_change_reason_cache"),
    ("projects/ores.reporting/modeling/ores.reporting.report_instance.org",
     "parent_entity_singular"),
}

# "| [[id:UUID][has_tenant_id]] | true |" and the plain "| feature | value |"
# both appear in profile assignment tables.
ASSIGNMENT_RE = re.compile(
    r"^\|\s*(?:\[\[id:[^\]]*\]\[)?([a-z_]+)\]?\]?\s*\|\s*([^|]+?)\s*\|\s*$"
)
PROPERTY_RE = re.compile(r"^:([a-z_]+):\s*(.+?)\s*$")


def profile_slug(path: Path) -> str:
    """variability_uuid_identified_lookup.org -> uuid-identified-lookup."""
    return path.stem.removeprefix("variability_").replace("_", "-")


def load_profiles() -> dict:
    """Map each profile slug to the feature values it fixes."""
    profiles = {}
    for path in sorted(PROFILE_DIR.glob("variability_*.org")):
        text = path.read_text(encoding="utf-8")
        if "#+type: profile" not in text:
            continue
        assignments, in_section = {}, False
        for line in text.splitlines():
            if line.startswith("* "):
                in_section = line.strip() == "* Assignments"
                continue
            if not in_section:
                continue
            m = ASSIGNMENT_RE.match(line)
            if not m:
                continue
            feature, value = m.group(1), m.group(2).strip()
            # Skip the header row and the table rule.
            if feature in ("Feature", "feature") or set(value) <= {"-", "+"}:
                continue
            assignments[feature] = value
        profiles[profile_slug(path)] = assignments
    return profiles


def model_flags(path: Path) -> tuple:
    """Read every property drawer in the model, keyed by feature name.

    A model states its features across several drawers rather than one.
    The entity-level ones sit in ``* Flags``, but the Qt tier states
    ``has_uuid_primary_key``, ``has_pagination`` and
    ``has_change_reason_cache`` in the nested ``** Qt`` drawer under
    ``* C++``. Reading only ``* Flags`` made the three features the
    profiles most often fix invisible, so the check passed by not
    looking. Every drawer is read instead, and the heading path is kept
    so a disagreement between two of them can be reported rather than
    silently resolved by ordering.
    """
    values, headings = {}, []
    stack, in_drawer = [], False
    for line in path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if stripped.startswith("*") and " " in stripped:
            stars = len(stripped) - len(stripped.lstrip("*"))
            title = stripped[stars:].strip()
            del stack[stars - 1:]
            stack.append(title)
            in_drawer = False
            continue
        if stripped == ":PROPERTIES:":
            in_drawer = True
            continue
        if stripped == ":END:":
            in_drawer = False
            continue
        if not in_drawer:
            continue
        m = PROPERTY_RE.match(stripped)
        if not m:
            continue
        key, value = m.group(1), m.group(2).strip()
        where = " / ".join(stack) or "(top)"
        values.setdefault(key, []).append((where, value))
        headings.append(key)
    return values, headings


def stated(values: dict, feature: str):
    """The value a model states for a feature, or None if it states none.

    Returns the disagreement instead when the model states the same
    feature twice with different values: that is a defect in the model
    regardless of what any profile says.
    """
    entries = values.get(feature)
    if not entries:
        return None, None
    distinct = {v for _, v in entries}
    if len(distinct) > 1:
        return None, entries
    return entries[0][1], None


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--summary", action="store_true",
                    help="print binding counts alongside any drift")
    args = ap.parse_args()

    profiles = load_profiles()
    if not profiles:
        print(f"No profiles found under {PROFILE_DIR}", file=sys.stderr)
        return 2

    models = sorted(REPO_ROOT.glob("projects/*/modeling/*.org"))
    bound, unknown, drifted, conflicts, excepted, internal, missing = (
        0, [], [], [], [], [], [])

    for path in models:
        values, _ = model_flags(path)
        name, _ = stated(values, "profile")
        if not name:
            continue
        bound += 1
        rel = path.relative_to(REPO_ROOT)
        names = [n.strip() for n in name.split(",") if n.strip()]
        fixed_by = {}
        for one in names:
            if one not in profiles:
                unknown.append((rel, one))
                continue
            for feature, expected in profiles[one].items():
                if feature in fixed_by and fixed_by[feature][1] != expected:
                    conflicts.append(
                        (rel, fixed_by[feature][0], one, feature,
                         fixed_by[feature][1], expected))
                    continue
                fixed_by[feature] = (one, expected)
                actual, disagreement = stated(values, feature)
                if disagreement:
                    internal.append((rel, feature, disagreement))
                    continue
                if expected.startswith("required"):
                    if not actual:
                        if (str(rel), feature) in KNOWN_MODEL_DRIFT:
                            excepted.append((rel, feature))
                        else:
                            missing.append((rel, one, feature))
                    continue
                if actual is not None and actual != expected:
                    if (str(rel), feature) in KNOWN_MODEL_DRIFT:
                        excepted.append((rel, feature))
                        continue
                    drifted.append((rel, one, feature, expected, actual))

    for rel, name in unknown:
        print(f"{rel}: binds to unknown profile '{name}'")
    for rel, name, feature, expected, actual in drifted:
        print(f"{rel}: profile '{name}' fixes {feature}={expected}, "
              f"model declares {actual}")
    for rel, first, second, feature, one, other in conflicts:
        print(f"{rel}: profiles '{first}' and '{second}' disagree on "
              f"{feature} ({one} vs {other})")

    for rel, name, feature in missing:
        print(f"{rel}: profile '{name}' requires {feature} to be set, "
              f"model states none")
    for rel, feature, entries in internal:
        where = "; ".join(f"{w}={v}" for w, v in entries)
        print(f"{rel}: states {feature} twice with different values ({where})")
    for rel, feature in excepted:
        print(f"{rel}: {feature} contradicts its profile, allowed by "
              f"KNOWN_MODEL_DRIFT")

    if args.summary:
        print(f"\n{bound} of {len(models)} models bind to a profile; "
              f"{len(profiles)} profiles defined.")

    if unknown or drifted or conflicts or internal or missing:
        print(f"\nModel drift: {len(drifted)} contradicted assignment(s), "
              f"{len(conflicts)} profile conflict(s), {len(internal)} "
              f"self-contradicting model(s), {len(missing)} unset required "
              f"feature(s), {len(unknown)} unknown profile(s). A model that names a profile must not redeclare "
              f"a feature the profile fixes to another value.")
        return 1

    print(f"No model drift: every bound model agrees with its profile "
          f"({len(excepted)} known exception(s)).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
