#!/usr/bin/env python3
"""Set the cybernetic level on every skill, and report the attenuated slices.

The level turns the cybernetic systems into a variety attenuator: a session
declares its System, and the level selects the slice of the catalogue in
scope by default. A regulator swamped by variety it cannot use stops
regulating, and an agent shown every skill at once is exactly that.

The level is a judgement about which System the skill serves, so it is a
reviewed table rather than a rule over names. LEVELS below is that table:
every skill appears, and the value is defensible from what the skill acts
on. A skill missing from it is an error, not a default.

Attenuation is advisory at S1, S2, S3, S4 and S5 and binding at S3*, the
audit channel, which sees read-only actions only. Read-only is derived from
the verb register rather than tagged, which is one of the reasons that
register exists: show- and find- promise to change nothing.

Usage: python3 build/scripts/apply_skill_levels.py [--check] [--slice LEVEL]
  --check        exit non-zero if any skill's level differs from the table.
  --slice LEVEL  print the slice a session at LEVEL sees, and stop.
  --prune-deployed LEVEL
                 remove the out-of-scope skills from the deployed tree, so
                 an agent spawned at LEVEL cannot load them. This makes the
                 attenuation binding, which is what a read-only System
                 wants: the audit channel is held read-only by what is on
                 disk when it starts, rather than by asking it in a prompt.
                 Normal operation deploys the full set and leaves every
                 advisory level free to reach outside its slice.
"""
import argparse
import pathlib
import re
import shutil
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
SKILLS = ROOT / "doc" / "llm" / "skills"
DEPLOYED = ROOT / ".claude" / "skills"
LEVELS_DOC = SKILLS / "skill_systems_levels.org"
BEGIN = "# BEGIN generated inventory (build/scripts/apply_skill_levels.py)"
END = "# END generated inventory"

READ_ONLY_VERBS = ("show", "find")
ORDER = ["s1", "s2", "s3", "s3star", "s4", "s5", "cross"]

# Skill -> level. The reason is the skill's target: what does it act on, and
# whose horizon is that?
LEVELS = {
    # Execution: code, generated code, environments, documents, pull
    # requests. The agent's own hours-long horizon.
    "compass-code-add-domain-type": "s1",
    "compass-code-add-tests": "s1",
    "compass-code-investigate-test-failure": "s1",
    "compass-code-review-comments": "s1",
    "compass-code-review-component": "s1",
    "compass-code-review-data-model": "s1",
    "compass-code-review-pr": "s1",
    "compass-code-run-build": "s1",
    "compass-code-run-feature-test": "s1",
    "compass-code-start-branch": "s1",
    "compass-codegen-add-component": "s1",
    "compass-codegen-add-entity": "s1",
    "compass-codegen-add-qt-entity": "s1",
    "compass-codegen-add-sql-schema": "s1",
    "compass-codegen-add-surface-entity": "s1",
    "compass-codegen-fix-drift": "s1",
    "compass-codegen-sync-cmake-sources": "s1",
    "compass-devops-deploy-manual": "s1",
    "compass-devops-deploy-settings": "s1",
    "compass-devops-deploy-site": "s1",
    "compass-devops-deploy-skills": "s1",
    "compass-devops-recreate-db": "s1",
    "compass-devops-run-client": "s1",
    "compass-devops-run-environment": "s1",
    "compass-devops-run-shell": "s1",
    "compass-devops-run-sql": "s1",
    "compass-devops-setup-environment": "s1",
    "compass-devops-start-services": "s1",
    "compass-devops-stop-services": "s1",
    "compass-devops-update-environment": "s1",
    "compass-doc-add-chart": "s1",
    "compass-doc-add-class-diagram": "s1",
    "compass-doc-add-component-model": "s1",
    "compass-doc-add-entity-chapter": "s1",
    "compass-doc-add-er-diagram": "s1",
    "compass-doc-add-recipe": "s1",
    "compass-doc-run-manual-screenshots": "s1",
    "compass-doc-sync-recipes": "s1",
    "compass-doc-update-manual": "s1",
    "compass-pr-address-review": "s1",
    "compass-pr-merge": "s1",
    "compass-pr-raise": "s1",
    "compass-pr-sync-branch": "s1",
    "compass-agile-close-task": "s1",
    "compass-agile-start-task": "s1",
    "compass-agile-start-hotfix": "s1",

    # Coordination: the story, and the worktree a story runs in. Days.
    "compass-agile-start-story": "s2",
    "compass-devops-provision-environment": "s2",
    "compass-devops-deprovision-environment": "s2",

    # Control: the sprint and the backlog it draws from. Weeks.
    "compass-agile-open-sprint": "s3",
    "compass-agile-plan-sprint": "s3",
    "compass-agile-refine-backlog": "s3",
    "compass-agile-review-sprint": "s3",
    "compass-agile-add-timeline-snapshot": "s3",
    "compass-agile-find-heading": "s3",

    # Development: the version and its release. Months.
    "compass-agile-add-release-notes": "s4",

    # Identity: what belongs in the product at all.
    "compass-agile-brainstorm-idea": "s5",

    # Methods. The level is the level of the work the method sequences:
    # execution for the four that carry a change, cross for one that only
    # reads, s2 for the one that plans work spanning many units.
    "compass-method-bug-fix": "s1",
    "compass-method-feature": "s1",
    "compass-method-refactoring": "s1",
    "compass-method-prototype": "s1",
    "compass-method-investigation": "cross",
    "compass-method-figure-it-out": "s2",

    # Bindings on an upstream principle. The level is the level of the
    # judgement the principle is cited at, not of the principle itself.
    "compass-principle-type-system-discipline": "s1",
    "compass-principle-never-block-on-the-human": "cross",

    # Every level acts on these: the documents and skills themselves, and
    # the orientation any System starts from.
    "compass-doc-add": "cross",
    "compass-doc-add-memory": "cross",
    "compass-doc-find": "cross",
    "compass-doc-review-academic": "cross",
    "compass-doc-review-ste100": "cross",
    "compass-doc-show": "cross",
    "compass-doc-sync-index": "cross",
    "compass-agile-find-bearings": "cross",
    "compass-skill-add": "cross",
    "compass-skill-delete": "cross",
    "compass-skill-review": "cross",
    "compass-skill-show-catalogue": "cross",
}

LEVEL_RE = re.compile(r"^#\+level:.*$", re.M)


def skill_dirs():
    return sorted(d for d in SKILLS.iterdir()
                  if d.is_dir() and (d / "SKILL.org").is_file())


def verb(name):
    """The verb segment of compass-<domain>-<verb>[-<object>]."""
    parts = name.split("-")
    return parts[2] if len(parts) > 2 else ""


def read_only(name):
    return verb(name) in READ_ONLY_VERBS


def in_scope(name, level):
    """What a session at this level sees by default.

    S3* is the binding case: the audit channel observes and does not mutate,
    so its slice is the read-only actions and nothing else. Every other
    level sees its own slice plus the cross-level skills, and may reach
    outside on request.
    """
    if level == "s3star":
        return read_only(name)
    declared = LEVELS.get(name)
    return declared in (level, "cross")


def current_level(path):
    m = re.search(r"^#\+level:\s*(\S+)\s*$", path.read_text(encoding="utf-8"), re.M)
    return m.group(1) if m else None


def set_level(path, level):
    text = path.read_text(encoding="utf-8")
    line = f"#+level: {level}"
    if LEVEL_RE.search(text):
        updated = LEVEL_RE.sub(line, text, count=1)
    else:
        # Sits with the other frontmatter, after the export-exclude line the
        # skill scaffold always carries.
        updated = text.replace("#+export_exclude_tags: noexport\n",
                               f"#+export_exclude_tags: noexport\n{line}\n", 1)
    if updated == text:
        return False
    path.write_text(updated, encoding="utf-8")
    return True


SYSTEMS = {
    "s1": ("Agent", "Hours"),
    "s2": ("Orchestrator", "Days"),
    "s3": ("Sprint Planner", "Weeks"),
    "s3star": ("Auditor", "Continuous"),
    "s4": ("Version Planner", "Months"),
    "s5": ("Identity Steward", "Indefinite"),
}


def inventory(names):
    """The slice each System actually sees, counted from the same table the
    skills are tagged from, so the document cannot drift from the catalogue."""
    lines = [BEGIN, "",
             "What each System sees by default, counted from the levels the "
             "skills carry. Attenuation is advisory everywhere except the "
             "audit channel, where it is binding and enforced by what is "
             "deployed.", "",
             "#+ATTR_HTML: :class hug-leading",
             "| Level | System | Horizon | Sees | Attenuation |",
             "|-------+--------+---------+------+-------------|"]
    for level, (system, horizon) in SYSTEMS.items():
        n = sum(1 for name in names if in_scope(name, level))
        strength = "binding" if level == "s3star" else "advisory"
        lines.append(f"| ={level}= | {system} | {horizon} | {n} skills | {strength} |")
    cross = sum(1 for name in names if LEVELS.get(name) == "cross")
    lines += ["",
              f"{cross} skills are tagged =cross= and appear in every "
              "advisory slice: the documents and the skills themselves, and "
              "the orientation any System starts from.", "",
              END]
    return "\n".join(lines)


def write_inventory(names):
    import re as _re
    text = LEVELS_DOC.read_text(encoding="utf-8")
    pattern = _re.escape(BEGIN) + r".*?" + _re.escape(END)
    old_pattern = (r"# BEGIN generated inventory \(build/scripts/"
                   r"generate_skills_catalogue\.py\).*?# END generated inventory")
    block = inventory(names)
    if _re.search(pattern, text, _re.S):
        updated = _re.sub(pattern, lambda _m: block, text, flags=_re.S)
    elif _re.search(old_pattern, text, _re.S):
        updated = _re.sub(old_pattern, lambda _m: block, text, flags=_re.S)
    else:
        print(f"❌ inventory markers not found in {LEVELS_DOC}", file=sys.stderr)
        return None
    return updated


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true",
                    help="exit non-zero if a level differs from the table")
    ap.add_argument("--slice", metavar="LEVEL", choices=ORDER,
                    help="print the slice a session at LEVEL sees")
    ap.add_argument("--prune-deployed", metavar="LEVEL", choices=ORDER,
                    help="remove out-of-scope skills from the deployed tree, "
                         "making the attenuation binding for whatever is "
                         "spawned next")
    args = ap.parse_args()

    names = [d.name for d in skill_dirs()]

    missing = [n for n in names if n not in LEVELS]
    unknown = [n for n in LEVELS if n not in names]
    if missing or unknown:
        for n in missing:
            print(f"❌ no level for {n}", file=sys.stderr)
        for n in unknown:
            print(f"❌ table names a skill that does not exist: {n}",
                  file=sys.stderr)
        return 1

    if args.prune_deployed:
        level = args.prune_deployed
        if not DEPLOYED.is_dir():
            print(f"❌ nothing deployed at {DEPLOYED}", file=sys.stderr)
            return 1
        removed = []
        for d in sorted(DEPLOYED.iterdir()):
            if not d.is_dir() or d.name not in LEVELS:
                continue
            if in_scope(d.name, level):
                continue
            shutil.rmtree(d)
            removed.append(d.name)
        kept = sum(1 for d in DEPLOYED.iterdir() if d.is_dir())
        print(f"🧭 deployed tree pruned to {level}: {kept} skill(s) remain, "
              f"{len(removed)} removed.")
        print("   Rebuild the bundle to restore the full set.")
        return 0

    if args.slice:
        visible = [n for n in names if in_scope(n, args.slice)]
        binding = " (binding)" if args.slice == "s3star" else " (advisory)"
        print(f"🧭 {args.slice}{binding}: {len(visible)} of {len(names)} skills\n")
        for n in visible:
            print(f"  {n}")
        return 0

    changed = []
    for d in skill_dirs():
        path = d / "SKILL.org"
        want = LEVELS[d.name]
        if current_level(path) == want:
            continue
        if args.check:
            print(f"❌ {d.name}: level is {current_level(path)}, table says {want}",
                  file=sys.stderr)
            changed.append(d.name)
            continue
        set_level(path, want)
        changed.append(d.name)

    if args.check:
        updated = write_inventory(names)
        if updated is None:
            return 1
        if updated != LEVELS_DOC.read_text(encoding="utf-8"):
            print(f"❌ stale inventory in {LEVELS_DOC.name}", file=sys.stderr)
            changed.append(LEVELS_DOC.name)
        if changed:
            print("   run: python3 build/scripts/apply_skill_levels.py",
                  file=sys.stderr)
            return 1
        print(f"✅ all {len(names)} skills carry their table level.")
        return 0

    updated = write_inventory(names)
    if updated is None:
        return 1
    if updated != LEVELS_DOC.read_text(encoding="utf-8"):
        LEVELS_DOC.write_text(updated, encoding="utf-8")
        print(f"✅ regenerated the inventory in {LEVELS_DOC.name}")

    print(f"✅ set the level on {len(changed)} skill(s); {len(names)} total.")
    for level in ORDER:
        n = sum(1 for name in names if in_scope(name, level))
        print(f"   {level:7} sees {n}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
