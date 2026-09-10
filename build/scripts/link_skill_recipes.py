#!/usr/bin/env python3
"""Point every action skill at the recipes that own its commands.

Under the architecture the recipe holds the command, tested and runnable by
a human, and the skill holds the judgement: when to reach for it, what must
hold first, what to do when the guard rails refuse. A skill that restates a
recipe's command creates a second place for it to drift.

The mapping below is a judgement about which recipe owns which skill's
commands, so it is a reviewed table. Applying it is mechanical: each skill's
Recipes section gains the links it lacks, in the order given, without
disturbing links it already carries.

Usage: python3 build/scripts/link_skill_recipes.py [--check]
  --check  exit non-zero if a skill is missing one of its recipe links.
"""
import argparse
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
SKILLS = ROOT / "doc" / "llm" / "skills"
RECIPES = ROOT / "doc" / "recipes"

# Skill -> the recipes that own its commands, as (recipe path, why it is
# reached for). The path is resolved to its :ID: so a moved recipe fails
# loudly here rather than leaving a dead link in a skill.
OWNERS = {
    "compass-devops-provision-environment": [
        ("compass/how_do_i_provision_an_environment_with_compass",
         "creates the worktree, assigns ports, writes the .env"),
    ],
    "compass-devops-deprovision-environment": [
        ("compass/how_do_i_deprovision_an_environment_with_compass",
         "stops the services and removes the worktree"),
    ],
    "compass-devops-run-shell": [
        ("compass/how_do_i_run_the_ores_shell",
         "interactive and scripted invocations"),
    ],
    "compass-devops-run-sql": [
        ("sql/how_do_i_run_sql_with_compass",
         "credentials and connection come from the .env"),
    ],
    "compass-devops-recreate-db": [
        ("cmake/how_do_i_start_services",
         "the database rebuild and the service restart around it"),
    ],
    "compass-devops-start-services": [
        ("cmake/how_do_i_start_services", "starting the fleet and verifying it"),
    ],
    "compass-devops-stop-services": [
        ("compass/how_do_i_manage_the_environment_with_compass",
         "the environment lifecycle verbs"),
    ],
    "compass-devops-run-client": [
        ("compass/how-do-i-use-named-env-files",
         "launching the client per environment, with colour"),
    ],
    "compass-devops-deploy-manual": [
        ("cmake/how_do_i_build_the_system", "the deploy_manual target"),
    ],
    "compass-devops-deploy-site": [
        ("cmake/how_do_i_deploy_the_site", "building and publishing the site"),
    ],
    "compass-devops-deploy-settings": [
        ("cmake/how_do_i_deploy_the_settings",
         "tangling settings.json from its org source"),
    ],
    "compass-agile-close-task": [
        ("agile/how_do_i_work_a_task",
         "the state transitions and the story and sprint sync"),
    ],
    "compass-code-start-branch": [
        ("git/how_do_i_start_a_new_feature_branch_phase",
         "branching off a fresh main"),
    ],
    "compass-agile-review-sprint": [
        ("agile/how_do_i_get_a_sprint_or_story_status",
         "the status and audit commands the review reads"),
    ],
    "compass-doc-add-component-model": [
        ("codegen/how_do_i_create_a_component_overview",
         "scaffolding and filling the overview"),
    ],
    "compass-codegen-add-component": [
        ("codegen/how_do_i_create_a_component_overview",
         "the overview every new component needs"),
    ],
    "compass-pr-sync-branch": [
        ("git/how_do_i_rebase_my_branch_on_main",
         "the rebase and what to do when it conflicts"),
    ],
}


def recipe_id(rel):
    path = RECIPES / f"{rel}.org"
    if not path.is_file():
        raise SystemExit(f"❌ no recipe at {path.relative_to(ROOT)}")
    m = re.search(r"^:ID:\s*(\S+)\s*$", path.read_text(encoding="utf-8"), re.M)
    if not m:
        raise SystemExit(f"❌ no :ID: in {path.relative_to(ROOT)}")
    return m.group(1)


def recipe_title(rel):
    path = RECIPES / f"{rel}.org"
    m = re.search(r"^#\+title:\s*(.+?)\s*$", path.read_text(encoding="utf-8"), re.M)
    return m.group(1) if m else rel


def link_line(rel, why):
    return f"- [[id:{recipe_id(rel)}][{recipe_title(rel)}]] — {why}."


def apply(name, wanted, check):
    path = SKILLS / name / "SKILL.org"
    if not path.is_file():
        raise SystemExit(f"❌ no skill at {path.relative_to(ROOT)}")
    text = path.read_text(encoding="utf-8")
    m = re.search(r"^\* Recipes\s*$(.*?)(?=^\* )", text, re.M | re.S)
    if not m:
        # A skill with no Recipes section has nowhere to delegate its
        # commands to, which is the state this sweep exists to end. Create
        # it above the artefacts block the scaffold always carries.
        if check:
            print(f"❌ {name} has no Recipes section", file=sys.stderr)
            return True
        anchor = "* Artefacts"
        block = "* Recipes\n\n* Reference\n\n"
        if anchor in text:
            text = text.replace(anchor, block + anchor, 1)
        else:
            text = text.rstrip("\n") + "\n\n" + block
        path.write_text(text, encoding="utf-8")
        m = re.search(r"^\* Recipes\s*$(.*?)(?=^\* )", text, re.M | re.S)

    body = m.group(1)
    missing = [(rel, why) for rel, why in wanted
               if f"id:{recipe_id(rel)}" not in body]
    if not missing:
        return False
    if check:
        for rel, _why in missing:
            print(f"❌ {name} does not link {rel}", file=sys.stderr)
        return True

    added = "\n".join(link_line(rel, why) for rel, why in missing)
    existing = body.strip("\n")
    new_body = f"\n\n{existing}\n{added}\n\n" if existing else f"\n\n{added}\n\n"
    path.write_text(text[:m.start(1)] + new_body + text[m.end(1):],
                    encoding="utf-8")
    return True


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true",
                    help="exit non-zero if a link is missing")
    args = ap.parse_args()

    changed = [name for name, wanted in OWNERS.items()
               if apply(name, wanted, args.check)]

    if args.check:
        if changed:
            print("   run: python3 build/scripts/link_skill_recipes.py",
                  file=sys.stderr)
            return 1
        print(f"✅ all {len(OWNERS)} skills link the recipes that own their "
              "commands.")
        return 0

    print(f"✅ linked {len(changed)} skill(s) to their recipes.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
