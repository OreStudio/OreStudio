#!/usr/bin/env python3
"""Regenerate the skills catalogue from the SKILL.org sources.

Scans doc/llm/skills/*/SKILL.org, extracts each skill's ID, name,
title, and description, groups them by domain prefix per the skill
naming conventions, and rewrites the generated region of
doc/llm/skills/claude_code_skills.org (between the BEGIN/END
generated-catalogue markers). Everything outside the markers is
preserved, so the prose, naming note, retired-skills section, and
see-also survive regeneration.

The catalogue is derived data: never edit the generated region by
hand — fix the SKILL.org sources (or this script) and re-run.

Usage: python3 build/scripts/generate_skills_catalogue.py [--check]
  --check  exit non-zero if the catalogue is stale (for CI).
"""
import argparse
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
SKILLS_DIR = ROOT / "doc" / "llm" / "skills"
CATALOGUE = SKILLS_DIR / "claude_code_skills.org"
BEGIN = "# BEGIN generated catalogue (build/scripts/generate_skills_catalogue.py)"
END = "# END generated catalogue"

# Prefix -> (section heading, blurb). Order defines section order.
DOMAINS = {
    "agile": ("Agile (=agile-=)",
              "Stories, tasks, sprints, backlog, journal, orientation."),
    "pr": ("Pull requests (=pr-=)", "PR lifecycle."),
    "doc": ("Documentation (=doc-=)",
            "Recipes, knowledge, manual, memory, diagrams, search."),
    "code": ("Hand-written code (=code-=)",
             "Build, tests, review, investigation — code written by hand."),
    "codegen": ("Generated code (=codegen-=)",
                "Entities, components, and schemas produced by ores.codegen."),
    "devops": ("DevOps (=devops-=)",
               "Environment, database, services, shell, client, site."),
    "skill": ("Skills (=skill-=)", "The skills themselves."),
}

# Planned-but-not-yet-created skills, shown as prose under each section.
PENDING = {
    "agile": ("=agile-add-story=, =agile-add-task=, =agile-start-story=, "
              "=agile-start-task=, =agile-start-hotfix=, =agile-close-task=, "
              "=agile-show-status=, =agile-show-fleet=, =agile-show-journal=, "
              "=agile-show-backlog=, =agile-show-sprint="),
    "pr": "=pr-merge=, =pr-address-review=, =pr-sync-branch=, =pr-show-checks=",
    "doc": "=doc-find=, =doc-show=, =doc-sync-index=, =doc-add-knowledge=",
    "code": "=code-start-branch=",
    "devops": ("=devops-setup-environment=, =devops-recreate-db=, "
               "=devops-run-sql=, =devops-start-services=, "
               "=devops-stop-services=, =devops-show-services=, "
               "=devops-run-shell=, =devops-run-client=, =devops-deploy-site=, "
               "=devops-deploy-manual=, =devops-deploy-skills=, "
               "=devops-deploy-settings="),
}

MAX_DESC = 110


def skill_meta(path):
    text = (path / "SKILL.org").read_text(encoding="utf-8")
    sid = re.search(r"^:ID: (\S+)", text, re.M).group(1)
    title = re.search(r"^#\+title: (.*)$", text, re.M).group(1).strip()
    desc = re.search(r"^description: (.*)$", text, re.M).group(1).strip()
    desc = desc.replace("|", "/")
    if len(desc) > MAX_DESC:
        desc = desc[:MAX_DESC - 3].rstrip() + "..."
    return sid, title, desc


def generate():
    groups = {prefix: [] for prefix in DOMAINS}
    strays = []
    for d in sorted(SKILLS_DIR.iterdir()):
        if not d.is_dir() or not (d / "SKILL.org").is_file():
            continue
        prefix = d.name.split("-", 1)[0]
        if prefix in groups:
            groups[prefix].append(d)
        else:
            strays.append(d.name)
    if strays:
        print(f"⚠  skills with no register prefix: {', '.join(strays)}",
              file=sys.stderr)

    parts = [BEGIN, ""]
    for prefix, (heading, blurb) in DOMAINS.items():
        parts += [f"* {heading}", "", blurb, ""]
        if groups[prefix]:
            parts += ["| Name | Title | Description |",
                      "|------+-------+-------------|"]
            for d in groups[prefix]:
                sid, title, desc = skill_meta(d)
                parts.append(f"| [[id:{sid}][{d.name}]] | {title} | {desc} |")
            parts.append("")
        if prefix in PENDING:
            parts += ["Being created by the /Action-oriented skill cleanup/ "
                      "story:", f"{PENDING[prefix]}.", ""]
    parts.append(END)
    return "\n".join(parts)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--check", action="store_true",
                    help="exit 1 if the catalogue is stale")
    args = ap.parse_args()

    text = CATALOGUE.read_text(encoding="utf-8")
    m = re.search(re.escape(BEGIN) + r".*?" + re.escape(END), text, re.S)
    if not m:
        print(f"❌ markers not found in {CATALOGUE}", file=sys.stderr)
        return 1
    fresh = text[:m.start()] + generate() + text[m.end():]
    if args.check:
        if fresh != text:
            print("❌ skills catalogue is stale — run "
                  "build/scripts/generate_skills_catalogue.py",
                  file=sys.stderr)
            return 1
        print("✅ skills catalogue is up to date")
        return 0
    if fresh != text:
        CATALOGUE.write_text(fresh, encoding="utf-8")
        print(f"✅ regenerated {CATALOGUE.relative_to(ROOT)}")
    else:
        print("✅ catalogue already up to date")
    return 0


if __name__ == "__main__":
    sys.exit(main())
