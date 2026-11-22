#!/usr/bin/env python3
"""Prune the deployed skills to the slice of one level.

An agent spawned into the pruned tree cannot load a skill outside its
slice, which makes the attenuation binding: the audit channel is held
read-only by what is on disk when it starts, rather than by asking it in a
prompt. Normal operation deploys the full set and leaves every advisory
level free to reach outside its slice. Rebuild the bundle to restore it.

The slices are compass-ng's: its skill_levels codec writes them into the
levels page from compass/checks/skill_levels.org. A deployed skill the
table "Levels" of that document does not name is left alone.

Usage: python3 build/scripts/prune_deployed_skills.py LEVEL
"""
import pathlib
import re
import shutil
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
DOCUMENT = ROOT / "compass" / "checks" / "skill_levels.org"
PAGE = ROOT / "doc" / "llm" / "skills" / "skill_systems_levels.org"
DEPLOYED = ROOT / ".claude" / "skills"


def tabled():
    """The skills the table "Levels" of the check document names."""
    text = DOCUMENT.read_text(encoding="utf-8")
    section = re.search(r"^\* Levels\n(.*?)^\* ", text, re.S | re.M).group(1)
    rows = [line for line in section.splitlines()
            if line.startswith("|") and not line.startswith("|-")]
    return {row.split("|")[1].strip() for row in rows[1:]}


def slices():
    """Each level's slice, from the generated slices of the levels page."""
    text = PAGE.read_text(encoding="utf-8")
    region = re.search(r"^# BEGIN generated slices .*?^# END generated slices",
                       text, re.S | re.M).group(0)
    out = {}
    for m in re.finditer(r"^=(\S+)=, \w+: \d+ of \d+ skills\.\n\n?"
                         r"((?:- .*\n)*)", region, re.M):
        out[m.group(1)] = {line[2:] for line in m.group(2).splitlines()}
    return out


def main():
    if len(sys.argv) != 2:
        print(__doc__, file=sys.stderr)
        return 2
    level = sys.argv[1]
    by_level = slices()
    if level not in by_level:
        print(f"❌ no slice for {level}; the levels are "
              f"{', '.join(by_level)}", file=sys.stderr)
        return 1
    if not DEPLOYED.is_dir():
        print(f"❌ nothing deployed at {DEPLOYED}", file=sys.stderr)
        return 1
    names = tabled()
    removed = []
    for d in sorted(DEPLOYED.iterdir()):
        if d.is_dir() and d.name in names and d.name not in by_level[level]:
            shutil.rmtree(d)
            removed.append(d.name)
    kept = sum(1 for d in DEPLOYED.iterdir() if d.is_dir())
    print(f"🧭 deployed tree pruned to {level}: {kept} skill(s) remain, "
          f"{len(removed)} removed.")
    print("   Rebuild the bundle to restore the full set.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
