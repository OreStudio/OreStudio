#!/usr/bin/env python3
"""
Wire cross-reference links into the org-mode model graph (task 95031F88).

Three passes:
  1. Add * Sub-components table to group overviews that are missing it.
  2. Add group back-link to each sub-component overview (in * See also).
  3. Add group back-link to each entity org (new * See also section).
"""
import re
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent

# ─── Data ──────────────────────────────────────────────────────────────────────

GROUP_IDS = {
    "analytics":  "D4E6E417-7D34-44B1-A373-73A1C9183137",
    "compute":    "A8B7F2C9-D416-4E83-B527-9F814E62C1A5",
    "controller": "D2C5E91A-7864-4F32-A1B9-6E50D2B47A88",
    "database":   "3A8F2E91-D4C7-8AB4-5E23-91D6F8C04A72",
    "dq":         "AAF28605-81BE-4B7F-9B6E-7B9B1D99D7C3",
    "iam":        "BD9653B7-C92B-4A71-9B5D-7ADB4EBCB94F",
    "refdata":    "D774690E-B214-42B0-94FC-B636C49F6F37",
    "reporting":  "F1B83A47-E029-4D78-9A56-31B7CFE49265",
    "scheduler":  "B788F24E-2E3F-432A-BD4F-CA8D6EBB2C9D",
    "trading":    "5C7A961E-D834-4B2F-AE15-F049B27361D8",
    "workflow":   "7F4B91E2-3D86-4A57-B92C-08E146DA7359",
    "workspace":  "E6134412-DF7E-4CBF-B2C2-BF84EDBC618E",
}

# Groups whose overviews are missing a * Sub-components section
NEED_SUBCOMP = ["compute", "controller", "reporting", "trading", "workflow"]

# ─── Helpers ──────────────────────────────────────────────────────────────────

def read_org_field(text, field):
    m = re.search(rf'^#\+{field}:\s+(.+)', text, re.M)
    return m.group(1).strip() if m else ""

def read_id(text):
    m = re.search(r'^:ID:\s+(\S+)', text, re.M)
    return m.group(1) if m else ""

def has_section(text, name):
    return bool(re.search(rf'^\* {re.escape(name)}', text, re.M))

def has_id_link(text, uuid):
    return uuid.lower() in text.lower()


# ─── Pass 1: add * Sub-components to group overviews ─────────────────────────

def build_subcomp_table(group):
    sub_paths = sorted(
        p for p in (ROOT / f"projects/ores.{group}").rglob("component_overview.org")
        if p.parent != ROOT / f"projects/ores.{group}/modeling"
    )
    rows = []
    for p in sub_paths:
        text = p.read_text()
        sub_id   = read_id(text)
        title    = read_org_field(text, "title")
        desc     = read_org_field(text, "description")
        # Truncate long descriptions for the table cell
        if len(desc) > 90:
            desc = desc[:87] + "…"
        rows.append(f"| [[id:{sub_id}][{title}]] | {desc} |")
    if not rows:
        return None
    body = "\n".join(rows)
    return f"\n* Sub-components\n\n| Sub-component | Brief |\n|---------------+-------|\n{body}\n"


def pass1_add_subcomponents():
    changed = []
    for group in NEED_SUBCOMP:
        path = ROOT / f"projects/ores.{group}/modeling/component_overview.org"
        text = path.read_text()
        if has_section(text, "Sub-components"):
            continue
        table = build_subcomp_table(group)
        if not table:
            continue
        # Insert before * Entity modules (or * Summary if not present)
        insert_before = re.search(r'^\* Entity modules', text, re.M)
        if insert_before:
            pos = insert_before.start()
            text = text[:pos] + table + "\n" + text[pos:]
        else:
            text = text.rstrip() + "\n" + table + "\n"
        path.write_text(text)
        changed.append(str(path.relative_to(ROOT)))
    return changed


# ─── Pass 2: add group back-link to sub-component overviews ──────────────────

def pass2_subcomp_group_links():
    changed = []
    for group, gid in GROUP_IDS.items():
        group_path = ROOT / f"projects/ores.{group}/modeling/component_overview.org"
        if not group_path.exists():
            continue
        group_title = read_org_field(group_path.read_text(), "title")
        sub_paths = sorted(
            p for p in (ROOT / f"projects/ores.{group}").rglob("component_overview.org")
            if p.parent != ROOT / f"projects/ores.{group}/modeling"
        )
        for p in sub_paths:
            text = p.read_text()
            if has_id_link(text, gid):
                continue  # already linked
            link = f"- [[id:{gid}][{group_title}]] — component group overview."
            if has_section(text, "See also"):
                # Append the link at the end of the existing * See also section
                text = re.sub(
                    r'(^\* See also\n)',
                    rf'\1\n{link}\n',
                    text, count=1, flags=re.M
                )
            else:
                text = text.rstrip() + f"\n\n* See also\n\n{link}\n"
            p.write_text(text)
            changed.append(str(p.relative_to(ROOT)))
    return changed


# ─── Pass 3: add group back-link to entity orgs ──────────────────────────────

def pass3_entity_group_links():
    changed = []
    for group, gid in GROUP_IDS.items():
        modeling = ROOT / f"projects/ores.{group}/modeling"
        if not modeling.exists():
            continue
        group_path = modeling / "component_overview.org"
        if not group_path.exists():
            continue
        group_title = read_org_field(group_path.read_text(), "title")
        entity_paths = sorted(
            p for p in modeling.glob(f"ores.{group}.*.org")
            if not p.name.endswith("module.org")
        )
        for p in entity_paths:
            text = p.read_text()
            if has_id_link(text, gid):
                continue
            link = f"- [[id:{gid}][{group_title}]] — component group overview."
            if has_section(text, "See also"):
                text = re.sub(
                    r'(^\* See also\n)',
                    rf'\1\n{link}\n',
                    text, count=1, flags=re.M
                )
            else:
                text = text.rstrip() + f"\n\n* See also\n\n{link}\n"
            p.write_text(text)
            changed.append(str(p.relative_to(ROOT)))
    return changed


# ─── Main ─────────────────────────────────────────────────────────────────────

if __name__ == "__main__":
    dry = "--dry-run" in sys.argv

    c1 = pass1_add_subcomponents()
    c2 = pass2_subcomp_group_links()
    c3 = pass3_entity_group_links()

    print(f"Pass 1 — group overviews updated ({len(c1)}): {', '.join(c1) or 'none'}")
    print(f"Pass 2 — sub-component back-links ({len(c2)}):")
    for f in c2: print(f"  {f}")
    print(f"Pass 3 — entity back-links ({len(c3)}):")
    for f in c3: print(f"  {f}")
    print(f"\nTotal files changed: {len(c1) + len(c2) + len(c3)}")
