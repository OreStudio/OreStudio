"""Compares each declared economic list with the old name rule.

Run from the repository root. A mismatch means a model declares a field the
old rule would have treated differently.
"""
import re, sys, glob
sys.path.insert(0, "scripts")
from declare_economic_fields import is_economic
roots = ["projects/ores.trading/api/include/ores.trading.api/domain", "projects/ores.dq/api/include/ores.dq.api/domain"]
bad = 0; checked = 0
for root in roots:
    for h in glob.glob(root + "/*.hpp"):
        t = open(h).read()
        m = re.search(r"economic_fields_of\(std::type_identity<(\w+)>\)", t)
        if not m: continue
        name = m.group(1)
        sm = re.search(r"struct %s (?:final )?\{(.*?)\n\};" % name, t, re.S)
        body = re.sub(r"/\*.*?\*/", "", sm.group(1), flags=re.S)
        body = re.sub(r"friend bool operator==.*?;", "", body, flags=re.S)
        members = re.findall(r"^\s+(?:rfl::Skip<.*?>|[\w:<>,\s\*&]+?)\s+(\w+)(?:\s*=\s*[^;]+)?;", body, re.M)
        arr = re.search(r"%s_economic_fields\[\] = \{(.*?)\};" % name, t, re.S)
        declared = re.findall(r'"(\w+)"', arr.group(1)) if arr else []
        expect = [x for x in members if is_economic(x) and x not in ("identity","audit")]
        # group members: identity/audit hold structs; legacy included them by name, recursed
        checked += 1
        if sorted(declared) != sorted(expect):
            bad += 1
            print("MISMATCH", name, "declared-only:", sorted(set(declared)-set(expect)), "legacy-only:", sorted(set(expect)-set(declared)))
print("checked", checked, "mismatches", bad)
