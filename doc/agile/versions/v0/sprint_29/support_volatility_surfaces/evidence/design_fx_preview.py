"""Summarise a saved FX class calibration preview against the proposed approval rules.

Save the preview first: curl -b <session> http://localhost:<web port>/api/synthetic/calibration/fx
-o fx_preview.json, then run python3 -I design_fx_preview.py fx_preview.json.
"""
import json
import sys

RULES = "returns>=250, starts agree, 0.80<=persistence<=0.995, alpha+gamma/2>=0.01"


def failures(v):
    out = []
    if (v.get("used_count") or 0) < 250:
        out.append("returns<250")
    if not v.get("starts_agreed"):
        out.append("starts disagree")
    if not 0.80 <= (v.get("persistence") or 0) <= 0.995:
        out.append("persistence out of range")
    if (v.get("alpha") or 0) + (v.get("gamma") or 0) / 2 < 0.01:
        out.append("no reaction")
    return out


preview = json.load(open(sys.argv[1]))
pairs = {}
for feed in preview.get("feeds", []):
    if feed.get("variance"):
        pairs[feed["base_currency_code"] + "/" + feed["quote_currency_code"]] = feed["variance"]
print("rules:", RULES)
print("pair | state | returns | starts_agreed | persistence | alpha | gamma | outcome")
passed = waiting = 0
for pair in sorted(pairs):
    v = pairs[pair]
    if v["state"] != "proposed":
        outcome = "not proposed: " + (v.get("state_reason") or v["state"])[:60]
    else:
        f = failures(v)
        outcome = "waits: " + ", ".join(f) if f else "passes"
        passed += not f
        waiting += bool(f)
    print(f"{pair} | {v['state']} | {v.get('used_count')} | {v.get('starts_agreed')} | "
          f"{(v.get('persistence') or 0):.4f} | {(v.get('alpha') or 0):.3g} | "
          f"{(v.get('gamma') or 0):.3g} | {outcome}")
print(f"proposed fits: {passed + waiting}; pass: {passed}; wait: {waiting}")
