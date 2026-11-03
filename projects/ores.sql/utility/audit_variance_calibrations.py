#!/usr/bin/env python3
"""Audit every stored GJR-GARCH calibration record against what it should hold.

Run from the repository root: python3 -I projects/ores.sql/utility/audit_variance_calibrations.py. Reads the database named in .env as its
DDL user and prints one line per check, PASS or FAIL with the rows that fail.
"""
import math
import os
import subprocess
import sys
from collections import defaultdict
from datetime import date, timedelta

env = {}
for line in open(".env"):
    line = line.strip()
    if line and not line.startswith("#") and "=" in line:
        k, v = line.split("=", 1)
        env[k] = v.strip().strip('"')

DB = env.get("ORES_DB_DATABASE") or env.get("ORES_TEST_DB_DATABASE")
PSQL_ENV = dict(os.environ, PGPASSWORD=env["ORES_DB_DDL_PASSWORD"])
LIVE = "valid_to = ores_utility_infinity_timestamp_fn()"
NOT_TEST = "change_reason_code <> 'system.test'"


def rows(sql):
    out = subprocess.run(
        ["psql", "-h", env.get("ORES_DB_HOST", "localhost"), "-p", env.get("ORES_DB_PORT", "5432"),
         "-U", env["ORES_DB_DDL_USER"], "-d", DB, "-A", "-t", "-F", "\t", "-X", "-q", "-c", sql],
        env=PSQL_ENV, capture_output=True, text=True, check=True).stdout
    return [line.split("\t") for line in out.splitlines() if line]


failures = 0


def check(name, bad):
    global failures
    if bad:
        failures += 1
        print(f"FAIL {name}: {len(bad)}")
        for b in bad[:5]:
            print("     ", b)
    else:
        print(f"PASS {name}")


def num(x):
    return None if x == "" else float(x)


inputs = {r[0]: r for r in rows(
    f"select id, fixing_uri, provider_code, inverted, window_end, window_days, point_count, "
    f"used_count, base_currency_code, quote_currency_code, policy_id, policy_version "
    f"from ores_synthetic_variance_calibration_inputs_tbl where {LIVE} and {NOT_TEST}")}
points = defaultdict(list)
for r in rows(f"select input_id, fixing_date, fixing_text, fixing_value, log_return, used, "
              f"exclusion_reason from ores_synthetic_variance_calibration_input_points_tbl "
              f"where {LIVE} and {NOT_TEST} order by input_id, fixing_date"):
    points[r[0]].append(r)
cals = rows(f"select id, method, input_id, alpha, gamma, beta, persistence, long_run_variance, "
            f"starting_variance_ratio, log_likelihood, returns_used, starts, starts_agreed, state, "
            f"state_reason, base_currency_code, quote_currency_code, method_version, version "
            f"from ores_synthetic_variance_calibrations_tbl where {LIVE} and {NOT_TEST}")
feeds = rows(f"select source_name, variance_model, variance_calibration_id, base_currency_code, "
             f"quote_currency_code from ores_synthetic_fx_spot_generation_configs_tbl where {LIVE} "
             f"and {NOT_TEST}")
policy = rows(f"select id, version, variance_window_days from ores_synthetic_calibration_policies_tbl "
              f"where {LIVE} and asset_class_code = 'fx'")
print(f"{len(inputs)} inputs, {sum(len(v) for v in points.values())} points, {len(cals)} calibrations, "
      f"{len(feeds)} FX feeds")

check("every input holds exactly the points it counts",
      [(i, r[6], len(points[i])) for i, r in inputs.items() if int(r[6]) != len(points[i])])
check("every input's used count is its used points",
      [(i, r[7], sum(p[5] == "t" for p in points[i])) for i, r in inputs.items()
       if int(r[7]) != sum(p[5] == "t" for p in points[i])])
bad = []
for i, r in inputs.items():
    end = date.fromisoformat(r[4])
    start = end - timedelta(days=int(r[5]))
    dates = [date.fromisoformat(p[1]) for p in points[i]]
    if any(d < start or d > end for d in dates) or len(set(dates)) != len(dates):
        bad.append((i, r[1], min(dates), max(dates), start, end))
check("every point lies in its input's window, once per date", bad)
if policy:
    pid, pver, pwin = policy[0]
    check("every input was read under the FX policy's current id, version and variance window",
          [(i, r[10], r[11], r[5]) for i, r in inputs.items()
           if r[10] != pid or r[11] != pver or r[5] != pwin])

bad_ret, bad_reason = [], []
for i, r in inputs.items():
    inverted = r[3] == "t"
    previous = None
    for p in points[i]:
        value = num(p[3])
        if p[5] == "t":
            if previous is None or value is None or p[4] == "":
                bad_ret.append((i, p[1], "used without a return or a previous value"))
            else:
                expected = math.log(value / previous) * (-1 if inverted else 1)
                if abs(float(p[4]) - expected) > 1e-12 * max(1.0, abs(expected)):
                    bad_ret.append((i, p[1], float(p[4]), expected))
            if p[6] != "":
                bad_reason.append((i, p[1], "used with a reason"))
        else:
            if p[6] == "":
                bad_reason.append((i, p[1], "left out with no reason"))
        if value is not None and value > 0:
            previous = value
    first_readable = next((p for p in points[i] if num(p[3]) and num(p[3]) > 0), None)
    if first_readable and first_readable[5] == "t":
        bad_reason.append((i, first_readable[1], "the first readable point is used"))
check("every used return is the log of its fixing over the previous one, in market convention",
      bad_ret)
check("every point is used with no reason, or left out with one", bad_reason)

bad = []
for c in cals:
    (cid, method, input_id, a, g, b, pers, lrv, ratio, ll, returns, starts, agreed, state,
     reason, base, quote, mver, version) = c
    a, g, b, pers, lrv, ratio, ll = map(num, (a, g, b, pers, lrv, ratio, ll))
    if method == "gjr_garch_qmle":
        inp = inputs.get(input_id)
        if inp is None:
            bad.append((cid, "names an input that is not stored"))
            continue
        if (inp[8], inp[9]) != (base, quote):
            bad.append((cid, "pair differs from its input's"))
        if state != "rejected":
            if int(returns) != int(inp[7]):
                bad.append((cid, "returns_used", returns, "input used", inp[7]))
            if not (0 <= a < 1 and 0 <= b < 1 and a + g >= 0 and pers < 1):
                bad.append((cid, "inadmissible shape", a, g, b, pers))
            lo, hi = a + b + min(0.0, g), a + b + max(0.0, g)
            if not (lo - 1e-12 <= pers <= hi + 1e-12):
                bad.append((cid, "persistence outside alpha + gamma p + beta for p in [0, 1]", pers))
            if not (lrv and lrv > 0 and ratio and ratio > 0 and ll is not None and math.isfinite(ll)):
                bad.append((cid, "missing or non-finite evidence"))
            if int(starts) <= 0:
                bad.append((cid, "no starts"))
        else:
            if reason == "":
                bad.append((cid, "rejected with no reason"))
            # A fit's own rejection is the calibration's first version; a reviewer's is a later one.
            if int(version) == 1 and int(inp[7]) >= 250 and "fit failed" not in reason:
                bad.append((cid, "the fit rejected an input with enough returns", inp[7], reason))
    else:
        if input_id != "" or int(returns) != 0 or int(starts) != 0 or ll is not None:
            bad.append((cid, "an authored shape that claims fit evidence"))
        if abs(pers - (a + 0.5 * g + b)) > 1e-12:
            bad.append((cid, "authored persistence is not alpha + gamma / 2 + beta", pers))
    if state in ("approved", "rejected", "superseded") and reason == "":
        bad.append((cid, state, "with no reason"))
check("every calibration's evidence agrees with its method and state", bad)

approved = defaultdict(list)
for c in cals:
    if c[13] == "approved":
        approved[(c[15], c[16])].append(c[0])
check("at most one approved calibration per pair",
      [(k, v) for k, v in approved.items() if len(v) > 1])
by_id = {c[0]: c for c in cals}
bad = []
for f in feeds:
    name, model, cid, base, quote = f
    if model == "gjr_garch":
        c = by_id.get(cid)
        if c is None:
            bad.append((name, "names no stored calibration", cid))
        elif (c[15], c[16]) != (base, quote):
            bad.append((name, "runs another pair's calibration", cid))
        elif c[13] != "approved":
            bad.append((name, "runs a calibration that is", c[13]))
check("every GJR-GARCH feed names the approved calibration of its own pair", bad)

sys.exit(1 if failures else 0)
