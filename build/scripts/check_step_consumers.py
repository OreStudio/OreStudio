#!/usr/bin/env python3
"""Every step-command stream carries exactly one consumer, and the transport
numbers the atomic step claim is built from.

The contract this proves, and why it needs a live server
--------------------------------------------------------
A step command is delivered at least once, so two deliveries of one message can
be in flight together. The transport answers that with one consumer per stream,
declared from one `step_routes` table, and acking inside the handler. That shape
is not visible in the source: a durable consumer is a server-side object, and a
service that declares one durable per subject produces exactly the same C++ as a
service that declares one. Only the server can be asked.

So this check runs against a live NATS server and asserts, for every stream whose
name ends in `_steps`:

  * exactly one consumer, named `<stream>_consumer`, so a second durable cannot
    be added without this failing;
  * that consumer's ack policy is explicit, so no delivery is acknowledged on the
    server's behalf;
  * ack wait 30 s and maximum deliveries 10, which are the two numbers the atomic
    step claim's timing invariant is written against -- hold off x (deliveries
    minus one) must exceed the lease. Change either and the lease stops being
    able to outlast a dead holder's redeliveries, which is a bug no other gate
    can see.

What it does when there is no server
------------------------------------
It fails, with the command to run. It does not skip. `--exclude-heavy` keeps it
out of the ordinary run, so reaching it means the caller asked for the heavy
phase, and a check that answers "nothing to do" when it cannot reach the thing it
checks is a check that silently stops checking. A skip here would be invisible in
exactly the run that was supposed to prove the contract.

Why it is not in the ordinary phase
-----------------------------------
The streams exist only once the services have started and declared them, and the
fleet is not up during an ordinary `compass check`. Running it there would fail
on a healthy machine for the wrong reason, which trains people to ignore it.

The static half, considered and left out
----------------------------------------
A source-level check could assert that no single-subject step subscription helper
exists (`ensure_step_stream`, `subscribe_step_command` are gone from the tree),
and that every service declares its step subjects through `step_routes`. That
passes on bad code: a durable built inline, as
`js_queue_subscribe(subject + "_consumer", ...)`, contains none of the strings
such a check looks for, creates the second durable this gate exists to forbid,
and passes. A proxy that cannot fail on the regression it names is worse than no
static check, because it is read as proof. So the gate is the live one only.

Usage:
  python3 build/scripts/check_step_consumers.py
"""
from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

# The two numbers the claim's timing invariant is written against, and the ack
# policy that makes a handler responsible for answering.
EXPECTED_ACK_WAIT_NS = 30 * 1_000_000_000
EXPECTED_MAX_DELIVER = 10


def load_env() -> dict[str, str]:
    """The environment's values, from .env, without running compass."""
    values: dict[str, str] = {}
    env_file = REPO_ROOT / ".env"
    if not env_file.is_file():
        return values
    for line in env_file.read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, value = line.partition("=")
        values[key.strip()] = value.strip()
    return values


def nats_command(env: dict[str, str]) -> list[str] | None:
    """The nats CLI, pointed at this environment, or None when unusable."""
    port = env.get("ORES_NATS_PORT", "4222")
    ca = env.get("ORES_NATS_TLS_CA", "")
    cert = env.get("ORES_NATS_TLS_CERT", "")
    key = env.get("ORES_NATS_TLS_KEY", "")
    if not (ca and cert and key):
        return None
    for path in (ca, cert, key):
        if not Path(path).is_file():
            return None
    return ["nats", "--server", f"tls://localhost:{port}",
            "--tlsca", ca, "--tlscert", cert, "--tlskey", key]


def run_json(argv: list[str]) -> object | None:
    try:
        r = subprocess.run(argv, capture_output=True, text=True, timeout=30)
    except (OSError, subprocess.TimeoutExpired):
        return None
    if r.returncode != 0:
        return None
    try:
        return json.loads(r.stdout)
    except json.JSONDecodeError:
        return None


def consumer_names(raw: object) -> list[str]:
    if not raw:
        return []
    names = []
    for item in raw if isinstance(raw, list) else [raw]:
        names.append(item.get("name", "") if isinstance(item, dict) else str(item))
    return sorted(names)


def main() -> int:
    env = load_env()
    nats = nats_command(env)
    if nats is None:
        print("FAIL: no NATS client configuration. .env must name the CA and the"
              " client certificate, and the files must exist.", file=sys.stderr)
        print("  Run: ./compass.sh services start", file=sys.stderr)
        return 2

    streams_raw = run_json(nats + ["stream", "ls", "--json"])
    if streams_raw is None:
        print("FAIL: could not list streams. Is the server up?", file=sys.stderr)
        print("  Run: ./compass.sh services start", file=sys.stderr)
        return 2
    streams = sorted(s for s in streams_raw if str(s).endswith("_steps"))
    if not streams:
        print("FAIL: no step-command stream found. The services declare one on"
              " startup, so none exists until the fleet has run once.",
              file=sys.stderr)
        print("  Run: ./compass.sh services start, then let the services come up.",
              file=sys.stderr)
        return 2

    failures = 0
    for stream in streams:
        want = f"{stream}_consumer"
        names = consumer_names(run_json(nats + ["consumer", "ls", stream, "--json"]))
        info = run_json(nats + ["stream", "info", stream, "--json"]) or {}
        config = info.get("config", {}) if isinstance(info, dict) else {}
        retention = config.get("retention", "?")
        consumers = info.get("state", {}).get("consumer_count") if isinstance(info, dict) else None

        if names != [want]:
            print(f"FAIL {stream}: {len(names)} consumer(s) {names}, "
                  f"want exactly ['{want}']")
            print("     One durable per stream is what makes exactly one delivery"
                  " do a step's work. Declare the family in one step_routes table"
                  " and serve it with serve_step_commands.")
            failures += 1
            continue

        detail = run_json(nats + ["consumer", "info", stream, want, "--json"]) or {}
        cfg = detail.get("config", {}) if isinstance(detail, dict) else {}
        ack_policy = cfg.get("ack_policy", "?")
        ack_wait = cfg.get("ack_wait", 0)
        max_deliver = cfg.get("max_deliver", 0)

        problems = []
        if ack_policy != "explicit":
            problems.append(f"ack_policy={ack_policy}, want explicit")
        if ack_wait != EXPECTED_ACK_WAIT_NS:
            problems.append(f"ack_wait={ack_wait}ns, want {EXPECTED_ACK_WAIT_NS}ns (30s)")
        if max_deliver != EXPECTED_MAX_DELIVER:
            problems.append(f"max_deliver={max_deliver}, want {EXPECTED_MAX_DELIVER}")
        if problems:
            print(f"FAIL {stream}/{want}: " + "; ".join(problems))
            print("     step_claim_lease is written against these numbers: the"
                  " hold off times (deliveries - 1) must exceed the lease, or a"
                  " dead holder's redeliveries run out before the lease ends.")
            failures += 1
            continue

        print(f"PASS {stream}: 1 consumer '{want}', ack={ack_policy}, "
              f"ack_wait={ack_wait // 1_000_000_000}s, max_deliver={max_deliver}, "
              f"retention={retention}, stream_consumers={consumers}")

    print()
    if failures:
        print(f"FAIL: {failures} of {len(streams)} step stream(s) do not match the"
              " one-consumer contract.")
        return 1
    print(f"PASS: every step stream carries exactly its own consumer, and the"
          f" numbers the claim is built from ({len(streams)} streams).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
