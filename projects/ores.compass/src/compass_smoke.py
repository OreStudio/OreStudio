# -*- coding: utf-8 -*-
"""compass smoke — end-to-end checks against a running system.

A smoke check is the safety net the unit suites cannot be: it starts what the
product starts, provisions a tenant the way an operator does, and reads the
evidence the running services leave behind. It is deliberately small and
boring, because a check that needs a database and every service must be the
one thing that never lies about the path.
"""

import argparse
import re
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass, field
from pathlib import Path

# The bootstrap account the provisioning recipes use. It is a development
# password, not a secret: a fresh database has no other way in.
SUPER_ADMIN = "super_admin"
SUPER_ADMIN_EMAIL = "super_admin@localhost.com"
BOOTSTRAP_PASSWORD = "Secure-Password-123"
DEFAULT_TENANT = "acme_corporation"
DEFAULT_TENANT_NAME = "Acme Corporation"
DEFAULT_PROFILE = "acme_demo"

DEMAND_LINE = re.compile(
    r"Adapter '(?P<adapter>[^']+)' accepted (?P<accepted>\d+) and rejected "
    r"(?P<rejected>\d+) of (?P<demanded>\d+) id\(s\)")


@dataclass
class demand:
    """One adapter's reply to the last demand set the supervisor sent it."""

    adapter: str
    accepted: int
    rejected: int
    demanded: int


@dataclass
class evidence:
    """What the running system left behind, ready for the verdict."""

    tenant: str
    routes: int = 0
    subscriptions: int = 0
    ticks: int = 0
    first_tick: str = ""
    last_tick: str = ""
    future_ticks: int = 0
    demand: list[demand] = field(default_factory=list)
    waited_seconds: int = 0
    log_file: str = ""


def parse_demand(text: str) -> list[demand]:
    """The demand replies in @p text, oldest first.

    The supervisor logs one line for each reply it accepts, so the last line
    of an adapter is the state that matters. The parser is deliberately
    forgiving: a log holds every run's lines, and a line from another shape
    must not fail the check.
    """
    found = []
    for match in DEMAND_LINE.finditer(text):
        found.append(demand(
            adapter=match.group("adapter"),
            accepted=int(match.group("accepted")),
            rejected=int(match.group("rejected")),
            demanded=int(match.group("demanded")),
        ))
    return found


def latest_demand(lines: list[demand]) -> list[demand]:
    """Keep the last reply of each adapter."""
    latest: dict[str, demand] = {}
    for line in lines:
        latest[line.adapter] = line
    return [latest[key] for key in sorted(latest)]


def evaluate(ev: evidence) -> list[str]:
    """The reasons the system is not ticking, in the order a person should read them.

    The order follows the path: a party needs a subscription and a route, the
    route must reach an adapter as a demand, the adapter must accept the ids,
    and a tick must land. A failure early on explains every later one, so the
    first reason is the one to fix.
    """
    reasons = []
    if ev.subscriptions == 0:
        reasons.append(f"tenant '{ev.tenant}' holds no subscription")
    if ev.routes == 0:
        reasons.append(f"tenant '{ev.tenant}' has no route, so no source serves its parties")
    if not ev.demand:
        reasons.append("no adapter got a demand set: the supervisor sent nothing")
    else:
        demanded = sum(d.demanded for d in ev.demand)
        accepted = sum(d.accepted for d in ev.demand)
        if demanded == 0:
            reasons.append("every demand set was empty: no party's ids reached an adapter")
        elif accepted < demanded:
            reasons.append(
                f"the adapters accepted {accepted} of the {demanded} ids demanded")
    if ev.ticks == 0:
        reasons.append(f"no tick arrived within {ev.waited_seconds}s")
    if ev.future_ticks:
        reasons.append(f"{ev.future_ticks} tick(s) are dated in the future")
    return reasons


def _run(argv: list[str], root: Path, timeout: int | None = None) -> subprocess.CompletedProcess:
    """Run one command from the project root, with its output shown."""
    print("$ " + " ".join(argv), flush=True)
    return subprocess.run(argv, cwd=str(root), text=True, capture_output=True, timeout=timeout)


def _sql(root: Path, query: str) -> str:
    """One scalar row from the database, through the project's own psql path."""
    result = _run(["./compass.sh", "sql", "--", "-t", "-A", "-c", query], root, timeout=120)
    if result.returncode != 0:
        raise RuntimeError(result.stderr.strip() or "psql failed")
    return result.stdout.strip()


def _tenant_geometry(root: Path, tenant: str) -> tuple[int, int]:
    """The subscription and route counts of @p tenant."""
    subs = int(_sql(root, (
        "select count(*) from ores_marketdata_subscriptions_tbl s "
        "join ores_iam_tenants_tbl t on t.id = s.tenant_id "
        f"where t.code = '{tenant}'")) or 0)
    routes = int(_sql(root, (
        "select count(*) from ores_marketdata_routes_tbl r "
        "join ores_iam_tenants_tbl t on t.id = r.tenant_id "
        f"where t.code = '{tenant}'")) or 0)
    return subs, routes


def _tick_counts(root: Path, tenant: str) -> tuple[int, str, str, int]:
    """The tick count, its first and last instant, and its future-dated count."""
    row = _sql(root, (
        "select count(*), coalesce(min(ts)::text, ''), coalesce(max(ts)::text, ''), "
        "count(*) filter (where ts > now() + interval '5 seconds') "
        "from ores_marketdata_ticks_tbl tk "
        "join ores_iam_tenants_tbl t on t.id = tk.tenant_id "
        f"where t.code = '{tenant}'"))
    parts = row.split("|")
    if len(parts) != 4:
        raise RuntimeError(f"unexpected tick query output: {row!r}")
    return int(parts[0] or 0), parts[1], parts[2], int(parts[3] or 0)


def _log_path(root: Path, env: dict) -> Path:
    preset = env.get("ORES_PRESET", "")
    return root / "build" / "output" / preset / "publish" / "log" / "ores.marketdata.service.log"


def _service_unit(env: dict) -> str:
    """The systemd unit of the marketdata service in this checkout.

    Each worktree names its units after its label, so the journal of this
    environment is the only one the check may read.
    """
    label = env.get("ORES_CHECKOUT_LABEL", "")
    return f"ores.marketdata.service-{label}.service"


def _service_text(root: Path, env: dict, start_epoch: float, log_path: Path) -> str:
    """The marketdata log since @p start_epoch, from its file or from the journal.

    Services write a file only when the launcher asks them to; under systemd
    they write to the journal. The check reads whichever holds the run it
    started, so it works in both shapes.
    """
    # The launcher's file and the journal do not always carry the same lines:
    # a unit started under systemd writes the demand replies to the journal,
    # and the file may hold only part of the run. Read both and let the parser
    # find the lines.
    parts = []
    if log_path.is_file():
        parts.append(log_path.read_text(errors="replace"))
    since = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(start_epoch))
    result = _run(["journalctl", "--user", "-u", _service_unit(env),
                   "--since", since, "--no-pager", "-o", "cat"], root, timeout=120)
    if result.returncode == 0:
        parts.append(result.stdout)
    return "\n".join(parts)


def _provision(root: Path, tenant: str, tenant_name: str, profile: str,
               password: str, super_email: str) -> subprocess.CompletedProcess:
    """Provision @p tenant the way the recipe does, without the compute packages.

    The recipe's own script also publishes the vendored ORE engine packages.
    A ticks check does not need a compute package, so it runs the bootstrap
    step alone and keeps the check to the path it measures.
    """
    script = (
        "connect $ORES_NATS_URL\n"
        f"bootstrap setup {SUPER_ADMIN} {password} {super_email} "
        f"--tenant-admin-password {password} "
        f"--tenant-code {tenant} --tenant-name \"{tenant_name}\" "
        f"--tenant-hostname {tenant} --profile {profile}\n"
        "logout\n")
    with tempfile.NamedTemporaryFile("w", suffix=".ores", delete=False) as handle:
        handle.write(script)
        path = handle.name
    try:
        return _run(["./compass.sh", "shell", "-l", path], root)
    finally:
        Path(path).unlink(missing_ok=True)


def _wait_evidence(root: Path, tenant: str, env: dict, start_epoch: float,
                   log_path: Path, timeout: int):
    """Poll for a tick and for a demand set that carries ids.

    A tick can land before the supervisor's next demand refresh, and a demand
    with no ids means no route reached the adapter. The check waits for both,
    so a healthy system is not failed for being a minute early.
    """
    deadline = time.monotonic() + timeout
    count, first, last, future = 0, "", "", 0
    lines: list[demand] = []
    while True:
        waited = int(timeout - max(deadline - time.monotonic(), 0))
        count, first, last, future = _tick_counts(root, tenant)
        text = _service_text(root, env, start_epoch, log_path)
        lines = latest_demand(parse_demand(text))
        has_demand = any(line.demanded > 0 for line in lines)
        if count and has_demand:
            return count, first, last, future, lines, waited
        if time.monotonic() >= deadline:
            return count, first, last, future, lines, timeout
        time.sleep(5)


def ticks(args, root: Path, env_file: Path | None = None) -> int:
    """compass smoke ticks — provision a non-system tenant and assert that it ticks."""
    import compass_db
    import compass_services

    env = compass_db.load_env(root, env_file)
    tenant = args.tenant_code
    log_path = _log_path(root, env)
    start_epoch = time.time()

    if args.fresh:
        # A live service holds connections to the database the recreate drops.
        # Stop them first, or the provisioning meets a service that lost its
        # store halfway through.
        _run(["./compass.sh", "services", "stop"], root)
        # A connection can outlive the stop by a moment, so the recreate
        # retries: the drop is refused while one is open.
        for attempt in range(3):
            result = _run(["./compass.sh", "db", "recreate", "-y", "-k"], root)
            if result.returncode == 0:
                break
            if attempt == 2:
                print(result.stdout, file=sys.stderr)
                print(result.stderr, file=sys.stderr)
                return 1
            time.sleep(10)
        args.start_services = True

    if args.start_services:
        result = _run(["./compass.sh", "services", "start"], root)
        if result.returncode != 0:
            print(result.stdout, file=sys.stderr)
            print(result.stderr, file=sys.stderr)
            return 1
        if not _wait_for_service(root, "ores.marketdata.service", args.startup_timeout):
            print("error: the marketdata service did not start", file=sys.stderr)
            return 1

    if not args.skip_provision:
        result = _provision(root, tenant, args.tenant_name, args.profile,
                            args.bootstrap_password, args.super_admin_email)
        if result.returncode != 0:
            print(result.stdout)
            print(result.stderr, file=sys.stderr)
            print("error: the provisioning step failed", file=sys.stderr)
            return 1

    subs, routes = _tenant_geometry(root, tenant)
    count, first, last, future, lines, waited = _wait_evidence(
        root, tenant, env, start_epoch, log_path, args.timeout)
    ev = evidence(
        tenant=tenant,
        routes=routes,
        subscriptions=subs,
        ticks=count,
        first_tick=first,
        last_tick=last,
        future_ticks=future,
        demand=lines,
        waited_seconds=waited,
        log_file=str(log_path),
    )
    print(f"tenant        : {ev.tenant}")
    print(f"subscriptions : {ev.subscriptions}")
    print(f"routes        : {ev.routes}")
    print(f"ticks         : {ev.ticks}")
    print(f"first tick    : {ev.first_tick or '-'}")
    print(f"last tick     : {ev.last_tick or '-'}")
    for line in ev.demand:
        print(f"adapter       : {line.adapter} accepted {line.accepted}, rejected "
              f"{line.rejected} of {line.demanded}")
    print(f"evidence      : {ev.log_file}")

    # The rest of the gate needs NATS up and the services down: the suites
    # share the bus with a live service. Stop what this check started unless
    # the caller means to keep it.
    if args.start_services and not args.leave_running:
        _run(["./compass.sh", "services", "stop"], root)

    reasons = evaluate(ev)
    if reasons:
        print("❌ ticks smoke failed:", file=sys.stderr)
        for reason in reasons:
            print(f"   - {reason}", file=sys.stderr)
        return 1
    print("✅ ticks smoke passed")
    return 0


def _wait_for_service(root: Path, name: str, timeout: int) -> bool:
    """Wait until @p name reports running, so the provisioning meets a live service."""
    import compass_services
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        status = _run(["./compass.sh", "services", "status", name], root, timeout=60)
        # The table row starts with the state and then the unit name; the footer
        # counts every state, so matching on the word alone would never pass.
        if re.search(r"^\s*running\s+" + re.escape(name), status.stdout, re.MULTILINE):
            return True
        time.sleep(3)
    return False


def run(argv: list[str], project_root: Path, env_file: Path | None = None) -> int:
    parser = argparse.ArgumentParser(prog="compass smoke")
    sub = parser.add_subparsers(dest="subcmd", required=True)
    ticks_p = sub.add_parser(
        "ticks",
        help="Provision a non-system tenant and assert that ticks arrive. "
             "Needs a database and the services; --fresh recreates the database "
             "and starts them.")
    ticks_p.add_argument("--fresh", action="store_true",
                         help="Recreate the database first (destructive) and start the services")
    ticks_p.add_argument("--start-services", action="store_true",
                         help="Start the services if they are stopped")
    ticks_p.add_argument("--skip-provision", action="store_true",
                         help="Use the tenant already in the database")
    ticks_p.add_argument("--tenant-code", default=DEFAULT_TENANT,
                         help=f"Tenant code to provision and watch (default: {DEFAULT_TENANT})")
    ticks_p.add_argument("--tenant-name", default=DEFAULT_TENANT_NAME,
                         help="Tenant display name")
    ticks_p.add_argument("--profile", default=DEFAULT_PROFILE,
                         help=f"Provisioning profile (default: {DEFAULT_PROFILE})")
    ticks_p.add_argument("--bootstrap-password", default=BOOTSTRAP_PASSWORD,
                         help="The bootstrap super administrator password")
    ticks_p.add_argument("--super-admin-email", default=SUPER_ADMIN_EMAIL,
                         help="The bootstrap super administrator email")
    ticks_p.add_argument("--leave-running", action="store_true",
                         help="Keep the services running after the check")
    ticks_p.add_argument("--timeout", type=int, default=420,
                         help="Seconds to wait for the first tick (default: 420)")
    ticks_p.add_argument("--startup-timeout", type=int, default=180,
                         help="Seconds to wait for the marketdata service (default: 180)")

    args = parser.parse_args(argv)
    if args.subcmd == "ticks":
        return ticks(args, project_root, env_file)
    return 2
