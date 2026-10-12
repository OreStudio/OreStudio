#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""Load the compute grid with random bursts of jobs until it is stopped.

The load is a share of what the grid can do. --capacity says how many jobs a
minute the grid finishes, and every phase draws its rate as a fraction of that.
A quiet phase runs at a fraction of the capacity. A burst phase runs above it,
so the queue climbs, and now and then a spike runs well above it. How long a
phase lasts, how long the pause before the next wave is and which principal
sends a wave are drawn at random. The aim is a load curve with the shape of
real use: the queue climbing in a burst, draining in a quiet stretch, and every
node taking its share.

The driver keeps its own estimate of the queue, which is the jobs it sent minus
what the capacity could finish since. A wave that would push that estimate past
--max-queue-minutes of work is held back. A job that waits longer than the
storage token lives fails, so the default keeps the wait well inside an hour.

Each wave is one ores.shell session that adds a batch and dispatches its jobs
to the app version. The run stops on Ctrl-C or SIGTERM, after the wave in
flight, and prints what it sent.

Usage, from the repository root:

    python3 tools/grid_soak/grid_soak.py --app-version <id of the engine version>
    python3 tools/grid_soak/grid_soak.py --capacity 90 --seed 7

The app version changes when the database is recreated. Each wave is sent by a
principal drawn by weight, so the work lands in several tenants and the grid's
usage screen has groups to show. Name a principal as NAME or NAME:WEIGHT. The
password is read from ORES_SHELL_LOGIN_PASSWORD in .env unless --password is
given, and it must fit every principal.
"""

import argparse
import os
import random
import signal
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path

DEFAULT_APP_VERSION = "87cce313-e356-41c7-a02b-727c15f1d819"
DEFAULT_PACKAGE = "external/ore/packages/TA002_IR_Swap.tar.gz"
DEFAULT_PRINCIPALS = [
    "tenant_admin@acme_corporation:6",
    "tenant_admin@barclays_plc:3",
    "super_admin@localhost:1",
]

# The jobs a minute the grid finished in a measured soak of 22 nodes.
DEFAULT_CAPACITY = 45

stopping = False


def request_stop(signum, _frame):
    """Finish the wave in flight, then stop."""
    global stopping
    stopping = True
    print(f"[soak] signal {signum}: stopping after the wave in flight", flush=True)


@dataclass(frozen=True)
class Phase:
    """The parameters of one kind of phase."""

    name: str
    load: tuple[float, float]
    gap: tuple[float, float]
    minutes: tuple[float, float]


def env_value(root: Path, key: str) -> str | None:
    """A value from the process environment, or from .env at the root."""
    if key in os.environ:
        return os.environ[key]
    env_file = root / ".env"
    if env_file.is_file():
        for line in env_file.read_text(encoding="utf-8").splitlines():
            if line.startswith(f"{key}="):
                return line.split("=", 1)[1].strip().strip('"')
    return None


def parse_principals(specs):
    """The principals and their weights, from NAME or NAME:WEIGHT."""
    names, weights = [], []
    for spec in specs:
        name, _, weight = spec.partition(":")
        names.append(name)
        weights.append(float(weight) if weight else 1.0)
    return names, weights


def wave_script(principal, password, reference, count, app_version, package):
    """The ores.shell script of one wave."""
    return "\n".join(
        [
            "connect $ORES_NATS_URL",
            f"login {principal} {password}",
            f'batches add {reference} open system.new_record "grid soak"',
            f"compute dispatch-batch {reference} {count} {app_version} {package}",
            "logout",
            "exit",
            "",
        ]
    )


def send_wave(root, args, rng, reference, count):
    """Run one wave through compass shell. Returns True when the shell exited cleanly."""
    names, weights = args.principals
    principal = rng.choices(names, weights)[0]
    print(f"[soak] {reference} sent by {principal}", flush=True)
    script = wave_script(
        principal, args.password, reference, count, args.app_version, args.package
    )
    with tempfile.NamedTemporaryFile("w", suffix=".ores", delete=False) as handle:
        handle.write(script)
        path = handle.name
    try:
        if args.dry_run:
            return True
        # The shell's own output is left alone: the driver does not redirect it.
        return subprocess.run([str(root / "compass.sh"), "shell", "-f", path], check=False).returncode == 0
    finally:
        os.unlink(path)


def pick_phase(rng, phases, burst_share, previous):
    """The next phase: a burst with the given probability, otherwise quiet."""
    quiet, burst = phases
    chosen = burst if rng.random() < burst_share else quiet
    # Two bursts in a row are fine, but never open with one.
    return quiet if previous is None else chosen


def wave_size(capacity, load, gap_seconds):
    """The jobs that make a rate of load times capacity over one gap."""
    return max(1, round(capacity * load * gap_seconds / 60.0))


def drained(queue, capacity, seconds):
    """The queue estimate after the grid has worked for some seconds."""
    return max(0.0, queue - capacity * seconds / 60.0)


def sleep_unless_stopped(seconds):
    """Sleep in short steps so a stop is noticed within a second."""
    deadline = time.monotonic() + seconds
    while not stopping and time.monotonic() < deadline:
        time.sleep(min(1.0, max(0.0, deadline - time.monotonic())))


def build_parser():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--seed", type=int, default=None, help="Seed the random draws, for a repeatable run")
    ap.add_argument("--capacity", type=float, default=DEFAULT_CAPACITY,
                    help="Jobs a minute the grid finishes; every rate is a share of it")
    ap.add_argument("--quiet-load", type=float, nargs=2, default=(0.2, 0.5), metavar=("MIN", "MAX"),
                    help="Share of the capacity sent in a quiet phase")
    ap.add_argument("--burst-load", type=float, nargs=2, default=(1.2, 2.0), metavar=("MIN", "MAX"),
                    help="Share of the capacity sent in a burst phase")
    ap.add_argument("--spike-load", type=float, nargs=2, default=(2.5, 3.0), metavar=("MIN", "MAX"),
                    help="Share of the capacity sent in a spike wave")
    ap.add_argument("--quiet-minutes", type=float, nargs=2, default=(3, 8), metavar=("MIN", "MAX"))
    ap.add_argument("--burst-minutes", type=float, nargs=2, default=(2, 4), metavar=("MIN", "MAX"))
    ap.add_argument("--quiet-gap", type=float, nargs=2, default=(30, 60), metavar=("MIN", "MAX"),
                    help="Seconds between waves in a quiet phase")
    ap.add_argument("--burst-gap", type=float, nargs=2, default=(15, 30), metavar=("MIN", "MAX"),
                    help="Seconds between waves in a burst phase")
    ap.add_argument("--burst-share", type=float, default=0.4,
                    help="Chance that the phase after one is a burst")
    ap.add_argument("--spike-chance", type=float, default=0.05,
                    help="Chance that a burst wave is a spike")
    ap.add_argument("--max-queue-minutes", type=float, default=20,
                    help="Hold a wave back that would queue more than this much work")
    ap.add_argument("--app-version", default=DEFAULT_APP_VERSION)
    ap.add_argument("--package", default=DEFAULT_PACKAGE)
    ap.add_argument("--principal", action="append", metavar="NAME[:WEIGHT]",
                    help="Who sends a wave; repeat it. Default: the Acme and Barclays tenant "
                         "administrators and the super administrator, weighted 6, 3 and 1")
    ap.add_argument("--password", default=None)
    ap.add_argument("--dry-run", action="store_true", help="Draw the load and print it, send nothing")
    return ap


def main(argv):
    ap = build_parser()
    args = ap.parse_args(argv)
    args.principals = parse_principals(args.principal or DEFAULT_PRINCIPALS)

    root = Path(__file__).resolve().parents[2]
    args.package = str((root / args.package).resolve()) if not Path(args.package).is_absolute() else args.package
    if args.password is None:
        args.password = env_value(root, "ORES_SHELL_LOGIN_PASSWORD")
    if args.password is None:
        ap.error("no password: set ORES_SHELL_LOGIN_PASSWORD in .env or pass --password")

    seed = args.seed if args.seed is not None else random.SystemRandom().randrange(1 << 32)
    rng = random.Random(seed)
    phases = (
        Phase("quiet", tuple(args.quiet_load), tuple(args.quiet_gap), tuple(args.quiet_minutes)),
        Phase("burst", tuple(args.burst_load), tuple(args.burst_gap), tuple(args.burst_minutes)),
    )
    run_id = time.strftime("%m%d%H%M%S")
    signal.signal(signal.SIGINT, request_stop)
    signal.signal(signal.SIGTERM, request_stop)

    print(
        f"[soak] run {run_id} seed {seed}, capacity {args.capacity:g} jobs a minute, "
        "Ctrl-C or a stop request ends it",
        flush=True,
    )
    sent_jobs = 0
    sent_waves = 0
    failed_waves = 0
    held_waves = 0
    queue = 0.0
    last = time.monotonic()
    phase = None
    while not stopping:
        phase = pick_phase(rng, phases, args.burst_share, phase)
        minutes = rng.uniform(*phase.minutes)
        print(f"[soak] {phase.name} phase of {minutes:.1f} minute(s)", flush=True)
        phase_end = time.monotonic() + minutes * 60
        while not stopping and time.monotonic() < phase_end:
            gap = rng.uniform(*phase.gap)
            load = rng.uniform(*phase.load)
            label = phase.name
            if phase.name == "burst" and rng.random() < args.spike_chance:
                load = rng.uniform(*args.spike_load)
                label = "spike"
            count = wave_size(args.capacity, load, gap)

            now = time.monotonic()
            queue = drained(queue, args.capacity, now - last)
            last = now
            if queue + count > args.capacity * args.max_queue_minutes:
                held_waves += 1
                print(
                    f"[soak] holding back {count} job(s): about {queue / args.capacity:.0f} "
                    f"minute(s) of work is queued already",
                    flush=True,
                )
            else:
                sent_waves += 1
                reference = f"soak-{run_id}-{sent_waves}"
                print(
                    f"[soak] wave {sent_waves} {label}: {count} job(s) at {load:.1f}x capacity "
                    f"as {reference}",
                    flush=True,
                )
                if send_wave(root, args, rng, reference, count):
                    sent_jobs += count
                    queue += count
                else:
                    failed_waves += 1
                    print(f"[soak] wave {sent_waves} failed", flush=True)
            sleep_unless_stopped(gap)

    print(
        f"[soak] stopped: {sent_waves} wave(s), {sent_jobs} job(s) sent, "
        f"{held_waves} held back, {failed_waves} failed",
        flush=True,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
