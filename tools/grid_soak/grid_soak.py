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

The load alternates between two phases. A quiet phase sends small waves a
little apart, and a burst phase sends large waves close together. How long a
phase lasts, how big a wave is and how long the pause before the next one is
are all drawn at random, and now and then a spike wave is several times larger
than a burst. The aim is a load curve with the shape of real use, so the grid
screen has something to show: the queue climbing in a burst, draining in a
quiet stretch, and every node taking its share.

Each wave is one ores.shell session that adds a batch and dispatches its jobs
to the app version. The run stops on Ctrl-C or SIGTERM, after the wave in
flight, and prints what it sent.

Usage, from the repository root:

    python3 tools/grid_soak/grid_soak.py
    python3 tools/grid_soak/grid_soak.py --seed 7 --burst-jobs 500 4000

The login is the super administrator's, read from ORES_SHELL_LOGIN_PASSWORD in
.env unless --password is given.
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

DEFAULT_APP_VERSION = "77a3e12f-5202-4216-822f-398dcd7b18d1"
DEFAULT_PACKAGE = "external/ore/packages/TA002_IR_Swap.tar.gz"

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
    jobs: tuple[int, int]
    gap: tuple[float, float]
    waves: tuple[int, int]


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


def send_wave(root, args, reference, count):
    """Run one wave through compass shell. Returns True when the shell exited cleanly."""
    script = wave_script(
        args.principal, args.password, reference, count, args.app_version, args.package
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


def sleep_unless_stopped(seconds):
    """Sleep in short steps so a stop is noticed within a second."""
    deadline = time.monotonic() + seconds
    while not stopping and time.monotonic() < deadline:
        time.sleep(min(1.0, max(0.0, deadline - time.monotonic())))


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--seed", type=int, default=None, help="Seed the random draws, for a repeatable run")
    ap.add_argument("--quiet-jobs", type=int, nargs=2, default=(5, 40), metavar=("MIN", "MAX"))
    ap.add_argument("--burst-jobs", type=int, nargs=2, default=(400, 3000), metavar=("MIN", "MAX"))
    ap.add_argument("--quiet-gap", type=float, nargs=2, default=(20, 60), metavar=("MIN", "MAX"),
                    help="Seconds between waves in a quiet phase")
    ap.add_argument("--burst-gap", type=float, nargs=2, default=(5, 20), metavar=("MIN", "MAX"),
                    help="Seconds between waves in a burst phase")
    ap.add_argument("--quiet-waves", type=int, nargs=2, default=(2, 5), metavar=("MIN", "MAX"))
    ap.add_argument("--burst-waves", type=int, nargs=2, default=(2, 5), metavar=("MIN", "MAX"))
    ap.add_argument("--burst-share", type=float, default=0.5,
                    help="Chance that the phase after one is a burst")
    ap.add_argument("--spike-chance", type=float, default=0.08,
                    help="Chance that a burst wave is a spike, several times larger")
    ap.add_argument("--app-version", default=DEFAULT_APP_VERSION)
    ap.add_argument("--package", default=DEFAULT_PACKAGE)
    ap.add_argument("--principal", default="super_admin@localhost")
    ap.add_argument("--password", default=None)
    ap.add_argument("--dry-run", action="store_true", help="Draw the load and print it, send nothing")
    args = ap.parse_args(argv)

    root = Path(__file__).resolve().parents[2]
    args.package = str((root / args.package).resolve()) if not Path(args.package).is_absolute() else args.package
    if args.password is None:
        args.password = env_value(root, "ORES_SHELL_LOGIN_PASSWORD")
    if args.password is None:
        ap.error("no password: set ORES_SHELL_LOGIN_PASSWORD in .env or pass --password")

    seed = args.seed if args.seed is not None else random.SystemRandom().randrange(1 << 32)
    rng = random.Random(seed)
    phases = (
        Phase("quiet", tuple(args.quiet_jobs), tuple(args.quiet_gap), tuple(args.quiet_waves)),
        Phase("burst", tuple(args.burst_jobs), tuple(args.burst_gap), tuple(args.burst_waves)),
    )
    run_id = time.strftime("%m%d%H%M%S")
    signal.signal(signal.SIGINT, request_stop)
    signal.signal(signal.SIGTERM, request_stop)

    print(f"[soak] run {run_id} seed {seed}, Ctrl-C or a stop request ends it", flush=True)
    sent_jobs = 0
    sent_waves = 0
    failed_waves = 0
    phase = None
    while not stopping:
        phase = pick_phase(rng, phases, args.burst_share, phase)
        waves = rng.randint(*phase.waves)
        print(f"[soak] {phase.name} phase of {waves} wave(s)", flush=True)
        for _ in range(waves):
            if stopping:
                break
            count = rng.randint(*phase.jobs)
            if phase.name == "burst" and rng.random() < args.spike_chance:
                count *= rng.randint(2, 4)
                label = "spike"
            else:
                label = phase.name
            sent_waves += 1
            reference = f"soak-{run_id}-{sent_waves}"
            print(f"[soak] wave {sent_waves} {label}: {count} job(s) as {reference}", flush=True)
            if send_wave(root, args, reference, count):
                sent_jobs += count
            else:
                failed_waves += 1
                print(f"[soak] wave {sent_waves} failed", flush=True)
            sleep_unless_stopped(rng.uniform(*phase.gap))

    print(
        f"[soak] stopped: {sent_waves} wave(s), {sent_jobs} job(s) sent, {failed_waves} failed",
        flush=True,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
