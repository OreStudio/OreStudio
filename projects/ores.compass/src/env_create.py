"""compass env provision / deprovision — worktree lifecycle management.

provision:   Creates a git worktree at ../ores_dev_<name>, assigns isolated
             ports, writes a skeleton .env, and optionally runs env configure.
deprovision: Removes the git worktree and its directory.

Directory naming: new environments use ores_dev_<name_underscored>
(e.g. ores_dev_festive_hawking).  Legacy OreStudio.* worktrees are
recognised in port-scanning for backwards compatibility.
"""

import argparse
import re
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

# Embedded adjective-noun word lists (GCP / GitHub style).
_ADJECTIVES = [
    "brave", "bright", "calm", "clever", "daring", "eager", "festive",
    "gentle", "happy", "jolly", "kind", "lively", "merry", "noble", "plucky",
    "proud", "quiet", "robust", "serene", "sharp", "smart", "solid", "steady",
    "strong", "sunny", "swift", "tender", "vibrant", "warm", "witty",
]
_NOUNS = [
    "aho", "babbage", "bohr", "curie", "darwin", "dijkstra", "dirac",
    "einstein", "euler", "faraday", "feynman", "gauss", "hawking", "hopper",
    "kay", "kernighan", "knuth", "lamport", "liskov", "lovelace", "maxwell",
    "newton", "nygaard", "planck", "ritchie", "tesla", "turing", "ullman",
    "wirth",
]

_NAME_RE = re.compile(r"^[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)+$")


def _name_to_dir(name: str) -> str:
    """Convert adjective-noun name to directory basename: festive-hawking → ores_dev_festive_hawking."""
    return "ores_dev_" + name.lower().replace("-", "_")


def _generate_name(parent_dir: Path) -> str:
    """Return an unused adjective-noun name, e.g. 'festive-hawking'."""
    import random
    existing_dirs = {d.name for d in parent_dir.iterdir() if d.is_dir()}
    for _ in range(100):
        name = f"{random.choice(_ADJECTIVES)}-{random.choice(_NOUNS)}"
        if _name_to_dir(name) not in existing_dirs:
            return name
    raise RuntimeError("Could not generate a unique environment name after 100 tries")


def _scan_ports(parent_dir: Path) -> tuple[int, int, int]:
    """Scan sibling worktrees for used port values; returns (base_port, nats_port, nats_monitor_port).

    Scans both new-style ores_dev_* and legacy OreStudio.* directories.
    """
    from env_init import _read_env
    used_base: set[int] = set()
    used_nats: set[int] = set()
    patterns = ["ores_dev_*/.env", "OreStudio.*/.env"]
    for pattern in patterns:
        for env_file in parent_dir.glob(pattern):
            d = _read_env(env_file)
            if d.get("ORES_BASE_PORT"):
                try:
                    used_base.add(int(d["ORES_BASE_PORT"]))
                except ValueError:
                    pass
            if d.get("ORES_NATS_PORT"):
                try:
                    used_nats.add(int(d["ORES_NATS_PORT"]))
                except ValueError:
                    pass

    base_port = 50000
    while base_port in used_base:
        base_port += 1000

    nats_port = 42221
    while nats_port in used_nats:
        nats_port += 1
    nats_monitor_port = 8221 + (nats_port - 42221)

    return base_port, nats_port, nats_monitor_port


def run_provision(argv: list[str], project_root: Path) -> int:
    """compass env provision — provision a new named worktree."""
    p = argparse.ArgumentParser(
        prog="compass env provision",
        description="Provision a new named worktree. "
                    "Creates ../ores_dev_<name>, assigns isolated ports, writes a .env "
                    "skeleton, and optionally runs compass env configure.")
    p.add_argument("name", nargs="?",
                   help="Environment name (adjective-noun, e.g. festive-hawking). "
                        "Generated automatically if omitted.")
    p.add_argument("--type", choices=["light", "full"], default="full",
                   dest="env_type",
                   help="Environment type (default: full)")
    p.add_argument("--preset",
                   help="Build preset to pass to compass env configure. "
                        "When given, configure runs immediately after provisioning.")
    p.add_argument("-y", "--yes", action="store_true",
                   help="Pass -y to compass env configure (skip overwrite prompt).")
    args = p.parse_args(argv)

    parent_dir = project_root.parent

    # Resolve / validate name.
    name = args.name
    if not name:
        name = _generate_name(parent_dir)
        print(f"Generated name: {name}")
    elif not _NAME_RE.fullmatch(name):
        print(f"❌ Invalid name '{name}': must be lowercase adjective-noun "
              f"(letters/digits separated by one or more hyphens, e.g. festive-hawking).",
              file=sys.stderr)
        return 1

    worktree_dir = parent_dir / _name_to_dir(name)
    if worktree_dir.exists():
        print(f"❌ Directory already exists: {worktree_dir}", file=sys.stderr)
        return 1

    # Assign ports.
    base_port, nats_port, nats_monitor_port = _scan_ports(parent_dir)

    # Create git worktree off origin/main.
    print(f"Provisioning worktree: {worktree_dir}")
    result = subprocess.run(
        ["git", "worktree", "add", str(worktree_dir), "origin/main"],
        cwd=str(project_root),
    )
    if result.returncode != 0:
        print("❌ Failed to create git worktree.", file=sys.stderr)
        return 1

    # Write skeleton .env so compass env configure picks up the pre-assigned values.
    env_file = worktree_dir / ".env"
    ts = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC")
    env_file.write_text(
        f"# ORE Studio Environment: {name}\n"
        f"# Provisioned by compass env provision on {ts}\n"
        f"# Run 'compass env configure --preset <preset>' to complete setup.\n"
        f"# DO NOT COMMIT — this file contains secrets.\n"
        f"\n"
        f"ORES_ENV_NAME={name}\n"
        f"ORES_ENV_TYPE={args.env_type}\n"
        f"ORES_BASE_PORT={base_port}\n"
        f"ORES_NATS_PORT={nats_port}\n"
        f"ORES_NATS_MONITOR_PORT={nats_monitor_port}\n"
    )
    env_file.chmod(0o600)
    print(f"  Wrote skeleton .env to {env_file}")

    print(f"\n✅ Environment '{name}' provisioned")
    print(f"   Path:      {worktree_dir}")
    print(f"   Type:      {args.env_type}")
    print(f"   Base port: {base_port}")
    print(f"   NATS port: {nats_port}")

    # Optionally run compass env configure in the new worktree.
    if args.preset:
        print(f"\nRunning compass env configure --preset {args.preset}...")
        import env_init
        configure_argv = ["--preset", args.preset]
        if args.yes:
            configure_argv.append("-y")
        return env_init.run(configure_argv, worktree_dir)

    print(f"\nNext: cd {worktree_dir} && ./projects/ores.compass/compass.sh "
          f"env configure --preset <preset>")
    return 0


def run_deprovision(argv: list[str], project_root: Path) -> int:
    """compass env deprovision <name> — remove a named worktree."""
    p = argparse.ArgumentParser(
        prog="compass env deprovision",
        description="Deprovision a named worktree: removes the git worktree and its directory.")
    p.add_argument("name",
                   help="Environment name to remove (e.g. festive-hawking).")
    p.add_argument("-y", "--yes", action="store_true",
                   help="Skip the confirmation prompt.")
    args = p.parse_args(argv)

    name = args.name
    parent_dir = project_root.parent
    worktree_dir = parent_dir / _name_to_dir(name)

    # Also check legacy OreStudio.* naming for backwards compatibility.
    if not worktree_dir.exists():
        legacy_dir = parent_dir / f"OreStudio.{name}"
        if legacy_dir.exists():
            worktree_dir = legacy_dir
        else:
            print(f"❌ Worktree not found: {worktree_dir} (also checked {legacy_dir})",
                  file=sys.stderr)
            return 1

    # Safety: refuse to remove the current worktree.
    if worktree_dir.resolve() == project_root.resolve():
        print("❌ Cannot deprovision the current worktree.", file=sys.stderr)
        return 1

    if not args.yes:
        ans = input(f"Deprovision '{name}' at {worktree_dir}? "
                    f"This will delete all uncommitted changes. [y/N] ")
        if ans not in ("y", "Y"):
            print("Aborted.")
            return 1

    print(f"Removing worktree: {worktree_dir}")
    result = subprocess.run(
        ["git", "worktree", "remove", "--force", str(worktree_dir)],
        cwd=str(project_root),
    )
    if result.returncode != 0:
        # Fallback: prune stale references, then remove the directory.
        subprocess.run(["git", "worktree", "prune"], cwd=str(project_root))
        if worktree_dir.exists():
            import shutil
            shutil.rmtree(str(worktree_dir))
            print(f"  Removed directory (forced): {worktree_dir}")

    print(f"\n✅ Environment '{name}' deprovisioned.")
    return 0
