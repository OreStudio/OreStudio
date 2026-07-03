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


from env_init import _scan_ports


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
                   help="Provisioning size (default: full)")
    p.add_argument("--preset",
                   help="Build preset to pass to compass env configure. "
                        "When given, configure runs immediately after provisioning.")
    p.add_argument("-y", "--yes", action="store_true",
                   help="Pass -y to compass env configure (skip overwrite prompt).")
    pkg_grp = p.add_mutually_exclusive_group()
    pkg_grp.add_argument("--install-packages", action="store_true",
                         help="Pass --install-packages to compass env configure.")
    pkg_grp.add_argument("--skip-packages", action="store_true",
                         help="Pass --skip-packages to compass env configure.")
    p.add_argument("-n", "--dry-run", action="store_true",
                   help="Show what would be provisioned (including the "
                        "disk-space check) without creating anything.")
    p.add_argument("--min-free-gb", type=int, default=None,
                   help="Override the low-disk-space warning threshold in GiB "
                        "(default 20). Useful for testing the warning.")
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

    # A full worktree pulls in vcpkg and a build tree — guard against a
    # nearly-full disk before creating it. Light worktrees are cheap, so only
    # the full path is gated.
    if args.env_type == "full":
        from disk_guard import check_disk_space, MIN_FREE_GB
        min_free = args.min_free_gb if args.min_free_gb is not None else MIN_FREE_GB
        if not check_disk_space(parent_dir, assume_yes=args.yes or args.dry_run,
                                min_free_gb=min_free):
            return 1

    # Assign ports.
    base_port, nats_port, nats_monitor_port = _scan_ports(parent_dir)

    if args.dry_run:
        print(f"[dry-run] Would provision '{name}' ({args.env_type}):")
        print(f"[dry-run]   Path      : {worktree_dir}")
        print(f"[dry-run]   Base port : {base_port}")
        print(f"[dry-run]   NATS port : {nats_port} (monitor {nats_monitor_port})")
        print(f"[dry-run]   git worktree add {worktree_dir} origin/main")
        if args.env_type == "full":
            print("[dry-run]   then: git submodule update --init vcpkg")
        if args.preset:
            print(f"[dry-run]   then: compass env configure --preset {args.preset}")
        print("[dry-run] No changes made.")
        return 0

    # Create git worktree off origin/main.
    print(f"Provisioning worktree: {worktree_dir}")
    result = subprocess.run(
        ["git", "worktree", "add", str(worktree_dir), "origin/main"],
        cwd=str(project_root),
    )
    if result.returncode != 0:
        print("❌ Failed to create git worktree.", file=sys.stderr)
        return 1

    # Full environments need the vcpkg submodule so cmake can run immediately
    # after configure.  Git worktrees do not auto-initialize submodules.
    if args.env_type == "full":
        print("  Initialising vcpkg submodule...")
        sub = subprocess.run(
            ["git", "submodule", "update", "--init", "vcpkg"],
            cwd=str(worktree_dir),
        )
        if sub.returncode != 0:
            print("⚠️  vcpkg submodule init failed — cmake configure will need it later.",
                  file=sys.stderr)

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
        f"ORES_PROVISION_TYPE={args.env_type}\n"
        f"ORES_BASE_PORT={base_port}\n"
        f"ORES_NATS_PORT={nats_port}\n"
        f"ORES_NATS_MONITOR_PORT={nats_monitor_port}\n"
    )
    env_file.chmod(0o600)
    print(f"  Wrote skeleton .env to {env_file}")

    # Build skills and settings from their org sources into the new worktree.
    # --direct bypasses cmake/vcpkg so this works for both light and full envs.
    # --no-deps skips the pip round-trip since only stdlib + emacs are needed.
    compass_sh = worktree_dir / "projects" / "ores.compass" / "compass.sh"
    if not compass_sh.is_file():
        print("⚠️  compass.sh not found in new worktree — run 'compass build --direct skills settings' manually.",
              file=sys.stderr)
    else:
        print("  Deploying skills and settings...")
        build = subprocess.run(
            ["bash", str(compass_sh), "--no-deps", "build", "--direct", "skills", "settings"],
            cwd=str(worktree_dir),
        )
        if build.returncode != 0:
            print("⚠️  Skills/settings build failed — run 'compass build --direct skills settings' manually.",
                  file=sys.stderr)

    print(f"\n✅ Environment '{name}' provisioned")
    print(f"   Path:      {worktree_dir}")
    print(f"   Provision: {args.env_type}")
    print(f"   Base port: {base_port}")
    print(f"   NATS port: {nats_port}")

    # Optionally run compass env configure in the new worktree.
    if args.preset:
        print(f"\nRunning compass env configure --preset {args.preset}...")
        import env_init
        configure_argv = ["--preset", args.preset]
        if args.yes:
            configure_argv.append("-y")
        if args.install_packages:
            configure_argv.append("--install-packages")
        if args.skip_packages:
            configure_argv.append("--skip-packages")
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
        # Fallback: remove the directory first, then prune so git can see it is
        # gone and clean up the stale .git/worktrees/<name> registration.
        if worktree_dir.exists():
            import shutil
            shutil.rmtree(str(worktree_dir))
            print(f"  Removed directory (forced): {worktree_dir}")
        subprocess.run(["git", "worktree", "prune"], cwd=str(project_root))

    print(f"\n✅ Environment '{name}' deprovisioned.")
    return 0
