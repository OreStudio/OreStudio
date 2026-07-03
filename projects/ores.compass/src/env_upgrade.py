"""compass env upgrade — promote a light environment to a full (C++/vcpkg) environment."""

import argparse
import os
import subprocess
import sys
from pathlib import Path


def run(argv, checkout_root: Path) -> int:
    p = argparse.ArgumentParser(
        prog="compass env upgrade",
        description=(
            "Promote a light environment to a full (C++/vcpkg) environment.\n\n"
            "Patches ORES_PROVISION_TYPE from 'light' to 'full' and updates\n"
            "ORES_PRESET in .env (all secrets are preserved), then initialises\n"
            "the vcpkg submodule and runs cmake configure (which triggers\n"
            "vcpkg install via the toolchain file)."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    p.add_argument(
        "--preset",
        help="Build preset to activate (e.g. linux-clang-debug-ninja). Required.",
    )
    p.add_argument("-y", "--yes", action="store_true",
                   help="Skip confirmation prompt")
    args = p.parse_args(argv)

    env_file = checkout_root / ".env"
    if not env_file.is_file():
        print("Error: .env not found. Run 'compass env configure' first.", file=sys.stderr)
        return 1

    # Parse current .env into a dict for inspection
    raw_lines = env_file.read_text(encoding="utf-8").splitlines(keepends=True)
    existing: dict[str, str] = {}
    for line in raw_lines:
        stripped = line.strip()
        if stripped and not stripped.startswith("#") and "=" in stripped:
            k, _, v = stripped.partition("=")
            existing[k.strip()] = v.strip()

    current_type = existing.get("ORES_PROVISION_TYPE", "")
    current_preset = existing.get("ORES_PRESET", "")

    if current_type == "full":
        print("Already a full environment (ORES_PROVISION_TYPE=full). Nothing to do.")
        return 0
    if current_type != "light":
        print(
            f"Error: ORES_PROVISION_TYPE={current_type!r} — expected 'light'.",
            file=sys.stderr,
        )
        return 1

    # Resolve preset
    preset = args.preset
    if not preset and not os.environ.get("CI"):
        print(
            "Error: --preset is required.\n"
            "  Example: compass env upgrade --preset linux-clang-debug-ninja",
            file=sys.stderr,
        )
        return 1
    if not preset:
        preset = current_preset  # CI fallback: keep whatever is already set

    env_name = existing.get("ORES_ENV_NAME", checkout_root.name)

    # Promotion to full pulls in vcpkg and a build tree — guard against a
    # nearly-full disk before committing to the operation.
    from disk_guard import check_disk_space
    if not check_disk_space(checkout_root, assume_yes=args.yes):
        return 1

    if not args.yes:
        print(f"Upgrade '{env_name}' from light → full?\n")
        print(f"  ORES_PROVISION_TYPE : light  →  full")
        print(f"  ORES_PRESET         : {current_preset or '(unset)'}  →  {preset}")
        print(f"\n  Steps:")
        print(f"    1. Patch .env in-place (secrets preserved)")
        print(f"    2. git submodule update --init vcpkg")
        print(f"    3. cmake --preset {preset}  (triggers vcpkg install)")
        ans = input("\nProceed? [y/N] ").strip().lower()
        if ans != "y":
            print("Aborted.")
            return 0

    # -------------------------------------------------------------------------
    # Patch .env: replace ORES_PROVISION_TYPE and ORES_PRESET lines in-place.
    # All other content (passwords, NATS config, IAM key) is preserved exactly.
    # -------------------------------------------------------------------------
    new_lines: list[str] = []
    preset_written = False
    for line in raw_lines:
        key = line.partition("=")[0].strip()
        if key == "ORES_PROVISION_TYPE":
            new_lines.append("ORES_PROVISION_TYPE=full\n")
        elif key == "ORES_PRESET":
            new_lines.append(f"ORES_PRESET={preset}\n")
            preset_written = True
        else:
            new_lines.append(line if line.endswith("\n") else line + "\n")

    if not preset_written:
        # ORES_PRESET was absent — insert it on the line after ORES_PROVISION_TYPE
        for i, line in enumerate(new_lines):
            if line.startswith("ORES_PROVISION_TYPE="):
                new_lines.insert(i + 1, f"ORES_PRESET={preset}\n")
                break

    # Write to a temp file first, then atomically replace — .env is never absent
    # even if the process is killed mid-write.
    old_content = "".join(raw_lines)
    tmp = env_file.with_name(".env.new")
    tmp.write_text("".join(new_lines), encoding="utf-8")
    tmp.chmod(0o600)
    tmp.replace(env_file)
    # Backup written after the atomic replace; losing it is acceptable.
    (checkout_root / ".env.old").write_text(old_content, encoding="utf-8")
    print(f"✅ .env updated  (ORES_PROVISION_TYPE=full, ORES_PRESET={preset})")

    # -------------------------------------------------------------------------
    # Initialise vcpkg submodule
    # -------------------------------------------------------------------------
    print("\n🔧 Initialising vcpkg submodule…")
    rc = subprocess.run(
        ["git", "submodule", "update", "--init", "vcpkg"],
        cwd=str(checkout_root),
    ).returncode
    if rc != 0:
        print("❌ vcpkg submodule init failed.", file=sys.stderr)
        return rc
    print("✅ vcpkg submodule ready")

    # -------------------------------------------------------------------------
    # cmake configure — triggers vcpkg install via the toolchain file
    # -------------------------------------------------------------------------
    print(f"\n🔨 cmake --preset {preset}")
    print("   (vcpkg install will run as part of this step — may take a while)\n")
    rc = subprocess.run(
        ["cmake", "--preset", preset],
        cwd=str(checkout_root),
    ).returncode
    if rc != 0:
        print("\n❌ cmake configure failed.", file=sys.stderr)
        return rc

    print(f"\n✅ Environment '{env_name}' upgraded to full.")
    print(f"   Build:     compass build")
    print(f"   Or direct: cmake --build --preset {preset}")
    return 0
