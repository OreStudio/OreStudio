"""compass nats init — Generate per-environment NATS server config.

Reads build/config/nats.conf.template, substitutes environment-specific
values, writes build/config/nats-<label>.conf, and creates the JetStream
store directory.

All values are read from the checkout .env when run standalone.
env_init calls generate() directly with the already-resolved values.
"""

import argparse
import os
from pathlib import Path


def generate(checkout_root: Path, label: str, nats_port: int,
             nats_monitor_port: int, nats_store_dir: Path) -> None:
    """Render the NATS config template and create the JetStream store dir."""
    template_file = checkout_root / "build" / "config" / "nats.conf.template"
    if not template_file.is_file():
        raise FileNotFoundError(f"NATS config template not found: {template_file}")

    config_out = checkout_root / "build" / "config" / f"nats-{label}.conf"
    config_out.parent.mkdir(parents=True, exist_ok=True)

    # Use forward slashes in paths embedded in the config — NATS server
    # parses the file on any platform and does not accept backslashes.
    def fwd(p):
        return str(p).replace("\\", "/")

    text = template_file.read_text()
    text = (text
            .replace("{{LABEL}}", label)
            .replace("{{NATS_PORT}}", str(nats_port))
            .replace("{{NATS_MONITOR_PORT}}", str(nats_monitor_port))
            .replace("{{NATS_STORE_DIR}}", fwd(nats_store_dir))
            .replace("{{CHECKOUT_ROOT}}", fwd(checkout_root)))
    config_out.write_text(text)
    print(f"  Written: {config_out}")

    nats_store_dir.mkdir(parents=True, exist_ok=True)
    print(f"  Ready: {nats_store_dir}")


def _load_dotenv(env_file: Path) -> dict:
    env = {}
    if not env_file.is_file():
        return env
    for line in env_file.read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, val = line.partition("=")
        env[key.strip()] = val.strip()
    return env


def run(argv, project_root: Path) -> int:
    argparse.ArgumentParser(
        prog="compass nats init",
        description="Generate per-environment NATS server config and store directory."
    ).parse_args(argv)

    env = _load_dotenv(project_root / ".env")
    env.update(os.environ)  # shell env takes precedence

    label = env.get("ORES_CHECKOUT_LABEL", project_root.name)
    nats_port = int(env.get("ORES_NATS_PORT", "4222"))
    nats_monitor_port = int(env.get("ORES_NATS_MONITOR_PORT", "8222"))
    nats_store_dir = Path(env.get(
        "ORES_NATS_STORE_DIR",
        str(project_root / "build" / "nats" / label / "jetstream")))

    print(f"=== NATS init for environment '{label}' ===")
    print(f"  Port:         {nats_port}")
    print(f"  Monitor port: {nats_monitor_port}")
    print(f"  Store dir:    {nats_store_dir}")
    print()
    generate(project_root, label, nats_port, nats_monitor_port, nats_store_dir)
    print(f"\n=== NATS init complete for '{label}' ===")
    print(f"\nNext: start nats-server with:")
    print(f"  nats-server --config build/config/nats-{label}.conf")
    return 0
