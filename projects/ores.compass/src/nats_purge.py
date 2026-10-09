"""compass nats purge — Empty this environment's JetStream streams.

`compass db recreate` drops and recreates the PostgreSQL database but
leaves the JetStream store in build/nats/<label>/jetstream untouched, so
the workflow service replays the start messages it still holds into a
database that no longer has the tenants they name — eleven
provision_tenant_workflow and bundle_publish_workflow runs landed in a
freshly recreated database that way.  This command closes that hole by
purging the messages, so a recreated database is quiet.

The streams are purged, not deleted.  Services create a stream through
ensure_stream (projects/ores.nats/src/service/jetstream_admin.cpp) only
when it is missing, so a deleted stream would come back — but with its
consumers gone and its configuration re-derived at first use, which is a
larger change than this defect needs.  Purge keeps the topology the
services created once and removes only the work held in it.

The server is usually down when this runs, because `compass services
stop` stops nats-server-<label>.service with the rest of the fleet and
`compass db recreate` is documented as running after the fleet is
stopped; the command therefore starts the unit and waits for it before
listing.

Selects every stream whose name carries this environment's prefix
(ORES_NATS_SUBJECT_PREFIX), which keeps a shared NATS server's other
environments out of the purge.
"""

import argparse
import json
import subprocess
import sys
import time
from pathlib import Path

import nats_ensure
import nats_init
import systemctl_bus

_STREAM_LIST_SETTLE_SECONDS = 2.0
_sleep = time.sleep


def _load_env(project_root: Path) -> dict:
    env = nats_init._load_dotenv(project_root / ".env")
    return env


def _stream_prefix(env: dict) -> str:
    """The stream-name prefix for this environment.

    Subject prefixes are dot-separated (ores.dev.brave_hopper) while
    stream names use underscores (ores_dev_brave_hopper), so the two
    spellings meet here and nowhere else.
    """
    return env.get("ORES_NATS_SUBJECT_PREFIX", "").replace(".", "_")


def _tls_args(env: dict) -> list:
    """--tlsca/--tlscert/--tlskey for every file that exists.

    The server requires client certificates; without these flags the
    CLI stops at 'x509: certificate signed by unknown authority'.
    """
    args = []
    for flag, key in (("--tlsca", "ORES_NATS_TLS_CA"),
                      ("--tlscert", "ORES_NATS_TLS_CERT"),
                      ("--tlskey", "ORES_NATS_TLS_KEY")):
        value = env.get(key, "")
        if value and Path(value).is_file():
            args += [flag, value]
    return args


def _server(env: dict) -> str:
    return env.get("ORES_NATS_URL") or (
        f"nats://localhost:{env.get('ORES_NATS_PORT', '4222')}")


def _cli(env: dict) -> list:
    return ["nats", "--server", _server(env)] + _tls_args(env)


def _stream_names(text: str, prefix: str) -> list:
    """The streams this environment owns, in the order the CLI printed them."""
    return [line.strip() for line in text.splitlines()
            if line.strip().startswith(prefix)]


def _list_streams(env: dict):
    """Print stream names, or None when the server did not answer."""
    try:
        proc = subprocess.run(_cli(env) + ["stream", "ls", "-n"],
                              capture_output=True, text=True, check=False)
    except OSError:
        return None
    if proc.returncode != 0:
        return None
    return proc.stdout


def _start_server(label: str, port: int) -> int:
    unit = nats_ensure._find_unit(label)
    proc = systemctl_bus.run(["list-unit-files", unit],
                             capture_output=True, text=True, check=False)
    if unit not in proc.stdout:
        print(f"Error: no systemd user unit '{unit}' for environment "
              f"'{label}'.", file=sys.stderr)
        print("  Start the server manually with:", file=sys.stderr)
        print(f"    nats-server --config build/config/nats-{label}.conf",
              file=sys.stderr)
        return 1

    print(f"=== NATS server is down; starting {unit} ===")
    try:
        proc = systemctl_bus.run(["start", unit],
                                 capture_output=True, text=True, check=False)
    except OSError as e:
        print(f"Error: could not start {unit}: {e}", file=sys.stderr)
        return 1
    if proc.returncode != 0:
        print(f"Error: systemctl --user start {unit} failed:", file=sys.stderr)
        print(proc.stderr.strip() or proc.stdout.strip(), file=sys.stderr)
        return 1
    return nats_ensure._probe(port)


def _ensure_available(env: dict, label: str) -> int:
    """Return 0 once the server answers a stream listing.

    A down server is the common case, not an error: the unit is started
    and the port probed, and only a server that stays down fails.
    """
    if _list_streams(env) is not None:
        return 0

    try:
        port = int(env.get("ORES_NATS_PORT", "4222"))
    except ValueError:
        print(f"Error: invalid ORES_NATS_PORT in .env: "
              f"{env.get('ORES_NATS_PORT')}", file=sys.stderr)
        return 1
    if _start_server(label, port) != 0:
        return 1

    print(f"  Waiting {_STREAM_LIST_SETTLE_SECONDS:.0f}s for the streams to "
          f"settle")
    _sleep(_STREAM_LIST_SETTLE_SECONDS)
    if _list_streams(env) is None:
        print(f"Error: NATS server at {_server(env)} does not list streams "
              f"after start.", file=sys.stderr)
        return 1
    return 0


def run(argv, project_root: Path) -> int:
    ap = argparse.ArgumentParser(
        prog="compass nats purge",
        description="Purge every JetStream stream that carries this "
                    "environment's prefix, so a recreated database is not "
                    "replayed with stale work.")
    ap.parse_args(argv)

    env = _load_env(project_root)
    systemctl_bus.adopt_transport_setting(env)
    label = env.get("ORES_CHECKOUT_LABEL", project_root.name)
    prefix = _stream_prefix(env)
    if not prefix:
        print("Error: ORES_NATS_SUBJECT_PREFIX is not set; no stream prefix "
              "to purge.", file=sys.stderr)
        return 1

    print(f"=== NATS purge for environment '{label}' ===")
    print(f"  Server: {_server(env)}")
    print(f"  Prefix: {prefix}")

    if _ensure_available(env, label) != 0:
        return 1

    names = _stream_names(_list_streams(env) or "", prefix)
    if not names:
        print(f"No streams carry the prefix '{prefix}'.")
        return 0

    for name in names:
        info = subprocess.run(_cli(env) + ["stream", "info", name, "--json"],
                              capture_output=True, text=True, check=False)
        messages = ""
        if info.returncode == 0:
            try:
                state = json.loads(info.stdout).get("state", {})
                messages = f" ({state.get('messages', 0)} messages)"
            except ValueError:
                messages = ""
        print(f"=== Purging {name}{messages} ===")
        proc = subprocess.run(_cli(env) + ["stream", "purge", name, "-f"],
                              capture_output=True, text=True, check=False)
        if proc.returncode != 0:
            print(f"Error: purge of stream '{name}' failed:", file=sys.stderr)
            print(proc.stderr.strip() or proc.stdout.strip(), file=sys.stderr)
            return 1
        print(f"  Purged: {name}")

    print(f"\n=== Purged {len(names)} stream(s) for '{label}' ===")
    return 0
