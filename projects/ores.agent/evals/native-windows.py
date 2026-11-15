"""Start an owned, isolated Windows OreStudio fleet for the agent evaluation."""

import argparse
import os
from pathlib import Path
import re
import shutil
import socket
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / "build/scripts"))
from run_windows_services import WindowsJob, launch_plan, load_env, registry


def replace_setting(config, key, value):
    pattern = rf"(?m)^(\s*{re.escape(key)}\s*:)\s*[^\r\n]+$"
    result, count = re.subn(pattern, lambda match: f"{match[1]} {value}", config)
    if count != 1:
        raise RuntimeError(f"Expected one {key} setting in the native NATS configuration")
    return result


def long_path(path):
    """Use extended paths for the local object-store snapshot on Windows."""
    resolved = str(path.resolve())
    if resolved.startswith("\\\\"):
        return "\\\\?\\UNC\\" + resolved[2:]
    return "\\\\?\\" + resolved


def wait_until(predicate, process, description, timeout):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if process.poll() is not None:
            raise RuntimeError(f"{description} exited during startup")
        if predicate():
            return
        time.sleep(0.25)
    raise RuntimeError(f"{description} did not become ready")


def port_ready(port):
    try:
        with socket.create_connection(("127.0.0.1", port), timeout=0.5):
            return True
    except OSError:
        return False


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--http-port", type=int, required=True)
    parser.add_argument("--monitor-port", type=int, required=True)
    parser.add_argument("--broker", type=Path,
                        default=ROOT / "build/nats/vendored/windows-amd64/nats-server.exe")
    parser.add_argument("--nats-config", type=Path)
    parser.add_argument("--storage-source", type=Path)
    args = parser.parse_args()
    if os.name != "nt":
        parser.error("The native Windows launcher requires Windows Python")
    database = os.environ.get("ORES_AGENT_CLONE_DATABASE", "")
    if not re.fullmatch(r"ores_agent_it_[a-f0-9]{32}", database):
        parser.error("Launch from the agent database clone runner")
    output = Path(os.environ["ORES_AGENT_NATIVE_OUTPUT"])
    port = int(os.environ["ORES_AGENT_NATIVE_NATS_PORT"])
    ports = (port, args.http_port, args.monitor_port)
    if len(set(ports)) != 3 or any(not 1024 <= p <= 65535 or port_ready(p) for p in ports):
        parser.error("Supply three distinct unused local service ports")

    env = load_env(ROOT)
    original_ports = {int(env[name]) for name in ("ORES_NATS_PORT", "ORES_HTTP_PORT",
                      "ORES_NATS_MONITOR_PORT") if env.get(name)}
    if original_ports.intersection(ports):
        parser.error("The clone needs different ports from the configured development fleet")
    config_path = args.nats_config or ROOT / "build/config" / f"nats-{env['ORES_CHECKOUT_LABEL']}.conf"
    runtime = output / "fleet-runtime"
    runtime.mkdir()
    store = (runtime / "jetstream").as_posix()
    config = config_path.read_text(encoding="utf-8")
    config = replace_setting(config, "port", port)
    config = replace_setting(config, "http", f"127.0.0.1:{args.monitor_port}")
    config = replace_setting(config, "store_dir", f'"{store}"')
    if re.search(r"(?m)^\s*(host|listen|include)\s*:", config):
        parser.error("Use the Compass-generated standalone NATS configuration")
    nats_config = runtime / "nats-clone.conf"
    nats_config.write_text("host: 127.0.0.1\n" + config, encoding="utf-8")

    env.update({
        "ORES_DATABASE_NAME": database,
        "ORES_DB_HOST": os.environ["PGHOST"],
        "PGPORT": os.environ["PGPORT"],
        "ORES_NATS_PORT": str(port),
        "ORES_NATS_URL": f"nats://localhost:{port}",
        "ORES_NATS_SUBJECT_PREFIX": os.environ["ORES_AGENT_NATIVE_NATS_PREFIX"],
        "ORES_NATS_MONITOR_PORT": str(args.monitor_port),
        "ORES_NATS_MONITOR_URL": f"http://127.0.0.1:{args.monitor_port}",
        "ORES_TELEMETRY_SERVICE_NATS_MONITOR_URL": f"http://127.0.0.1:{args.monitor_port}",
        "ORES_HTTP_PORT": str(args.http_port),
        "ORES_HTTP_SERVER_PORT": str(args.http_port),
        "ORES_HTTP_SERVER_STORAGE_DIR": str(runtime / "storage"),
    })
    for key in env:
        if key.endswith("_DB_DATABASE"):
            env[key] = database
        elif key.endswith("_DB_HOST"):
            env[key] = os.environ["PGHOST"]
        elif key.endswith("_DB_PORT"):
            env[key] = os.environ["PGPORT"]
    services = [dict(s, replicas=1) for s in registry.load_service_registry(ROOT)
                if s["enabled"] and s["name"] not in (
                    "ores.web.service", "ores.marketdata.official")]
    for service in services:
        if service["name"] in ("ores.inbox.service", "ores.reporting.service"):
            service["depends_on"] = [*service.get("depends_on", []), "ores.scheduler.service"]
    plan = launch_plan(ROOT, env, services)
    publish = ROOT / "build/output" / env["ORES_PRESET"] / "publish"
    storage_source = args.storage_source or publish / "storage"
    if storage_source.is_dir():
        shutil.copytree(long_path(storage_source), long_path(runtime / "storage"))
    elif args.storage_source is not None:
        raise RuntimeError("The supplied object-storage source directory does not exist")
    child_env = {**os.environ, **env}
    child_env["PATH"] = os.pathsep.join([
        str(publish / "bin"), str(publish / "lib"),
        str(publish.parent / "vcpkg_installed/x64-windows/bin"), os.environ["PATH"]])
    children = []
    job = WindowsJob()

    def start(label, argv, cwd, process_env):
        log = output / f"{label}.log"
        with log.open("wb") as stream:
            child = subprocess.Popen(argv, cwd=cwd, env=process_env, stdout=stream,
                                     stderr=subprocess.STDOUT,
                                     creationflags=(subprocess.CREATE_NEW_PROCESS_GROUP |
                                                    subprocess.CREATE_NO_WINDOW))
        try:
            job.add(child)
        except OSError:
            child.terminate()
            raise
        children.append((label, child))
        return child, log

    try:
        broker, _ = start("nats", [str(args.broker.resolve()), "-c", str(nats_config)], output, child_env)
        wait_until(lambda: port_ready(port), broker, "Clone NATS", 20)
        key_path = ROOT / "build/keys/iam-rsa-private.pem"
        dependencies = {dependency for service in services for dependency in service.get("depends_on", [])}
        essentials = dependencies | {"ores.refdata.service", "ores.trading.service",
                                      "ores.reporting.service", "ores.marketdata.service"}
        for label, name, argv, cwd in plan:
            process_env = child_env.copy()
            if name == "ores.iam.service":
                process_env["ORES_IAM_SERVICE_JWT_PRIVATE_KEY"] = key_path.read_text()
            for flag, directory in (("--work-dir", "work"), ("--storage-dir", "storage")):
                if flag in argv:
                    argv[argv.index(flag) + 1] = str(runtime / directory)
            child, log = start(label, argv, cwd, process_env)
            if name in essentials:
                wait_until(lambda: "Service ready." in log.read_text(encoding="utf-8", errors="replace"),
                           child, label, 90)
        (output / "fleet-ready").touch()
        while True:
            for label, child in children:
                if child.poll() is not None:
                    raise RuntimeError(f"{label} exited with code {child.returncode}; see its log")
            time.sleep(0.5)
    finally:
        job.close()
        for _, child in children:
            child.wait(timeout=10)


if __name__ == "__main__":
    main()
