"""Run the local Windows application fleet in the foreground; Ctrl+C stops it.

PostgreSQL and NATS must already be running. This development launcher reads
the same service registry as the systemd deployment, without installing any
Windows services. A Windows job owns the processes and their descendants.
"""

import argparse
import ctypes
from ctypes import wintypes
from graphlib import TopologicalSorter
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import socket
import subprocess
import sys
import time
import uuid

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "projects/ores.compass/src"))
from compass_db import load_env
import systemd_generate as registry


class WindowsJob:
    """Close the job to terminate only this launcher's process trees."""

    def __init__(self):
        class BasicLimits(ctypes.Structure):
            _fields_ = [("process_time", ctypes.c_int64),
                        ("job_time", ctypes.c_int64),
                        ("flags", wintypes.DWORD),
                        ("min_working_set", ctypes.c_size_t),
                        ("max_working_set", ctypes.c_size_t),
                        ("active_processes", wintypes.DWORD),
                        ("affinity", ctypes.c_size_t),
                        ("priority", wintypes.DWORD),
                        ("scheduling", wintypes.DWORD)]

        class ExtendedLimits(ctypes.Structure):
            _fields_ = [("basic", BasicLimits),
                        ("io_counters", ctypes.c_uint64 * 6),
                        ("process_memory", ctypes.c_size_t),
                        ("job_memory", ctypes.c_size_t),
                        ("peak_process_memory", ctypes.c_size_t),
                        ("peak_job_memory", ctypes.c_size_t)]

        self.api = ctypes.WinDLL("kernel32", use_last_error=True)
        self.api.CreateJobObjectW.argtypes = [ctypes.c_void_p, wintypes.LPCWSTR]
        self.api.CreateJobObjectW.restype = wintypes.HANDLE
        self.api.SetInformationJobObject.argtypes = [
            wintypes.HANDLE, ctypes.c_int, ctypes.c_void_p, wintypes.DWORD]
        self.api.AssignProcessToJobObject.argtypes = [wintypes.HANDLE, wintypes.HANDLE]
        self.api.CloseHandle.argtypes = [wintypes.HANDLE]
        self.handle = self.api.CreateJobObjectW(None, None)
        if not self.handle:
            raise ctypes.WinError(ctypes.get_last_error())
        limits = ExtendedLimits()
        limits.basic.flags = 0x2000  # JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
        if not self.api.SetInformationJobObject(
                self.handle, 9, ctypes.byref(limits), ctypes.sizeof(limits)):
            error = ctypes.WinError(ctypes.get_last_error())
            self.close()
            raise error

    def add(self, process):
        if not self.api.AssignProcessToJobObject(self.handle, int(process._handle)):
            raise ctypes.WinError(ctypes.get_last_error())

    def close(self):
        if self.handle:
            self.api.CloseHandle(self.handle)
            self.handle = None


def launch_plan(root, env, services):
    """Build argv lists before starting anything; paths never pass through a shell."""
    publish = root / "build/output" / env["ORES_PRESET"] / "publish"
    keys = root / "build/keys/nats"
    enabled = {svc["name"]: svc for svc in services if svc["enabled"]}
    graph = {name: [dep for dep in svc.get("depends_on", []) if dep in enabled]
             for name, svc in enabled.items()}
    plan = []
    for name in TopologicalSorter(graph).static_order():
        svc = enabled[name]
        for index in range(1, svc["replicas"] + 1):
            label = name if svc["replicas"] == 1 else f"{name}-{index}"
            if svc.get("runtime") == "node":
                argv = [shutil.which("node") or "node", svc["entry_point"]]
                cwd = root / "projects/ores.web"
            else:
                values = {
                    "tenant_id": registry.SYSTEM_TENANT_UUID,
                    "replica_index": str(index),
                    "host_id": str(uuid.uuid5(registry.HOST_ID_NAMESPACE,
                                             f"{socket.gethostname()}:{index}")),
                    "http_port": env["ORES_HTTP_PORT"],
                    "work_dir": str(publish / f"run/wrappers/node_{index}"),
                    "log_dir": str(publish / "log"),
                }
                argv = [str(publish / "bin" / f"{name}.exe"),
                        "--log-enabled", "--log-to-console", "--log-level",
                        registry.service_log_level(env),
                        "--nats-tls-ca", str(keys / "ca.crt"),
                        "--nats-tls-cert", str(keys / f"{name}.crt"),
                        "--nats-tls-key", str(keys / f"{name}.key")]
                for extra in svc.get("extra_args", []):
                    for token in shlex.split(extra):
                        token = re.sub(r"\$\{([^}]+)\}",
                                       lambda match: env[match[1]], token)
                        argv.append(token.format_map(values))
                cwd = publish / "bin"
            plan.append((label, name, argv, cwd))
    return plan


def write_site_config(root, env, destination):
    keys = root / "build/keys/nats"
    label = env["ORES_CHECKOUT_LABEL"]
    configuration = {
        "active": label,
        "environments": [{"id": label, "displayName": env["ORES_ENV_NAME"],
                          "host": "localhost", "port": int(env["ORES_NATS_PORT"]),
                          "subjectPrefix": env["ORES_NATS_SUBJECT_PREFIX"],
                          "httpBaseUrl": f"http://localhost:{env['ORES_HTTP_PORT']}",
                          "nonProduction": True}],
        "tls": {"ca": str(keys / "ca.crt"),
                "cert": str(keys / "ores.web.service.crt"),
                "key": str(keys / "ores.web.service.key")},
        "developerTools": False, "developerAccounts": [],
    }
    destination.write_text(json.dumps(configuration, indent=2) + "\n",
                           encoding="utf-8", newline="\n")


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dry-run", action="store_true", help="List processes without starting them")
    args = parser.parse_args(argv)
    env = load_env(ROOT)
    services = registry.load_service_registry(ROOT)
    plan = launch_plan(ROOT, env, services)
    dependencies = {dep for svc in services if svc["enabled"]
                    for dep in svc.get("depends_on", [])}
    if args.dry_run:
        for label, _, command, _ in plan:
            print(f"{label}: {subprocess.list2cmdline(command)}")
        return 0
    if os.name != "nt":
        parser.error("This launcher requires native Windows Python")
    missing = [command[0] for _, _, command, _ in plan
               if not (Path(command[0]).is_file() or shutil.which(command[0]))]
    if missing:
        print("Build the application before starting the fleet. Missing:\n" +
              "\n".join(missing), file=sys.stderr)
        return 1
    publish = ROOT / "build/output" / env["ORES_PRESET"] / "publish"
    logs = publish / "log/windows"
    logs.mkdir(parents=True, exist_ok=True)
    site = logs / "site.json"
    write_site_config(ROOT, env, site)
    child_env = os.environ.copy()
    child_env.update(env)
    child_env["PATH"] = os.pathsep.join([
        str(publish / "bin"), str(publish / "lib"),
        str(publish.parent / "vcpkg_installed/x64-windows/bin"), child_env["PATH"]])
    child_env["ORES_WEB_SITE_CONFIG"] = str(site)
    child_env["ORES_WEB_ENV"] = env["ORES_CHECKOUT_LABEL"]
    children = []
    job = WindowsJob()
    try:
        for label, name, command, cwd in plan:
            process_env = child_env.copy()
            if name == "ores.iam.service":
                process_env["ORES_IAM_SERVICE_JWT_PRIVATE_KEY"] = (
                    ROOT / "build/keys/iam-rsa-private.pem").read_text()
            with (logs / f"{label}.log").open("wb") as output:
                process = subprocess.Popen(command, cwd=cwd, env=process_env,
                                           stdout=output, stderr=subprocess.STDOUT,
                                           creationflags=subprocess.CREATE_NEW_PROCESS_GROUP)
            children.append((label, process))
            try:
                job.add(process)
            except OSError:
                process.terminate()
                raise
            print(f"Started {label} (PID {process.pid})", flush=True)
            if name in dependencies:
                deadline = time.monotonic() + 120
                while "Service ready." not in (logs / f"{label}.log").read_text(
                        encoding="utf-8", errors="replace"):
                    if process.poll() is not None or time.monotonic() >= deadline:
                        raise RuntimeError(f"{label} did not become ready; inspect {logs / (label + '.log')}")
                    time.sleep(0.25)
        print(f"Logs: {logs}\nCtrl+C stops this fleet and its child processes.", flush=True)
        while True:
            for label, process in children:
                if process.poll() is not None:
                    print(f"{label} exited ({process.returncode}); inspect {logs / (label + '.log')}",
                          file=sys.stderr)
                    return 1
            time.sleep(1)
    except KeyboardInterrupt:
        return 0
    finally:
        job.close()
        for _, process in children:
            process.wait(timeout=10)


if __name__ == "__main__":
    sys.exit(main())
