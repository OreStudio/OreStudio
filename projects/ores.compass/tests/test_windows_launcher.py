"""Windows development launch arguments and process-tree ownership."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest

ROOT = Path(__file__).resolve().parents[3]
spec = importlib.util.spec_from_file_location(
    "windows_launcher", ROOT / "build/scripts/run_windows_services.py")
launcher = importlib.util.module_from_spec(spec)
spec.loader.exec_module(launcher)


def service(name, **kwargs):
    return dict(name=name, enabled=True, replicas=1) | kwargs


def test_plan_orders_dependencies_and_preserves_paths_with_spaces(tmp_path):
    root = tmp_path / "My Checkout"
    env = {"ORES_PRESET": "windows-msvc-release-ninja", "ORES_HTTP_PORT": "20000",
           "MONITOR_URL": "http://localhost:20006"}
    services = [service("ores.telemetry.service", depends_on=["ores.iam.service"],
                        extra_args=["--monitor ${MONITOR_URL}"]),
                service("ores.iam.service")]
    plan = launcher.launch_plan(root, env, services)
    assert [item[0] for item in plan] == ["ores.iam.service", "ores.telemetry.service"]
    command = plan[1][2]
    assert command[-2:] == ["--monitor", "http://localhost:20006"]
    assert command[command.index("--nats-tls-ca") + 1] == str(root / "build/keys/nats/ca.crt")


def test_wrapper_replicas_have_distinct_stable_ids_and_work_directories(tmp_path):
    services = [service("ores.compute.wrapper", replicas=2,
                        extra_args=["--host-id {host_id}", "--work-dir {work_dir}"])]
    env = {"ORES_PRESET": "windows-msvc-release-ninja", "ORES_HTTP_PORT": "20000"}
    plan = launcher.launch_plan(tmp_path, env, services)
    assert plan == launcher.launch_plan(tmp_path, env, services)
    assert plan[0][0] != plan[1][0]
    assert plan[0][2][-3] != plan[1][2][-3]
    assert plan[0][2][-1] != plan[1][2][-1]


def test_site_config_uses_lowercase_label_and_local_endpoints(tmp_path):
    env = {"ORES_CHECKOUT_LABEL": "orestudio", "ORES_ENV_NAME": "OreStudio",
           "ORES_NATS_PORT": "20005", "ORES_HTTP_PORT": "20000",
           "ORES_NATS_SUBJECT_PREFIX": "ores.dev.OreStudio"}
    destination = tmp_path / "site.json"
    launcher.write_site_config(tmp_path, env, destination)
    site = json.loads(destination.read_text())
    assert site["active"] == "orestudio"
    assert site["environments"][0]["host"] == "localhost"
    assert site["environments"][0]["subjectPrefix"] == "ores.dev.OreStudio"


@pytest.mark.skipif(os.name != "nt", reason="Windows job API")
def test_closing_job_stops_its_owned_process():
    process = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(120)"])
    job = launcher.WindowsJob()
    try:
        job.add(process)
        assert process.poll() is None
        job.close()
        process.wait(timeout=10)
        assert process.poll() is not None
    finally:
        job.close()
        if process.poll() is None:
            process.terminate()
            process.wait(timeout=10)
