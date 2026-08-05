"""Tests for load_org_service_registry_model's merged DB-access +
deployment aspect shape.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_org_loader_service_registry.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import load_org_service_registry_model  # noqa: E402

MODEL_PATH = REPO_ROOT / "projects/modeling/service_registry.org"


@pytest.fixture(scope="module")
def services():
    model = load_org_service_registry_model(MODEL_PATH)
    return {s["name"]: s for s in model["service_registry"]["services"]}


def test_every_entry_has_the_deployment_aspect(services):
    for name, svc in services.items():
        assert isinstance(svc["replicas"], int), name
        assert isinstance(svc["enabled"], bool), name
        assert isinstance(svc["depends_on"], list), name
        assert isinstance(svc["extra_args"], list), name


def test_db_access_aspect_is_optional(services):
    # ores.iam.service has its own NATS-domain-service role.
    assert "psql_var" in services["ores.iam.service"]
    # ores.http.server/ores.wt.service/ores.compute.wrapper do not.
    for name in ("ores.http.server", "ores.wt.service", "ores.compute.wrapper"):
        assert "psql_var" not in services[name], name


def test_compute_wrapper_has_five_replicas(services):
    assert services["ores.compute.wrapper"]["replicas"] == 5


def test_wt_service_disabled_by_default(services):
    assert services["ores.wt.service"]["enabled"] is False


def test_dependent_services_depend_on_iam(services):
    assert services["ores.iam.service"]["depends_on"] == []
    assert services["ores.refdata.service"]["depends_on"] == ["ores.iam.service"]


def test_extra_args_round_trip_multiple_flags(services):
    assert services["ores.compute.wrapper"]["extra_args"] == [
        "--host-id {host_id}",
        "--tenant-id {tenant_id}",
        "--work-dir {work_dir}",
        "--http-base-url http://localhost:{http_port}",
    ]


def test_no_stale_controller_entry(services):
    assert "controller" not in services
    assert "ores.controller.service" not in services


def test_nineteen_fleet_processes(services):
    assert len(services) == 19


def test_db_grant_prefixes_still_round_trip(services):
    syn = services["ores.synthetic.service"]
    assert {"prefix": "ores_synthetic_"} in syn["dml_prefixes"]
    assert {"prefix": "ores_synthetic_publish_"} in syn["execute_prefixes"]
    assert len(syn["select_prefixes"]) == 9
