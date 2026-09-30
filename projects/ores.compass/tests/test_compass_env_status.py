"""
Tests for compass_env_status.py.

No live database, systemd or git: the two boundaries this module composes
are monkeypatched, so the tests describe the composition rather than the
machine they happen to run on.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_env_status.py -v
"""

import json
import subprocess
import sys
from pathlib import Path
from types import SimpleNamespace

import pytest

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass_db
import compass_env_status
import compass_services


@pytest.fixture
def ctx():
    return SimpleNamespace(env_name="eager_maxwell", preset="preset",
                           root=Path("."), env={}, log_dir=Path("."),
                           nats_port=21405)


def _definitions(monkeypatch, *specs):
    """Replace the service registry with (name, replicas) pairs."""
    monkeypatch.setattr(compass_services.systemd_generate,
                        "load_service_registry", lambda root: [])
    monkeypatch.setattr(
        compass_services.systemd_generate, "fetch_service_definitions",
        lambda services: [{"service_name": name, "enabled": True,
                           "desired_replicas": replicas}
                          for name, replicas in specs])


def _states(monkeypatch, states, journal=()):
    """Drive the systemd boundary and the journal from a {unit: state} map.

    A unit the map does not name is one the manager has never heard of, so it
    reads as missing, which is the real shape of an undeployed unit. `journal`
    is the text that unit is treated as having logged."""
    monkeypatch.setattr(compass_services, "_unit_active_state",
                        lambda unit: states.get(unit, "missing"))
    monkeypatch.setattr(
        compass_services, "_journal_lines",
        lambda unit, lines=compass_services.JOURNAL_LINES: list(journal))


class TestClassification:
    """The five states, and what each one's detail says.

    The journal is the log source now, so these drive `_journal_lines` rather
    than a file on disk."""

    def test_a_compiled_service_is_running_when_it_is_active(self, ctx,
                                                             monkeypatch):
        """The compiled services are Type=notify, so ActiveState is the whole
        readiness rule and no log is consulted for them."""
        _states(monkeypatch, {"u": "active"})
        assert compass_services.classify_unit(ctx, "u") == ("running", "active")

    def test_the_node_unit_still_needs_its_readiness_line(self, ctx,
                                                          monkeypatch):
        _states(monkeypatch, {"u": "active"}, journal=["Service ready"])
        assert compass_services.classify_unit(ctx, "u", "node") == \
            ("running", "active")

    def test_the_node_unit_without_the_readiness_line_is_starting(
            self, ctx, monkeypatch):
        _states(monkeypatch, {"u": "active"}, journal=['{"m":"Listening"}'])
        state, detail = compass_services.classify_unit(ctx, "u", "node")
        assert state == "starting"
        assert detail == '{"m":"Listening"}'

    def test_a_failed_unit_is_failed_and_that_is_the_point(self, ctx,
                                                           monkeypatch):
        """Folding failed into stopped hid a crashed service behind a
        deliberate shutdown, which is the defect this state exists for."""
        _states(monkeypatch, {"u": "failed"}, journal=["bind: address in use"])
        state, detail = compass_services.classify_unit(ctx, "u")
        assert state == "failed"
        assert detail == "bind: address in use"

    def test_an_unknown_unit_is_missing(self, ctx, monkeypatch):
        _states(monkeypatch, {})
        assert compass_services.classify_unit(ctx, "u") == \
            ("missing", "unit not loaded")

    def test_an_inactive_unit_is_stopped_not_failed(self, ctx, monkeypatch):
        _states(monkeypatch, {"u": "inactive"})
        assert compass_services.classify_unit(ctx, "u") == ("stopped", "inactive")

    def test_an_activating_unit_is_starting(self, ctx, monkeypatch):
        _states(monkeypatch, {"u": "activating"})
        assert compass_services.classify_unit(ctx, "u")[0] == "starting"

    def test_nats_is_reported_on_active_state_alone(self, ctx, monkeypatch):
        """nats-server is not a compiled service, so no readiness line is
        looked for. Its unit's port check is what the start path waits on."""
        _states(monkeypatch, {"nats-server": "active"})
        assert compass_services.classify_unit(ctx, "nats-server") == \
            ("running", "active")

    def test_a_service_log_line_is_trimmed_to_its_message(self, ctx,
                                                          monkeypatch):
        _states(monkeypatch, {"u": "active"},
                journal=['{"timestamp":"now","level":"info"] service started'])
        state, detail = compass_services.classify_unit(ctx, "u", "node")
        assert (state, detail) == ("starting", "service started")


class TestGatherUnits:
    def test_failed_is_counted_apart_from_stopped(self, ctx, monkeypatch):
        _definitions(monkeypatch, ("ores.web.service", 1),
                     ("ores.iam.service", 1))
        _states(monkeypatch, {"ores.web.service-eager_maxwell": "failed",
                              "ores.iam.service-eager_maxwell": "inactive"})
        counts = compass_services.gather_units(ctx)["counts"]
        assert counts == {"running": 0, "starting": 0, "stopped": 1,
                          "failed": 1, "missing": 0}

    def test_every_replica_is_its_own_row(self, ctx, monkeypatch):
        _definitions(monkeypatch, ("ores.compute.wrapper", 3))
        _states(monkeypatch, {})
        gathered = compass_services.gather_units(ctx)
        assert [row["label"] for row in gathered["units"]] == \
            ["compute.wrapper-1", "compute.wrapper-2", "compute.wrapper-3"]
        assert gathered["service_total"] == 3

    def test_nats_is_reported_apart_from_the_service_counts(self, ctx,
                                                            monkeypatch):
        _definitions(monkeypatch, ("ores.web.service", 1))
        _states(monkeypatch, {"nats-server-eager_maxwell": "active"})
        gathered = compass_services.gather_units(ctx)
        assert gathered["nats"]["state"] == "running"
        assert gathered["counts"]["running"] == 0
        assert gathered["service_total"] == 1

    def test_an_empty_environment_name_yields_no_units(self, monkeypatch):
        ctx = SimpleNamespace(env_name="", preset="preset", root=Path("."),
                              env={}, log_dir=Path("."), nats_port=0)
        assert compass_services.gather_units(ctx)["units"] == []


DB_INFO = {"restored_at": "2026-09-28 15:34", "schema_version": "0.0.25",
           "git_commit": "5559b012aed", "git_date": "2026/09/28 14:19:46"}

ENV = {"ORES_PRESET": "preset", "ORES_ENV_NAME": "eager_maxwell",
       "ORES_CHECKOUT_LABEL": "eager_maxwell", "ORES_ENV_VERSION": "26",
       "ORES_TEST_DB_DATABASE": "ores_dev_eager_maxwell", "PGPASSWORD": "x"}


def _gather(monkeypatch, root, *, counts=None, drift=None, bootstrap=False,
            required=26, activities=(), restored_at=None, reachable=True):
    """gather() with every boundary replaced by a stated answer.

    The restore time defaults to an hour ago so the default environment is
    a healthy one; a test that cares about the age states it."""
    import datetime
    counts = counts or {"running": 2, "starting": 0, "stopped": 0,
                        "failed": 0, "missing": 0}
    if restored_at is None:
        restored_at = (datetime.datetime.now()
                       - datetime.timedelta(hours=1)).strftime("%Y-%m-%d %H:%M")
    info = dict(DB_INFO)
    info["restored_at"] = restored_at
    ctx = SimpleNamespace(env_name="eager_maxwell", preset="preset",
                          root=root, env=ENV, log_dir=Path("/logs"),
                          nats_port=21405)
    monkeypatch.setattr(compass_services, "Ctx", lambda *a: ctx)
    monkeypatch.setattr(compass_services, "gather_units", lambda _ctx: {
        "nats": {"unit": "nats", "label": "nats-server", "state": "running",
                 "detail": "active", "service": "", "replica": 0, "log": ""},
        "units": [{"unit": "u", "service": "ores.web.service", "replica": 0,
                   "label": "web", "log": "web.log", "state": "running",
                   "detail": "active"}],
        "counts": counts, "service_total": 2})
    monkeypatch.setattr(compass_db, "database_info",
                        lambda env: info if reachable else None)
    monkeypatch.setattr(compass_db, "schema_drift",
                        lambda project_root, i: drift or (0, "current",
                                                          "\033[32m", None))
    monkeypatch.setattr(compass_db, "bootstrap_mode", lambda env: bootstrap)
    monkeypatch.setattr(compass_env_status, "session_scope",
                        lambda: ("dsh-1.scope", "app.slice"))
    import env_activity
    import env_init
    monkeypatch.setattr(env_activity, "pending",
                        lambda root, current: list(activities))
    monkeypatch.setattr(env_init, "current_version", lambda root: required)
    import compass
    monkeypatch.setattr(compass, "vcpkg_drift",
                        lambda root: {"error": None, "current": "a",
                                      "expected": "a"})
    return compass_env_status.gather(root, ENV)


class TestPayload:
    def test_a_healthy_environment_reads_ok(self, monkeypatch, tmp_path):
        payload = _gather(monkeypatch, tmp_path)
        assert payload["ok"] is True
        assert payload["health"] == {"level": "ok", "reasons": []}
        assert payload["services"]["counts"]["running"] == 2

    def test_the_restore_age_is_reported_with_its_level(self, monkeypatch,
                                                       tmp_path):
        payload = _gather(monkeypatch, tmp_path,
                          restored_at="2026-09-28 15:34")
        assert payload["database"]["restoredAt"] == "2026-09-28 15:34"
        assert payload["database"]["restoredAgeSeconds"] > 24 * 3600
        assert payload["database"]["restoredLevel"] == "critical"

    def test_a_fresh_restore_reads_ok(self, monkeypatch, tmp_path):
        payload = _gather(monkeypatch, tmp_path)
        assert payload["database"]["restoredAge"] == "1h"
        assert payload["database"]["restoredLevel"] == "ok"

    def test_a_failed_service_makes_the_environment_critical(self, monkeypatch,
                                                             tmp_path):
        payload = _gather(monkeypatch, tmp_path,
                          counts={"running": 1, "starting": 0, "stopped": 0,
                                  "failed": 1, "missing": 0})
        assert payload["health"]["level"] == "critical"
        assert "1 service(s) failed" in payload["health"]["reasons"]

    def test_a_missing_unit_names_the_deployment(self, monkeypatch, tmp_path):
        payload = _gather(monkeypatch, tmp_path,
                          counts={"running": 0, "starting": 0, "stopped": 0,
                                  "failed": 0, "missing": 2})
        assert payload["health"]["level"] == "critical"
        assert "2 unit(s) are not deployed" in payload["health"]["reasons"]

    def test_stopped_services_warn_rather_than_fail(self, monkeypatch,
                                                    tmp_path):
        payload = _gather(monkeypatch, tmp_path,
                          counts={"running": 0, "starting": 0, "stopped": 2,
                                  "failed": 0, "missing": 0})
        assert payload["health"]["level"] == "warn"
        assert "2 of 2 services are stopped" in payload["health"]["reasons"]

    def test_an_unreachable_database_is_critical(self, monkeypatch, tmp_path):
        payload = _gather(monkeypatch, tmp_path, reachable=False)
        assert payload["database"]["reachable"] is False
        assert payload["database"]["reason"] == "unreachable"
        assert payload["health"]["level"] == "critical"

    def test_bootstrap_mode_warns(self, monkeypatch, tmp_path):
        payload = _gather(monkeypatch, tmp_path, bootstrap=True)
        assert payload["health"]["level"] == "warn"
        assert any("bootstrap mode" in reason
                   for reason in payload["health"]["reasons"])

    def test_a_stale_env_file_warns(self, monkeypatch, tmp_path):
        payload = _gather(monkeypatch, tmp_path, required=27)
        assert payload["env"]["envStale"] is True
        assert any(".env is stale" in reason
                   for reason in payload["health"]["reasons"])

    def test_schema_drift_is_carried_with_its_level(self, monkeypatch,
                                                    tmp_path):
        payload = _gather(monkeypatch, tmp_path,
                          drift=(90000, "1d behind HEAD — drifting",
                                 "\033[33m", "⚠  Schema is drifting"))
        assert payload["database"]["driftLevel"] == "warn"
        assert any("drifting" in reason
                   for reason in payload["health"]["reasons"])

    def test_the_payload_carries_no_terminal_colour(self, monkeypatch,
                                                    tmp_path):
        payload = _gather(monkeypatch, tmp_path,
                          drift=(400000, "4d behind HEAD — stale",
                                 "\033[31m",
                                 "\033[31m⚠  Schema is stale\033[0m"))
        text = json.dumps(payload)
        assert "\033[" not in text
        assert "Schema is stale" in payload["database"]["warning"]

    def test_outstanding_activities_are_listed_and_warn(self, monkeypatch,
                                                        tmp_path):
        payload = _gather(monkeypatch, tmp_path,
                          activities=[(1, "2026-09-01", "Rotate a cert", "UUID-1")])
        assert payload["env"]["activities"][0]["title"] == "Rotate a cert"
        assert payload["health"]["level"] == "warn"

    def test_the_payload_round_trips_through_json(self, monkeypatch, tmp_path):
        payload = _gather(monkeypatch, tmp_path)
        assert json.loads(json.dumps(payload)) == payload


class TestRender:
    def test_the_block_names_the_identity_the_database_and_the_fleet(
            self, monkeypatch, tmp_path):
        payload = _gather(monkeypatch, tmp_path,
                          restored_at="2026-09-28 15:34")
        lines = compass_env_status.render_lines(payload)
        assert any(line.startswith("  Preset   : preset  (label: eager_maxwell")
                   for line in lines)
        assert any("restored 2026-09-28 15:34" in line and
                   "schema 0.0.25" in line for line in lines)
        assert any("running=2" in line and "failed=0" in line and
                   "(nats: running)" in line for line in lines)

    def test_an_unreachable_database_says_so_with_its_remedy(self, monkeypatch,
                                                             tmp_path):
        lines = compass_env_status.render_lines(
            _gather(monkeypatch, tmp_path, reachable=False))
        assert any("unreachable" in line and "compass db recreate" in line
                   for line in lines)


class TestCommand:
    def test_json_is_one_parseable_object(self, monkeypatch, tmp_path, capsys):
        monkeypatch.setattr(compass_env_status, "gather",
                            lambda root, env, preset=None: {"ok": True,
                                                            "probe": "yes"})
        monkeypatch.setattr(compass_db, "load_env", lambda root: ENV)
        assert compass_env_status.run(["--json"], tmp_path) == 0
        assert json.loads(capsys.readouterr().out) == {"ok": True,
                                                       "probe": "yes"}

    def test_the_text_form_names_the_health_level(self, monkeypatch, tmp_path,
                                                  capsys):
        monkeypatch.setattr(compass_env_status, "gather",
                            lambda root, env, preset=None: {
                                "env": {"preset": "p", "label": "l",
                                        "envVersion": 1, "scope": "",
                                        "slice": "", "envStale": False,
                                        "requiredEnvVersion": 1,
                                        "activities": [], "vcpkgWarning": ""},
                                "database": {"reachable": False},
                                "services": {"counts": {"running": 0,
                                                        "starting": 0,
                                                        "stopped": 0,
                                                        "failed": 0,
                                                        "missing": 0},
                                             "nats": None, "total": 0},
                                "health": {"level": "warn",
                                           "reasons": ["a reason"]}})
        monkeypatch.setattr(compass_db, "load_env", lambda root: ENV)
        assert compass_env_status.run([], tmp_path) == 0
        out = capsys.readouterr().out
        assert "Health   : warn" in out
        assert "- a reason" in out
