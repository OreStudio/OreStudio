"""
Tests for compass_services.py's per-service commands.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_services.py -v
No live database, systemd or bus required: the registry and the systemctl
boundary are monkeypatched.
"""

import subprocess
import sys
import time
from pathlib import Path
from types import SimpleNamespace

import pytest

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass_services
import systemctl_bus

REGISTRY = ["ores.iam.service", "ores.web.service", "ores.http.server",
            "ores.compute.wrapper"]


@pytest.fixture
def ctx(monkeypatch):
    monkeypatch.setattr(compass_services, "_registry_names",
                        lambda _ctx: REGISTRY)
    return SimpleNamespace(env_name="eager_maxwell", preset="preset",
                           root=Path("."), env={}, log_dir=Path("."),
                           nats_port=21405)


def _definitions(monkeypatch, service_name, replicas):
    monkeypatch.setattr(compass_services.systemd_generate,
                        "load_service_registry", lambda root: [])
    monkeypatch.setattr(
        compass_services.systemd_generate, "fetch_service_definitions",
        lambda services: [{"service_name": service_name, "enabled": True,
                           "desired_replicas": replicas}])


class TestSelector:
    @pytest.mark.parametrize("selector,expected", [
        ("web", "ores.web.service"),
        ("ores.web", "ores.web.service"),
        ("ores.web.service", "ores.web.service"),
        ("ores.web.service-eager_maxwell", "ores.web.service"),
        ("iam", "ores.iam.service"),
        ("http.server", "ores.http.server"),
        ("ores.http.server", "ores.http.server"),
    ])
    def test_short_and_full_forms_resolve(self, ctx, selector, expected):
        assert compass_services._resolve_service(ctx, selector) == expected

    def test_unknown_selector_raises(self, ctx):
        with pytest.raises(KeyError):
            compass_services._resolve_service(ctx, "nope")

    def test_unknown_selector_is_reported_with_the_known_names(self, ctx,
                                                               capsys):
        name, units = compass_services._resolve_service_or_report(ctx, "nope")
        assert (name, units) == (None, None)
        captured = capsys.readouterr()
        assert "no service 'nope'" in captured.err
        assert "ores.web.service" in captured.err


class TestUnitSelection:
    def test_one_service_selects_only_its_units(self, ctx, monkeypatch):
        _definitions(monkeypatch, "ores.web.service", 1)
        assert compass_services._service_units(
            ctx, only="ores.web.service") == [
            ("ores.web.service-eager_maxwell", "ores.web.service.0.log")]

    def test_a_replicated_service_selects_every_replica(self, ctx,
                                                        monkeypatch):
        _definitions(monkeypatch, "ores.compute.wrapper", 5)
        units = compass_services._service_units(
            ctx, only="ores.compute.wrapper")
        assert len(units) == 5
        assert units[0] == ("ores.compute.wrapper-eager_maxwell-1",
                            "ores.compute.wrapper.1.log")


class TestStopOne:
    def test_stops_only_the_named_unit(self, ctx, monkeypatch):
        seen = []

        def fake_systemctl(args, **_kwargs):
            seen.append(args)
            return subprocess.CompletedProcess(args, 0, "", "")

        monkeypatch.setattr(compass_services, "_systemctl", fake_systemctl)
        _definitions(monkeypatch, "ores.web.service", 1)

        assert compass_services._stop_one(
            ctx, SimpleNamespace(service="web")) == 0
        assert seen == [["stop", "ores.web.service-eager_maxwell.service"]]

    def test_an_unknown_service_stops_nothing(self, ctx, monkeypatch):
        seen = []
        monkeypatch.setattr(compass_services, "_systemctl",
                            lambda args, **kw: seen.append(args))
        assert compass_services._stop_one(
            ctx, SimpleNamespace(service="nope")) == 1
        assert seen == []


class TestStartOne:
    def test_starts_only_the_named_unit(self, ctx, monkeypatch, capsys):
        seen = []

        def fake_systemctl(args, **_kwargs):
            seen.append(args)
            return subprocess.CompletedProcess(args, 0, "", "")

        monkeypatch.setattr(compass_services, "_systemctl", fake_systemctl)
        monkeypatch.setattr(compass_services, "_wait_for_listen",
                            lambda _port: True)
        monkeypatch.setattr(compass_services, "_service_ready",
                            lambda *a, **k: True)
        monkeypatch.setattr(compass_services.systemd_generate, "cmd_generate",
                            lambda *a: 0)
        monkeypatch.setattr(compass_services.systemd_generate, "cmd_deploy",
                            lambda *a: 0)
        _definitions(monkeypatch, "ores.web.service", 1)

        rc = compass_services._start_one(
            ctx, SimpleNamespace(service="web"), start_ts=0.0)
        assert rc == 0
        assert seen == [["start", "ores.web.service-eager_maxwell.service"]]

    def test_an_unknown_service_starts_nothing(self, ctx, monkeypatch):
        monkeypatch.setattr(compass_services.systemd_generate, "cmd_generate",
                            lambda *a: 0)
        monkeypatch.setattr(compass_services.systemd_generate, "cmd_deploy",
                            lambda *a: 0)
        seen = []
        monkeypatch.setattr(compass_services, "_systemctl",
                            lambda args, **kw: seen.append(args))
        assert compass_services._start_one(
            ctx, SimpleNamespace(service="nope"), start_ts=0.0) == 1
        assert seen == []


class TestTransportSelection:
    """The services pillar takes the transport from the checkout file.

    compass reads .env into a dict and never writes os.environ, so the file's
    choice reaches systemctl_bus only if run() hands it over. A sandboxed
    `compass services status` otherwise reports the units as missing rather
    than stopped, which is the symptom this guards."""

    def _selection(self, tmp_path, env_text, argv, monkeypatch, environ=None):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        if environ is not None:
            monkeypatch.setenv("ORES_USE_BUSCTL", environ)
        (tmp_path / ".env").write_text("ORES_PRESET=preset\n" + env_text)
        monkeypatch.setattr(compass_services, "validate_env_version",
                            lambda *a, **k: None)
        monkeypatch.setattr(compass_services, "cmd_status",
                            lambda ctx, args: 0)
        systemctl_bus.set_use_busctl(None)
        try:
            compass_services.run(list(argv), tmp_path)
            return systemctl_bus.use_busctl()
        finally:
            systemctl_bus.set_use_busctl(None)

    def test_the_checkout_file_selects_the_bus(self, tmp_path, monkeypatch):
        assert self._selection(
            tmp_path, "ORES_USE_BUSCTL=1\n", ["status"], monkeypatch) is True

    def test_without_a_file_value_plain_systemctl_is_kept(
            self, tmp_path, monkeypatch):
        assert self._selection(
            tmp_path, "", ["status"], monkeypatch) is False

    def test_the_environment_selects_the_bus(self, tmp_path, monkeypatch):
        assert self._selection(
            tmp_path, "", ["status"], monkeypatch, environ="1") is True

    def test_the_environment_turns_the_bus_off(self, tmp_path, monkeypatch):
        assert self._selection(
            tmp_path, "ORES_USE_BUSCTL=1\n", ["status"], monkeypatch,
            environ="0") is False


class TestLogs:
    """`compass services logs` reads the journal.

    The level filter matches the service's own severity token rather than a
    journald priority, because journald records every console line at one
    priority whatever the service called it."""

    def test_the_compiled_format_reports_its_severity(self):
        line = "2026-09-30 15:02:19.123456 [warn] [iam] pool nearly full"
        assert compass_services._severity_tokens(line) == {"warn", "iam"}

    def test_the_nats_format_puts_the_severity_after_its_process_id(self):
        line = "[783113] 2026/09/30 01:02:51.145838 [INF] Server Exiting.."
        assert compass_services._severity_tokens(line) == {"783113", "inf"}

    def test_a_line_with_no_brackets_carries_no_severity(self):
        assert compass_services._severity_tokens("Stopped the unit.") == set()

    def test_warnings_and_errors_are_told_apart(self, monkeypatch):
        payload = [
            "2026-09-30 15:02:19.100000 [info] [iam] listening",
            "2026-09-30 15:02:20.100000 [warn] [iam] pool nearly full",
            "2026-09-30 15:02:21.100000 [error] [iam] gave up",
            "[783113] 2026/09/30 01:02:51.145838 [WRN] slow consumer",
        ]
        monkeypatch.setattr(compass_services, "_journal_lines",
                            lambda units, lines=0: list(payload))
        warnings, _ = compass_services._journal_tail(["u"], 10, "warnings")
        errors, _ = compass_services._journal_tail(["u"], 10, "errors")
        assert warnings == [payload[1], payload[3]]
        assert errors == [payload[2]]

    def test_unfiltered_lines_come_back_whole(self, monkeypatch):
        monkeypatch.setattr(compass_services, "_journal_lines",
                            lambda units, lines=0: ["a", "b"])
        found, capped = compass_services._journal_tail(["u"], 5)
        assert (found, capped) == (["a", "b"], False)

    def test_the_tail_reports_when_it_hit_its_cap(self, monkeypatch):
        monkeypatch.setattr(compass_services, "_journal_lines",
                            lambda units, lines=0: ["a", "b"])
        assert compass_services._journal_tail(["u"], 2)[1] is True

    def test_nats_is_reachable_though_no_service_selector_resolves_it(self,
                                                                     ctx):
        assert compass_services._resolve_units(ctx, "nats") == \
            ["nats-server-eager_maxwell"]
        assert compass_services._resolve_units(ctx, "nats-server") == \
            ["nats-server-eager_maxwell"]

    def test_a_short_name_resolves_to_its_units(self, ctx, monkeypatch):
        _definitions(monkeypatch, "ores.web.service", 1)
        assert compass_services._resolve_units(ctx, "web") == \
            ["ores.web.service-eager_maxwell"]

    def test_an_unresolvable_selector_names_nothing(self, ctx, monkeypatch):
        monkeypatch.setattr(compass_services.systemd_generate,
                            "load_service_registry", lambda root: [])
        monkeypatch.setattr(compass_services.systemd_generate,
                            "fetch_service_definitions", lambda services: [])
        assert compass_services._resolve_units(ctx, "nope") == []


class TestStartupFailure:
    """A failed unit ends the wait at once and says why."""

    def test_a_failed_unit_stops_the_wait_instead_of_burning_the_timeout(
            self, monkeypatch):
        monkeypatch.setattr(compass_services, "_unit_active_state",
                            lambda unit: "failed")
        started = time.monotonic()
        waiting = compass_services._await_active({"ores.iam.service"}, {},
                                                  timeout=300)
        elapsed = time.monotonic() - started

        assert waiting == {"ores.iam.service"}
        assert elapsed < 5, f"waited {elapsed:.1f}s on a unit that had failed"

    def test_a_unit_still_coming_up_is_waited_for(self, monkeypatch):
        states = iter(["activating", "active"])
        monkeypatch.setattr(compass_services, "_unit_active_state",
                            lambda unit: next(states))
        monkeypatch.setattr(compass_services.time, "sleep", lambda _s: None)

        ready = {}
        waiting = compass_services._await_active({"ores.iam.service"}, ready,
                                                 timeout=5)

        assert waiting == set()
        assert ready == {"ores.iam.service": True}

    def test_a_failure_prints_the_unit_and_its_journal(self, monkeypatch,
                                                       capsys, ctx):
        monkeypatch.setattr(compass_services, "_unit_active_state",
                            lambda unit: "failed")
        monkeypatch.setattr(
            compass_services, "_journal_lines",
            lambda unit, lines=None: ["REFUSING TO START: the database "
                                      "schema does not match this build."])

        compass_services._report_unstarted(["ores.iam.service"])
        printed = capsys.readouterr().out

        assert "1 unit(s) did not start" in printed
        assert "ores.iam.service: failed" in printed
        assert "REFUSING TO START" in printed

    def test_a_unit_that_never_ran_is_named_without_a_journal(self, monkeypatch,
                                                              capsys):
        monkeypatch.setattr(compass_services, "_unit_active_state",
                            lambda unit: "inactive")
        monkeypatch.setattr(compass_services, "_journal_lines",
                            lambda unit, lines=None: pytest.fail(
                                "a unit that never ran has no journal to read"))

        compass_services._report_unstarted(["ores.refdata.service"])
        printed = capsys.readouterr().out

        assert "ores.refdata.service: inactive" in printed
