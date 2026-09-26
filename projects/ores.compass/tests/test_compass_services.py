"""
Tests for compass_services.py's per-service commands.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_services.py -v
No live database, systemd or bus required: the registry and the systemctl
boundary are monkeypatched.
"""

import subprocess
import sys
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
