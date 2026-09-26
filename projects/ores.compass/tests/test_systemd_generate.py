"""
Tests for systemd_generate.py's TypeScript service rendering.

Run with:  python -m pytest projects/ores.compass/tests/test_systemd_generate.py -v
No live database or systemd access required: the renderer is a pure function.
"""

import sys
from pathlib import Path
from types import SimpleNamespace

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import systemd_generate
import systemctl_bus

PRESET = "linux-clang-debug-make"
TARGET = "ores-eager-maxwell.target"


def _node_def():
    return {
        "service_name": "ores.web.service",
        "binary_name": "ores.web.service",
        "desired_replicas": 1,
        "enabled": True,
        "args_template": "",
        "runtime": "node",
        "entry_point": "packages/bff/dist/main.js",
    }


def _render(deps_on=()):
    return systemd_generate.render_node_unit(
        _node_def(), list(deps_on), "/checkout", "eager_maxwell", TARGET, PRESET)


class TestRenderNodeUnit:
    def test_runs_the_entry_point_from_the_component_directory(self):
        unit = _render()
        assert "Type=simple" in unit
        assert "WorkingDirectory=/checkout/projects/ores.web" in unit
        assert "/checkout/projects/ores.web/packages/bff/dist/main.js" in unit
        assert "EnvironmentFile=/checkout/.env" in unit

    def test_per_service_certificate_overrides_the_shared_one(self):
        unit = _render()
        assert ('export ORES_NATS_TLS_CERT='
                '"/checkout/build/keys/nats/ores.web.service.crt"') in unit
        assert ('export ORES_NATS_TLS_KEY='
                '"/checkout/build/keys/nats/ores.web.service.key"') in unit

    def test_standard_output_lands_where_compass_looks_for_readiness(self):
        unit = _render()
        log = ("/checkout/build/output/"
               f"{PRESET}/publish/log/ores.web.service.0.log")
        assert f"StandardOutput=append:{log}" in unit
        assert f"StandardError=append:{log}" in unit

    def test_requires_nats_and_the_declared_dependencies(self):
        unit = _render(deps_on=["ores.iam.service"])
        assert ("Requires=nats-server-eager_maxwell.service "
                "ores.iam.service-eager_maxwell.service") in unit
        assert f"PartOf={TARGET}" in unit
        assert f"WantedBy={TARGET}" in unit


class TestFetchServiceDefinitions:
    def test_native_is_the_default_runtime(self):
        defs = systemd_generate.fetch_service_definitions(
            [{"name": "ores.iam.service", "replicas": 1, "enabled": True}])
        assert defs[0]["runtime"] == "native"

    def test_node_runtime_and_entry_point_survive(self):
        defs = systemd_generate.fetch_service_definitions(
            [{"name": "ores.web.service", "replicas": 1, "enabled": True,
              "runtime": "node", "entry_point": "packages/bff/dist/main.js"}])
        assert defs[0]["runtime"] == "node"
        assert defs[0]["entry_point"] == "packages/bff/dist/main.js"


class TestTransportSelection:
    """Where the busctl choice comes from.

    compass reads .env into a dict and never writes os.environ, so a value
    that lives only in the file is invisible to use_busctl() unless it is
    handed over. These cases drive run() against a real env file, which is
    the only way to catch that; the transport global is reset around each."""

    def _selection(self, tmp_path, env_text, argv, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        (tmp_path / ".env").write_text(env_text)
        systemctl_bus.set_use_busctl(False)
        try:
            systemd_generate.run(list(argv), tmp_path)
            return systemctl_bus.use_busctl()
        finally:
            systemctl_bus.set_use_busctl(False)

    def test_the_checkout_file_selects_the_bus(self, tmp_path, monkeypatch):
        assert self._selection(
            tmp_path, "ORES_USE_BUSCTL=1\n", ["deploy"], monkeypatch) is True

    def test_a_word_form_in_the_file_is_accepted(self, tmp_path, monkeypatch):
        assert self._selection(
            tmp_path, "ORES_USE_BUSCTL=yes\n", ["deploy"], monkeypatch) is True

    def test_the_file_saying_off_keeps_plain_systemctl(
            self, tmp_path, monkeypatch):
        assert self._selection(
            tmp_path, "ORES_USE_BUSCTL=0\n", ["deploy"], monkeypatch) is False

    def test_no_file_value_and_no_flag_keeps_plain_systemctl(
            self, tmp_path, monkeypatch):
        assert self._selection(tmp_path, "", ["deploy"], monkeypatch) is False

    def test_the_flag_selects_the_bus_without_a_file_value(
            self, tmp_path, monkeypatch):
        assert self._selection(
            tmp_path, "", ["deploy", "--use-busctl"], monkeypatch) is True

    def test_the_flag_wins_over_a_file_that_says_off(
            self, tmp_path, monkeypatch):
        assert self._selection(
            tmp_path, "ORES_USE_BUSCTL=0\n",
            ["deploy", "--use-busctl"], monkeypatch) is True


class TestAdoptTransportSetting:
    """The handover itself, independent of any command's argument parsing."""

    def setup_method(self):
        systemctl_bus.set_use_busctl(False)

    def teardown_method(self):
        systemctl_bus.set_use_busctl(False)

    def test_a_true_file_value_turns_the_bus_on(self, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        systemctl_bus.adopt_transport_setting({"ORES_USE_BUSCTL": "1"})
        assert systemctl_bus.use_busctl() is True

    def test_a_false_file_value_leaves_it_off(self, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        systemctl_bus.adopt_transport_setting({"ORES_USE_BUSCTL": "0"})
        assert systemctl_bus.use_busctl() is False

    def test_a_missing_key_leaves_it_off(self, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        systemctl_bus.adopt_transport_setting({})
        assert systemctl_bus.use_busctl() is False

    def test_the_flag_turns_it_on_with_no_file_value(self, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        systemctl_bus.adopt_transport_setting({}, flag=True)
        assert systemctl_bus.use_busctl() is True


class TestReloadSystemd:
    """A failed reload must not pass as a successful deploy.

    systemd never reads units it has not reloaded, so swallowing the failure
    left an undeployed fleet looking deployed."""

    @staticmethod
    def _result(returncode, stdout="", stderr=""):
        return SimpleNamespace(returncode=returncode, stdout=stdout,
                               stderr=stderr)

    def test_a_successful_reload_says_nothing(self, monkeypatch, capsys):
        monkeypatch.setattr(systemd_generate.systemctl_bus, "run",
                            lambda *a, **k: self._result(0))
        assert systemd_generate._reload_systemd() == 0
        assert capsys.readouterr().err == ""

    def test_a_failed_reload_reports_the_reason_and_fails(
            self, monkeypatch, capsys):
        monkeypatch.setattr(
            systemd_generate.systemctl_bus, "run",
            lambda *a, **k: self._result(
                1, stderr="Failed to connect to user scope bus\n"))
        assert systemd_generate._reload_systemd() == 1
        err = capsys.readouterr().err
        assert "did not reload" in err
        assert "Failed to connect to user scope bus" in err

    def test_a_failed_reload_without_busctl_points_at_the_sandbox(
            self, monkeypatch, capsys):
        monkeypatch.setattr(systemd_generate.systemctl_bus, "run",
                            lambda *a, **k: self._result(1, stderr="boom\n"))
        monkeypatch.setattr(systemd_generate.systemctl_bus, "use_busctl",
                            lambda: False)
        monkeypatch.setattr(systemd_generate.systemctl_bus, "sandbox_hint",
                            lambda: "  retry with --use-busctl")
        assert systemd_generate._reload_systemd() == 1
        assert "retry with --use-busctl" in capsys.readouterr().err

    def test_a_failed_reload_through_the_bus_omits_the_sandbox_hint(
            self, monkeypatch, capsys):
        monkeypatch.setattr(systemd_generate.systemctl_bus, "run",
                            lambda *a, **k: self._result(1, stderr="boom\n"))
        monkeypatch.setattr(systemd_generate.systemctl_bus, "use_busctl",
                            lambda: True)
        assert systemd_generate._reload_systemd() == 1
        assert "retry with --use-busctl" not in capsys.readouterr().err
