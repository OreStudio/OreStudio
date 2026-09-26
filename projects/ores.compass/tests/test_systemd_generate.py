"""
Tests for systemd_generate.py's TypeScript service rendering.

Run with:  python -m pytest projects/ores.compass/tests/test_systemd_generate.py -v
No live database or systemd access required: the renderer is a pure function.
"""

import sys
from pathlib import Path

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


class TestBusctlFlag:
    """`compass systemd` accepts the transport flag the services pillar has.

    The flag has to reach systemctl_bus, or a sandboxed caller keeps the
    unreachable-manager failure it passed the flag to avoid. Unlike the rest
    of this module these cases drive run(), so the transport global is reset
    around each one."""

    @staticmethod
    def _bare_checkout(tmp_path):
        # load_env exits when the file is absent, and the deploy only needs
        # to get past it to prove the flag landed.
        (tmp_path / ".env").write_text("")

    def test_the_flag_selects_the_bus(self, tmp_path, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        self._bare_checkout(tmp_path)
        systemctl_bus.set_use_busctl(False)
        try:
            systemd_generate.run(["deploy", "--use-busctl"], tmp_path)
            assert systemctl_bus.use_busctl() is True
        finally:
            systemctl_bus.set_use_busctl(False)

    def test_without_the_flag_plain_systemctl_is_kept(self, tmp_path, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        self._bare_checkout(tmp_path)
        systemctl_bus.set_use_busctl(False)
        try:
            systemd_generate.run(["deploy"], tmp_path)
            assert systemctl_bus.use_busctl() is False
        finally:
            systemctl_bus.set_use_busctl(False)
