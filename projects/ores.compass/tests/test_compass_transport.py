"""
Tests for the CLI's one-time adoption of the .env transport choice.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_transport.py -v
No live systemd, database or network access required.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass
import systemctl_bus


class TestAdoptFromEnvFile:
    """compass reads .env into a dict and never writes os.environ.

    systemctl_bus reads os.environ, so the CLI adopts the file's choice once
    before dispatch. Without that handover the value does nothing at all."""

    def setup_method(self):
        systemctl_bus.set_use_busctl(False)

    def teardown_method(self):
        systemctl_bus.set_use_busctl(False)

    def test_the_file_value_turns_the_bus_on(self, tmp_path, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        (tmp_path / ".env").write_text("ORES_USE_BUSCTL=1\n")
        compass._adopt_transport_from_env_file(tmp_path / ".env")
        assert systemctl_bus.use_busctl() is True

    def test_an_absent_file_is_not_an_error(self, tmp_path, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        compass._adopt_transport_from_env_file(tmp_path / ".env")
        assert systemctl_bus.use_busctl() is False

    def test_a_file_without_the_key_leaves_it_off(self, tmp_path, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        (tmp_path / ".env").write_text("ORES_ENV_NAME=elsewhere\n")
        compass._adopt_transport_from_env_file(tmp_path / ".env")
        assert systemctl_bus.use_busctl() is False

    def test_a_false_value_leaves_it_off(self, tmp_path, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        (tmp_path / ".env").write_text("ORES_USE_BUSCTL=0\n")
        compass._adopt_transport_from_env_file(tmp_path / ".env")
        assert systemctl_bus.use_busctl() is False


class TestMainAdoptsBeforeDispatch:
    """The centre runs for every command, not only the two pillars.

    Three of the five systemctl consumers have no flag of their own, so the
    only thing that makes them correct is this call landing before the
    dispatch branches."""

    def test_main_adopts_before_dispatching(self, monkeypatch):
        seen = []
        monkeypatch.setattr(compass, "_adopt_transport_from_env_file",
                            lambda path: seen.append(path))
        monkeypatch.setattr(compass, "cmd_capture", lambda argv: 0)
        monkeypatch.setattr(sys, "argv", ["compass", "capture", "list"])
        try:
            compass.main()
        except SystemExit:
            pass
        assert seen == [compass.PROJECT_ROOT / ".env"]


class TestEveryConsumerAdopts:
    """A module that drives systemd must adopt the .env choice.

    This defect shipped once: two pillars adopted the setting and five other
    callers did not, so a sandboxed seat still hit the unreachable manager
    for `compass nats ensure`, `compass site start` and `compass claude`.
    The rule is that compass.py adopts once before dispatch and is the only
    route to compass_claude, while systemctl_bus owns the transport. Every
    other module that calls systemctl_bus.run must hand the dict over."""

    CENTRE = {"compass.py", "compass_claude.py", "systemctl_bus.py"}

    def test_no_module_drives_systemd_without_adopting(self):
        src = Path(__file__).parent.parent / "src"
        offenders = []
        for path in sorted(src.glob("*.py")):
            if path.name in self.CENTRE:
                continue
            text = path.read_text(encoding="utf-8")
            if ("systemctl_bus.run(" in text
                    and "adopt_transport_setting" not in text):
                offenders.append(path.name)
        assert offenders == [], (
            "these modules call systemctl_bus.run without adopting the .env "
            "transport choice: " + ", ".join(offenders))
