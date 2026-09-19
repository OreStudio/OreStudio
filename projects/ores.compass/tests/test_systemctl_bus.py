"""
Tests for systemctl_bus.py (compass's --use-busctl transport).

Run with:  python -m pytest projects/ores.compass/tests/test_systemctl_bus.py -v
No live bus and no systemd access required: the busctl boundary is
monkeypatched and the parsers run against captured output.
"""

import json
import subprocess
import sys
from pathlib import Path

import pytest

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import systemctl_bus


@pytest.fixture(autouse=True)
def _systemctl_transport(monkeypatch):
    """Every test starts on the systemctl transport, chosen explicitly."""
    monkeypatch.setenv("ORES_USE_BUSCTL", "")
    systemctl_bus.set_use_busctl(False)
    yield
    systemctl_bus.set_use_busctl(False)


def _list_units_text(units):
    """Synthesise one ListUnits result: a(ssssssouso), ten fields each."""
    fields = []
    for name, description, load, active, sub in units:
        fields.extend([name, description, load, active, sub, "",
                       f"/org/freedesktop/systemd1/unit/{name}", "0", "",
                       "/"])
    body = " ".join(f'"{f}"' if i not in (7,) else f for i, f in enumerate(fields))
    return f"a(ssssssouso) {len(units)} {body}\n"


class TestParsers:
    def test_unit_name_escaping_matches_systemd(self):
        assert systemctl_bus._unit_path("ores.web.service-x") == (
            "/org/freedesktop/systemd1/unit/ores_2eweb_2eservice_2dx")

    def test_bare_fields_survive_the_tokenizer(self):
        text = ('a(ssssssouso) 1 "a.service" "desc" "loaded" "active" '
                '"running" "" "/org/x" 0 "" "/"\n')
        values = systemctl_bus._array_values(text)
        assert len(values) == 10
        assert values[0] == "a.service"
        assert values[6] == "/org/x"
        assert values[7] == "0"

    def test_array_header_is_dropped_without_a_newline(self):
        values = systemctl_bus._array_values('a(ss) 1 "/x/a.service" "enabled"')
        assert values == ["/x/a.service", "enabled"]

    def test_quoted_values_keep_escaped_quotes(self):
        assert systemctl_bus._parse_values(r's "a \"quoted\" word"') == [
            'a "quoted" word']


class TestIsActive:
    def test_active_unit_reports_the_state(self, monkeypatch):
        monkeypatch.setattr(systemctl_bus, "_get_property",
                            lambda *args: "active")
        result = systemctl_bus._is_active("ores.web.service")
        assert result.returncode == 0
        assert result.stdout == "active\n"

    def test_inactive_unit_is_reported_but_not_a_success(self, monkeypatch):
        values = iter(["loaded", "inactive"])
        monkeypatch.setattr(systemctl_bus, "_get_property",
                            lambda *args: next(values))
        result = systemctl_bus._is_active("ores.web.service")
        assert result.returncode == 3
        assert result.stdout == "inactive\n"

    def test_unknown_unit_stays_silent_so_compass_calls_it_missing(
            self, monkeypatch):
        monkeypatch.setattr(systemctl_bus, "_get_property",
                            lambda *args: "not-found")
        result = systemctl_bus._is_active("never-deployed.service")
        assert result.returncode == 3
        assert result.stdout == ""


class TestActions:
    def _job_started(self, monkeypatch, state, result_value=None):
        monkeypatch.setattr(
            systemctl_bus, "_method", lambda *a, **k: (0, 'o "/job/7"\n', ""))
        monkeypatch.setattr(systemctl_bus, "_wait_for_job", lambda *a: True)
        monkeypatch.setattr(systemctl_bus, "_get_property",
                            lambda *a: result_value or state)

    def test_start_succeeds_when_the_unit_reaches_active(self, monkeypatch):
        self._job_started(monkeypatch, "active")
        assert systemctl_bus._action("start", "x.service", 5).returncode == 0

    def test_start_fails_when_the_unit_did_not_start(self, monkeypatch):
        self._job_started(monkeypatch, "failed")
        result = systemctl_bus._action("start", "x.service", 5)
        assert result.returncode == 1
        assert "failed" in result.stderr

    def test_oneshot_success_is_read_from_the_service_result(self, monkeypatch):
        monkeypatch.setattr(
            systemctl_bus, "_method", lambda *a, **k: (0, 'o "/job/7"\n', ""))
        monkeypatch.setattr(systemctl_bus, "_wait_for_job", lambda *a: True)
        values = iter(["inactive", "success"])
        monkeypatch.setattr(systemctl_bus, "_get_property",
                            lambda *a: next(values))
        assert systemctl_bus._action("start", "x.service", 5).returncode == 0

    def test_stop_succeeds_when_the_unit_is_inactive(self, monkeypatch):
        self._job_started(monkeypatch, "inactive")
        assert systemctl_bus._action("stop", "x.service", 5).returncode == 0

    def test_a_job_that_never_finishes_times_out(self, monkeypatch):
        monkeypatch.setattr(
            systemctl_bus, "_method", lambda *a, **k: (0, 'o "/job/7"\n', ""))
        monkeypatch.setattr(systemctl_bus, "_wait_for_job", lambda *a: False)
        result = systemctl_bus._action("start", "x.service", 5)
        assert result.returncode == 1
        assert "Timed out" in result.stderr


class TestListings:
    def test_list_units_filters_by_type_and_emits_json(self, monkeypatch):
        text = _list_units_text([
            ("a.scope", "A scope", "loaded", "active", "running"),
            ("b.service", "A service", "loaded", "active", "running"),
        ])
        monkeypatch.setattr(systemctl_bus, "_method",
                            lambda *a, **k: (0, text, ""))
        result = systemctl_bus._list_units(
            ["--type=scope", "--no-legend", "--output=json"])
        entries = json.loads(result.stdout)
        assert [e["unit"] for e in entries] == ["a.scope"]
        assert entries[0]["active"] == "active"

    def test_list_unit_files_filters_by_pattern(self, monkeypatch):
        text = ('a(ss) 2 "/x/ores.web.service-eager.service" "disabled" '
                '"/x/other.service" "enabled"\n')
        monkeypatch.setattr(systemctl_bus, "_method",
                            lambda *a, **k: (0, text, ""))
        result = systemctl_bus._list_unit_files(["ores.web.service*"])
        assert "ores.web.service-eager.service" in result.stdout
        assert "other.service" not in result.stdout

    def test_unsupported_verb_says_so_rather_than_failing_silently(
            self, monkeypatch):
        result = systemctl_bus._dispatch(["enable", "x.service"], 5)
        assert result.returncode == 2
        assert "does not implement 'enable'" in result.stderr


class TestTransportSelection:
    def test_run_delegates_to_systemctl_when_busctl_is_off(self, monkeypatch):
        seen = {}

        def fake_run(argv, **kwargs):
            seen["argv"] = argv
            return subprocess.CompletedProcess(argv, 0, "", "")

        monkeypatch.setattr(systemctl_bus.subprocess, "run", fake_run)
        systemctl_bus.run(["--user", "is-active", "x.service"], check=False)
        assert seen["argv"] == ["systemctl", "--user", "is-active",
                                "x.service"]

    def test_process_environment_can_select_busctl(self, monkeypatch):
        monkeypatch.setenv("ORES_USE_BUSCTL", "1")
        assert systemctl_bus.use_busctl() is True

    def test_the_flag_selects_busctl(self):
        systemctl_bus.set_use_busctl(True)
        assert systemctl_bus.use_busctl() is True
