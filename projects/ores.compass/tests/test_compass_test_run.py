"""
Tests for compass.py's `compass test run` pre-flight.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_test_run.py -v
No live broker, database or build tree required: the port probe and the
environment map are monkeypatched.
"""

import sys
from pathlib import Path

import pytest

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass


@pytest.fixture
def env(monkeypatch):
    monkeypatch.setattr(compass, "_read_env_map",
                        lambda: {"ORES_NATS_PORT": "21405"})


def test_preflight_passes_when_the_broker_listens(env, monkeypatch):
    monkeypatch.setattr(compass, "_broker_is_listening", lambda port: True)

    assert compass._test_run_preflight() is True


def test_preflight_names_the_fix_when_the_broker_is_down(env, monkeypatch, capsys):
    monkeypatch.setattr(compass, "_broker_is_listening", lambda port: False)

    assert compass._test_run_preflight() is False

    printed = capsys.readouterr().err
    assert "compass services start" in printed
    assert "21405" in printed
