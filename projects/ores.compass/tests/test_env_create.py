"""
Tests for env_create.py's provisioned .env skeleton.

Run with:  python -m pytest projects/ores.compass/tests/test_env_create.py -v
No worktree is created and no git command runs: the skeleton is a pure
function, and the values it sets are the ones compass env configure then
preserves.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import env_create

TS = "2026-09-26 00:00:00 UTC"


def _skeleton(**overrides):
    args = dict(name="brave-hopper", env_type="full", base_port=20000,
                nats_port=20405, nats_monitor_port=20406, use_busctl=True,
                timestamp=TS)
    args.update(overrides)
    return env_create._skeleton_env(**args)


class TestSkeletonEnv:
    def test_the_busctl_transport_is_on_by_default(self):
        assert "ORES_USE_BUSCTL=1\n" in _skeleton()

    def test_the_transport_can_be_switched_off(self):
        assert "ORES_USE_BUSCTL=0\n" in _skeleton(use_busctl=False)

    def test_the_pre_assigned_values_survive(self):
        text = _skeleton()
        assert "ORES_ENV_NAME=brave-hopper\n" in text
        assert "ORES_PROVISION_TYPE=full\n" in text
        assert "ORES_BASE_PORT=20000\n" in text
        assert "ORES_NATS_PORT=20405\n" in text
        assert "ORES_NATS_MONITOR_PORT=20406\n" in text

    def test_the_file_is_marked_as_secret_bearing(self):
        assert "DO NOT COMMIT" in _skeleton()
