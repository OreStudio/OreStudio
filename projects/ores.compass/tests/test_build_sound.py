"""A build says when it starts and how it ends, in the desktop's own sounds.

No audio is played here: the seam is the command the build would run and the
spawn call, so the choice of sound and the decision to keep off the build's
critical path are both asserted without a sound server.
"""

import subprocess
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))
import compass  # noqa: E402


def theme_dir(tmp_path, names=None):
    """A stand-in for the desktop's sound theme holding the named files."""
    directory = tmp_path / "stereo"
    directory.mkdir()
    for name in names if names is not None else compass.BUILD_SOUND_FILES.values():
        (directory / name).write_bytes(b"OggS")
    return directory


@pytest.fixture(autouse=True)
def sounds_on(monkeypatch):
    monkeypatch.setenv("ORES_BUILD_SOUND", "1")


@pytest.fixture
def a_player(monkeypatch):
    """The one player this machine has, and only that one."""
    monkeypatch.setattr(compass.shutil, "which",
                        lambda name: "/usr/bin/pw-play" if name == "pw-play" else None)


def test_a_start_names_the_desktops_information_sound(tmp_path, a_player):
    cmd = compass.build_sound_command("start", theme_dir(tmp_path))
    assert cmd is not None
    assert cmd[-1].endswith("dialog-information.oga")


def test_a_green_build_names_the_desktops_complete_sound(tmp_path, a_player):
    cmd = compass.build_sound_command("succeeded", theme_dir(tmp_path))
    assert cmd is not None
    assert cmd[-1].endswith("complete.oga")


def test_a_red_build_names_the_desktops_error_sound(tmp_path, a_player):
    cmd = compass.build_sound_command("failed", theme_dir(tmp_path))
    assert cmd is not None
    assert cmd[-1].endswith("dialog-error.oga")


def test_the_three_events_are_three_different_sounds(tmp_path, a_player):
    theme = theme_dir(tmp_path)
    played = {event: compass.build_sound_command(event, theme)[-1]
              for event in compass.BUILD_SOUND_FILES}
    assert len(set(played.values())) == 3


def test_the_outcome_words_map_onto_the_events():
    assert compass.build_sound_event("succeeded") == "succeeded"
    assert compass.build_sound_event("failed, exit 2") == "failed"
    assert compass.build_sound_event("interrupted") == "failed"


def test_a_missing_theme_file_makes_no_sound(tmp_path, a_player):
    theme = theme_dir(tmp_path, names=["complete.oga"])
    assert compass.build_sound_command("start", theme) is None
    assert compass.build_sound_command("succeeded", theme) is not None


def test_a_machine_with_no_player_makes_no_sound(tmp_path, monkeypatch):
    monkeypatch.setattr(compass.shutil, "which", lambda name: None)
    assert compass.build_sound_command("start", theme_dir(tmp_path)) is None


def test_the_first_player_the_machine_has_is_the_one_used(tmp_path, monkeypatch):
    monkeypatch.setattr(compass.shutil, "which",
                        lambda name: f"/usr/bin/{name}" if name == "ffplay" else None)
    cmd = compass.build_sound_command("start", theme_dir(tmp_path))
    assert cmd is not None
    assert Path(cmd[0]).name == "ffplay"
    assert "-autoexit" in cmd


def test_the_sounds_can_be_turned_off(tmp_path, a_player, monkeypatch):
    monkeypatch.setenv("ORES_BUILD_SOUND", "0")
    assert compass.build_sound_command("start", theme_dir(tmp_path)) is None


def test_the_env_file_can_turn_them_off(tmp_path, a_player, monkeypatch):
    monkeypatch.delenv("ORES_BUILD_SOUND")
    monkeypatch.setattr(compass, "_read_env_map", lambda: {"ORES_BUILD_SOUND": "off"})
    assert compass.build_sound_command("start", theme_dir(tmp_path)) is None


def test_the_env_file_leaves_them_on_when_it_says_nothing(
        tmp_path, a_player, monkeypatch):
    monkeypatch.delenv("ORES_BUILD_SOUND")
    monkeypatch.setattr(compass, "_read_env_map", lambda: {})
    assert compass.build_sound_command("start", theme_dir(tmp_path)) is not None


def test_an_event_the_build_does_not_make_makes_no_sound(tmp_path, a_player):
    assert compass.build_sound_command("halfway", theme_dir(tmp_path)) is None


def test_the_player_is_spawned_detached_so_the_build_does_not_wait(
        monkeypatch, a_player):
    spawned = []
    monkeypatch.setattr(compass.subprocess, "Popen",
                        lambda cmd, **kwargs: spawned.append((cmd, kwargs)))
    assert compass.play_build_sound("start")
    assert len(spawned) == 1
    _, kwargs = spawned[0]
    assert kwargs["start_new_session"] is True
    assert kwargs["stdout"] is subprocess.DEVNULL
    assert kwargs["stderr"] is subprocess.DEVNULL


def test_a_player_that_cannot_start_does_not_reach_the_build(monkeypatch, a_player):
    def refuse(cmd, **kwargs):
        raise OSError("no sound server")

    monkeypatch.setattr(compass.subprocess, "Popen", refuse)
    assert compass.play_build_sound("start") is False
