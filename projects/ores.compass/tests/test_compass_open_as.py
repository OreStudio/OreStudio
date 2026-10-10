"""
compass open-as picks an Acme person and builds the sign-in it fills.

The staff come from the seeder dataset, so these checks read the real file.
No browser is started.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_open_as.py -v
"""

import struct
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / "projects" / "ores.compass" / "src"))

import compass_open_as as open_as  # noqa: E402


@pytest.fixture(scope="module")
def staff():
    return open_as.load_staff(ROOT)


def test_a_misspelt_name_finds_the_closest_person(staff):
    found = open_as.by_name(staff, "adrian vince")
    assert found[0]["username"] == "adrian.vance"


def test_a_username_finds_that_person(staff):
    assert [p["username"] for p in open_as.by_name(staff, "adrian.vance")] == [
        "adrian.vance"]


def test_a_title_in_london_returns_only_london_staff(staff):
    found = open_as.by_title(staff, "senior trader", open_as.DEFAULT_LOCATION)
    assert found
    assert all(p["job_title"] == "Senior Trader" for p in found)
    assert all(p["business_unit_code"].startswith("acme_uk.") for p in found)


def test_a_short_title_matches_senior_and_junior(staff):
    titles = {p["job_title"] for p in open_as.by_title(staff, "trader", "hk")}
    assert {"Senior Trader", "Junior Trader"} <= titles


def test_the_group_head_is_found_whatever_the_location(staff):
    found = open_as.by_title(staff, "group chief", "hk")
    assert [p["username"] for p in found] == ["adrian.vance"]


def test_a_role_in_hong_kong_returns_only_hong_kong_staff(staff):
    found = open_as.by_role(staff, "trading", "hk")
    assert found
    assert all(p["role"] == "Trading" for p in found)
    assert all(p["business_unit_code"].startswith("acme_hk.") for p in found)


def test_aliases_name_the_same_office(staff):
    assert open_as.location_prefix("HK") == open_as.location_prefix("hong kong")
    assert open_as.location_prefix("ny") == "acme_us"


def test_an_unknown_location_is_refused(staff):
    with pytest.raises(SystemExit):
        open_as.by_role(staff, "viewer", "paris")


def test_the_principal_carries_the_tenant_hostname(staff):
    person = open_as.by_name(staff, "adrian vance")[0]
    assert open_as.principal(person) == "adrian.vance@acme_corporation"


def test_a_client_frame_is_masked_and_carries_the_payload():
    key = b"\x01\x02\x03\x04"
    frame = open_as.encode_frame(b"hello", key)
    assert frame[0] == 0x81
    assert frame[1] == 0x80 | 5
    assert frame[2:6] == key
    assert bytes(b ^ key[i % 4] for i, b in enumerate(frame[6:])) == b"hello"


def test_a_long_frame_states_its_length_in_two_bytes():
    frame = open_as.encode_frame(b"x" * 300, b"\x00\x00\x00\x00")
    assert frame[1] == 0x80 | 126
    assert struct.unpack(">H", frame[2:4])[0] == 300


def test_a_shell_with_a_display_keeps_its_own_variables():
    environ = {"DISPLAY": ":7", "PATH": "/bin"}
    assert open_as.graphical_environment(environ) == environ


def test_a_wayland_desktop_is_found_from_its_socket(tmp_path):
    (tmp_path / "wayland-0").touch()
    (tmp_path / "wayland-0.lock").touch()
    found = open_as.graphical_environment({"PATH": "/bin"}, runtime_dir=tmp_path)
    assert found["WAYLAND_DISPLAY"] == "wayland-0"
    assert found["XDG_RUNTIME_DIR"] == str(tmp_path)


def test_the_profile_carries_the_persons_name(tmp_path):
    open_as.prepare_profile(tmp_path, False)
    open_as.name_profile(tmp_path, "Adrian Vance")
    state = open_as.json.loads((tmp_path / "Local State").read_text())
    prefs = open_as.json.loads((tmp_path / "Default" / "Preferences").read_text())
    assert state["profile"]["info_cache"]["Default"]["name"] == "Adrian Vance"
    assert prefs["profile"]["name"] == "Adrian Vance"
    assert prefs["credentials_enable_service"] is False


def test_naming_a_profile_keeps_what_chrome_already_wrote(tmp_path):
    (tmp_path / "Default").mkdir()
    (tmp_path / "Local State").write_text('{"profile": {"info_cache": {"Default": {"avatar_icon": "x"}}}}')
    open_as.name_profile(tmp_path, "Adrian Vance")
    state = open_as.json.loads((tmp_path / "Local State").read_text())
    assert state["profile"]["info_cache"]["Default"]["avatar_icon"] == "x"


def test_each_person_has_a_window_class_to_find_them_by():
    person = {"username": "ho.yin.tang"}
    assert open_as.window_class(person) == "ores-persona-ho.yin.tang"


def test_a_chrome_command_line_is_recognised_in_both_forms(tmp_path):
    spaced = f"/opt/google/chrome/chrome --user-data-dir={tmp_path} --no-first-run".encode()
    nul = f"chrome\0--user-data-dir={tmp_path}\0--no-first-run".encode()
    assert open_as.mentions_profile(spaced, tmp_path)
    assert open_as.mentions_profile(nul, tmp_path)


def test_a_longer_directory_name_is_not_taken_for_the_profile(tmp_path):
    other = f"chrome --user-data-dir={tmp_path}2 --no-first-run".encode()
    assert not open_as.mentions_profile(other, tmp_path)


def test_each_workspace_keeps_its_own_data_directory_per_person():
    person = {"username": "adrian.vance"}
    plain = open_as.profile_path("/x/env", person)
    three = open_as.profile_path("/x/env", person, 3)
    assert plain.name == three.name == "adrian.vance"
    assert three.parent.name == "workspace-3"
    assert plain != three


def test_the_profile_restores_the_previous_session(tmp_path):
    open_as.prepare_profile(tmp_path, False)
    prefs = open_as.json.loads((tmp_path / "Default" / "Preferences").read_text())
    assert prefs["session"]["restore_on_startup"] == 1


def test_the_tenant_administrator_signs_in_to_the_acme_tenant():
    assert open_as.principal(open_as.ADMINS["tenant_admin"]) == "tenant_admin@acme_corporation"


def test_the_system_administrator_signs_in_without_a_tenant():
    assert open_as.principal(open_as.ADMINS["super_admin"]) == "super_admin"
