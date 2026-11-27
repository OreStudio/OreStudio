"""Tests for the shell's login flags, and the password that left .env.

Environment format version 38 removed ORES_SHELL_LOGIN_PASSWORD: it was the
seeded tenant_admin's well-known password, the same in every checkout, and a
person's credential. compass shell now reads it from the process environment
only, so a file that still carries one must not reach the shell.

Run::

    python3 -m pytest projects/ores.compass/tests/test_compass_shell_login.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.compass/src"))

import compass  # noqa: E402


ENV_WITH_THE_OLD_PASSWORD = {
    "ORES_SHELL_LOGIN_USERNAME": "tenant_admin@acme_corporation",
    "ORES_SHELL_LOGIN_PASSWORD": "Secure-Password-123",
}


def test_the_username_still_comes_from_the_env_file():
    flags = compass._login_arguments("", "", ENV_WITH_THE_OLD_PASSWORD, {}, False)

    assert flags == ["--login-username", "tenant_admin@acme_corporation"]


def test_a_password_in_the_env_file_is_ignored():
    flags = compass._login_arguments("", "", ENV_WITH_THE_OLD_PASSWORD, {}, False)

    assert "--login-password" not in flags


def test_an_exported_password_is_used():
    flags = compass._login_arguments(
        "", "", {}, {"ORES_SHELL_LOGIN_PASSWORD": "typed-by-a-person"}, False)

    assert flags == ["--login-password", "typed-by-a-person"]


def test_the_argument_wins_over_the_exported_password():
    flags = compass._login_arguments(
        "someone@acme_corporation", "from-the-argument", {},
        {"ORES_SHELL_LOGIN_PASSWORD": "from-the-environment"}, False)

    assert flags == ["--login-username", "someone@acme_corporation",
                     "--login-password", "from-the-argument"]


def test_a_logged_out_session_states_no_login():
    flags = compass._login_arguments(
        "someone@acme_corporation", "from-the-argument", ENV_WITH_THE_OLD_PASSWORD,
        {"ORES_SHELL_LOGIN_PASSWORD": "from-the-environment"}, True)

    assert flags == []
