"""The credential check catches a public source key that a file would commit.

Run::

    python3 -m pytest build/scripts/tests/test_check_no_committed_credentials.py

A key pasted into a seed, a test or a note compiles and passes every other check, and
it reaches the remote with the next push. The check has to name the file and the
variable, never print the value, and leave a clean tree alone.
"""
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "build/scripts"))

import check_no_committed_credentials as check  # noqa: E402

KEY = "a1b2c3d4e5f6a7b8c9d0"


def _repo(tmp_path: Path) -> Path:
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    (tmp_path / ".gitignore").write_text(".env\n")
    (tmp_path / ".env").write_text(
        "# Public source credentials\n"
        f"ORES_MARKETDATA_BANXICO_API_KEY={KEY}\n"
        "ORES_MARKETDATA_SERVICE_DB_PASSWORD=generated-password\n")
    return tmp_path


def test_a_clean_tree_passes(tmp_path, capsys):
    root = _repo(tmp_path)
    (root / "seed.sql").write_text("credential_ref = 'env:ORES_MARKETDATA_BANXICO_API_KEY'\n")

    assert check.main(root) == 0


def test_a_file_holding_a_key_fails_and_names_the_variable_not_the_value(tmp_path, capsys):
    root = _repo(tmp_path)
    (root / "seed.sql").write_text(f"token = '{KEY}'\n")

    assert check.main(root) == 1
    out = capsys.readouterr().out
    assert "seed.sql: holds the value of ORES_MARKETDATA_BANXICO_API_KEY" in out
    assert KEY not in out


def test_the_generated_service_passwords_are_not_searched(tmp_path):
    root = _repo(tmp_path)
    (root / "notes.txt").write_text("generated-password\n")

    assert check.main(root) == 0


def test_a_checkout_without_env_passes(tmp_path):
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)

    assert check.main(tmp_path) == 0


def test_short_values_and_user_names_are_not_searched_and_quotes_are_dropped():
    found = check.credentials(
        'ORES_MARKETDATA_X_API_KEY="abcdefgh12"\nORES_MARKETDATA_Y_PASSWORD=ab\n'
        'ORES_MARKETDATA_Z_USER=someone@example.com\n')

    assert found == {"ORES_MARKETDATA_X_API_KEY": "abcdefgh12"}
