#!/usr/bin/env python3
"""
Entry point for ores.codegen. Bootstraps the virtual environment when needed,
changes to the repository root, and delegates to src/codegen/cli.py.

Usage: python3 codegen.py <subcommand> [options]
       ./codegen.py --help
"""
import os
import sys
import subprocess
from pathlib import Path

_SCRIPT_DIR = Path(__file__).resolve().parent
_VENV_PYTHON = _SCRIPT_DIR / "venv" / "bin" / "python3"
_SRC_DIR = _SCRIPT_DIR / "src"


def _in_venv() -> bool:
    return Path(sys.executable).resolve() == _VENV_PYTHON.resolve()


def _bootstrap_venv() -> None:
    venv_dir = _SCRIPT_DIR / "venv"
    print(f"Virtual environment not found at {venv_dir}. Creating...")
    subprocess.check_call([sys.executable, "-m", "venv", str(venv_dir)])
    req = _SCRIPT_DIR / "requirements.txt"
    if req.exists():
        print("Installing dependencies...")
        subprocess.check_call(
            [str(venv_dir / "bin" / "pip"), "install", "-q", "-r", str(req)]
        )
    print("Virtual environment ready.")


def main() -> None:
    if not _VENV_PYTHON.exists():
        _bootstrap_venv()

    if not _in_venv():
        # Re-exec under the venv interpreter; os.execv replaces this process.
        os.execv(str(_VENV_PYTHON), [str(_VENV_PYTHON)] + sys.argv)

    # Change to repo root so that relative output paths (projects/...) resolve.
    repo_root = _SCRIPT_DIR.parent.parent
    os.chdir(str(repo_root))

    sys.path.insert(0, str(_SRC_DIR))
    from codegen.cli import main as cli_main  # noqa: PLC0415
    cli_main()


if __name__ == "__main__":
    main()
