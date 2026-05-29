#!/usr/bin/env python3
"""
ores.codegen entry point. Invoke via codegen.sh (which activates the venv).

Usage: ./codegen.sh <subcommand> [options]
       ./codegen.sh --help
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent / "src"))

from codegen.cli import main  # noqa: E402

if __name__ == "__main__":
    main()
