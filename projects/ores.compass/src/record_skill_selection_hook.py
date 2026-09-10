#!/usr/bin/env python3
"""PreToolUse hook: record which skill the agent chose.

Feeds 'compass skills report'. See doc/llm/claude_code_settings.org's
Hooks section. Observing only: this hook never blocks the Skill tool,
so every failure path returns 0.
"""
import json
import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))


def main() -> int:
    try:
        data = json.load(sys.stdin)
    except (ValueError, json.JSONDecodeError):
        return 0
    if not isinstance(data, dict) or data.get("tool_name") != "Skill":
        return 0
    skill = data.get("tool_input", {}).get("skill")
    if not skill:
        return 0
    try:
        import compass_skills
        compass_skills.record_selection(
            data.get("cwd") or os.getcwd(), skill, data.get("session_id"))
    except Exception:
        return 0
    return 0


if __name__ == "__main__":
    sys.exit(main())
