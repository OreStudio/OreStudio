#!/usr/bin/env python3
"""PreToolUse hook: deny compass.sh invocations that pipe or redirect
their output. See CLAUDE.md's "Never pipe or redirect a compass
command" rule and doc/llm/claude_code_settings.org's Hooks section.

Compass build/deploy commands print their own well-known log-file
path for exactly this purpose; tailing that file separately is the
supported way to follow progress instead.
"""
import json
import re
import sys

COMPASS_RE = re.compile(
    r"(?:^|[\s&;(])(?:\./)?(?:projects/ores\.compass/)?compass\.sh(?:\s|$)"
)
# A lone "|" (pipe) not adjacent to another "|" -- excludes the "||"
# logical-OR operator (e.g. `compass.sh build || echo failed`, a normal
# error-handling idiom that pipes nothing) while still catching a real
# single-pipe pipeline. Any ">" is a redirect (">", ">>", "2>&1" all
# contain it) and has no equivalent non-redirect meaning to exclude.
REDIRECT_RE = re.compile(r"(?<!\|)\|(?!\|)|>")

DENIAL_MESSAGE = (
    "Never pipe or redirect a compass command (no `|`, no `>`, no "
    "`2>&1`) -- run it bare. Compass build/deploy commands print their "
    "own well-known log-file path (e.g. \"\U0001F4DD Build output: "
    "/tmp/<label>_<target>_build.log (tail -f to follow)\"). To watch "
    "progress, tail THAT reported file as a separate, standalone "
    "command (e.g. `tail -f <path>`) -- never by piping the compass "
    "invocation itself. See CLAUDE.md.\n"
)


def main() -> int:
    try:
        data = json.load(sys.stdin)
    except (ValueError, json.JSONDecodeError):
        return 0
    if not isinstance(data, dict) or data.get("tool_name") != "Bash":
        return 0
    command = data.get("tool_input", {}).get("command", "")
    if COMPASS_RE.search(command) and REDIRECT_RE.search(command):
        sys.stderr.write(DENIAL_MESSAGE)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
