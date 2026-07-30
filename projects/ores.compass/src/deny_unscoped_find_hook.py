#!/usr/bin/env python3
"""PreToolUse hook: deny `find` invocations rooted at `/` or another
filesystem-wide root. See doc/llm/claude_code_settings.org's Hooks
section.

Targeted finds (relative paths, or absolute paths narrower than a
top-level root) are unaffected -- only a bare filesystem-root scan is
denied.
"""
import json
import re
import sys

# Matches "find" as a command word: start of the whole command, or
# immediately after a shell operator (;, &, |, () skipping any
# whitespace -- deliberately NOT bare whitespace alone, which would
# also match "find" appearing as an ordinary English word inside an
# unrelated argument (e.g. a --description string mentioning "you'll
# find / at the root"). Followed by its first non-flag argument; flags
# (anything starting with "-") are skipped since find's path
# argument(s) may follow options like -H/-L/-P. Captures that first
# path argument.
FIND_RE = re.compile(
    r"(?:^|[&;|(]\s*)find\s+(?:-\w+\s+)*(\S+)"
)

# Filesystem-wide roots with no further subpath -- a scan starting here
# walks far more of the machine than any task in this repo needs.
UNSCOPED_ROOTS = {
    "/", "/home", "/usr", "/etc", "/var", "/proc", "/sys",
    "/root", "/opt", "/boot", "/dev", "/mnt", "/tmp",
}

DENIAL_MESSAGE = (
    "Unscoped `find` rooted at a filesystem-wide directory is denied -- "
    "scope it to the project directory (or another specific, narrower "
    "path) instead, or ask the user explicitly if a broader search is "
    "genuinely needed. See doc/llm/claude_code_settings.org.\n"
)


def main() -> int:
    try:
        data = json.load(sys.stdin)
    except (ValueError, json.JSONDecodeError):
        return 0
    if not isinstance(data, dict) or data.get("tool_name") != "Bash":
        return 0
    command = data.get("tool_input", {}).get("command", "")
    for match in FIND_RE.finditer(command):
        if match.group(1) in UNSCOPED_ROOTS:
            sys.stderr.write(DENIAL_MESSAGE)
            return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
