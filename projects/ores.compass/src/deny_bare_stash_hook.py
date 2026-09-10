#!/usr/bin/env python3
"""PreToolUse hook: deny bare use of the shared git stash stack.

Every worktree shares one stack, so a bare push contends for its top and
a bare pop can take another session's work. See
doc/llm/claude_code_settings.org's Hooks section.
"""
import json
import re
import sys

# Each invocation in the command line, taken up to the next shell
# separator so a compound command is checked per part.
INVOCATION_RE = re.compile(r"(?:^|[\s&;(])git\s+stash\b([^&;|)]*)")
MESSAGE_RE = re.compile(r"(?:^|\s)(?:-m|--message)(?:[=\s]|$)")

# The forms that add an entry to the stack. Everything else either names
# the entry it acts on (apply, drop, show) or is not a stash command at
# all, which matters because the words appear in prose about this rule.
PUSHES = {"", "push", "save"}

DENIAL_MESSAGE = (
    "The stash stack is shared by every worktree in the fleet, so a bare "
    "`git stash` contends for its top and a bare `git stash pop` can take "
    "another session's work.\n"
    "Prefer a temporary WIP commit, which shares nothing. If you do need "
    "the stash: `git stash push -u -m \"<unique-tag>\"`, capture the SHA "
    "with `git stash list --format='%H %gs'`, and restore with "
    "`git stash apply <sha>` rather than pop. Drop the entry afterwards, "
    "re-finding it by tag.\n"
)


def denied(command: str) -> bool:
    """True when the command pushes onto or pops off the shared stack
    without naming what it acts on."""
    for m in INVOCATION_RE.finditer(command):
        rest = m.group(1)
        words = [w for w in rest.split() if not w.startswith("-")]
        sub = words[0] if words else ""
        if sub == "pop":
            return True
        if sub not in PUSHES:
            continue
        # A message is what makes the entry findable again by its own tag
        # rather than by its position on a stack others are pushing to.
        if not MESSAGE_RE.search(rest):
            return True
    return False


def main() -> int:
    try:
        data = json.load(sys.stdin)
    except (ValueError, json.JSONDecodeError):
        return 0
    if not isinstance(data, dict) or data.get("tool_name") != "Bash":
        return 0
    if denied(data.get("tool_input", {}).get("command", "")):
        sys.stderr.write(DENIAL_MESSAGE)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
