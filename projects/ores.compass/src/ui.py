#!/usr/bin/env python3
"""
Shared terminal UX helpers for compass commands.

Single source of truth for the compass colour palette, the standard
per-doc-type icons, and the small formatting helpers used across
subcommands (bearings, list, show, ...). Keep all new output styling
here so every command renders documents the same way:

  <icon>  <type>: <bold-cyan title> — <description>
      <yellow compass show UUID>
"""

# ── Colour palette ────────────────────────────────────────────────────────────
GREEN = "\033[32m"
YELLOW = "\033[33m"
RED = "\033[31m"
CYAN = "\033[36m"
BOLD = "\033[1m"
RESET = "\033[0m"


def header(text: str) -> str:
    """Render a section header in the standard bold cyan."""
    return f"{BOLD}{CYAN}{text}{RESET}"


def ycmd(cmd: str) -> str:
    """Render a compass command hint in the standard yellow."""
    return f"{YELLOW}{cmd}{RESET}"


# ── Standard doc-type icons ───────────────────────────────────────────────────
# One icon per #+type:. Aligned with existing usage (🧠 memories and 🏢
# product identity in bearings, 📓 journal). Fallback is the generic 📄
# used by compass show.
TYPE_ICONS = {
    "version": "🏷️",
    "sprint": "🏃",
    "story": "📖",
    "task": "☑️",
    "recipe": "📜",
    "runbook": "📋",
    "knowledge": "📚",
    "memory": "🧠",
    "capture": "📥",
    "investigation": "🔍",
    "skill": "🧩",
    "manual": "📕",
    "component": "📦",
    "diagram": "📐",
    "product_identity": "🏢",
}

DEFAULT_ICON = "📄"


def icon_for(doctype: str | None) -> str:
    """Standard icon for a #+type:, falling back to the generic doc icon."""
    return TYPE_ICONS.get((doctype or "").lower(), DEFAULT_ICON)
