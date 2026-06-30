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

import re
from pathlib import Path

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

# ── Search bucket affinity ────────────────────────────────────────────────────
# Maps each doctype to a semantic search bucket so compass search buckets
# are driven by this registry rather than a hardcoded set in compass.py.
# Doctypes NOT listed here fall through to the dynamic tail (auto-labelled
# from their type icon + name).  Temporal types (capture, story, task) are
# handled by separate sprint-aware logic and do not need an entry here.
BUCKET_AFFINITY: dict[str, str] = {
    "recipe":        "how_to",
    "runbook":       "how_to",
    "skill":         "how_to",
    "manual":        "how_to",
    "memory":        "how_to",
    "knowledge":     "knowledge",
    "investigation": "knowledge",
    "diagram":       "knowledge",
    "component":     "knowledge",
}


def icon_for(doctype: str | None) -> str:
    """Standard icon for a #+type:, falling back to the generic doc icon."""
    return TYPE_ICONS.get((doctype or "").lower(), DEFAULT_ICON)


# ── State-aware icons ─────────────────────────────────────────────────────────
# Tasks and stories carry a State field in their * Status table.  The type-only
# icon (☑️ for every task) is misleading when the task is still BACKLOG or
# STARTED.  Use these instead when the file path is available.

_STATE_RE = re.compile(r"^\|\s*State\s*\|\s*([A-Z]+)\s*\|", re.MULTILINE)

_STATE_ICONS: dict[str, dict[str, str]] = {
    "task": {
        "DONE":      "☑️",
        "STARTED":   "▶️",
        "BLOCKED":   "🔴",
        "BACKLOG":   "🔲",
        "DISCOVERED": "🔲",
        "ABANDONED": "❌",
    },
    "story": {
        "DONE":      "✅",
        "STARTED":   "📖",
        "BACKLOG":   "📖",
        "ABANDONED": "❌",
    },
}


def read_state(path) -> str | None:
    """Return the State value from a doc's Status table, or None if absent."""
    try:
        text = Path(path).read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return None
    m = _STATE_RE.search(text)
    return m.group(1) if m else None


def icon_for_doc(doctype: str | None, path=None) -> str:
    """State-aware icon: tasks and stories reflect their current State field.

    Falls back to icon_for(doctype) when the state is absent or unrecognised.
    """
    dt = (doctype or "").lower()
    if path and dt in _STATE_ICONS:
        state = read_state(path)
        if state:
            icon = _STATE_ICONS[dt].get(state)
            if icon:
                return icon
    return icon_for(doctype)
