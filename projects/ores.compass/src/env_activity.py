"""compass env activity — one-off environment setup activities.

Sibling mechanism to env_init's .env-format version log (env_init.py's
current_version/new_version), for changes that need a manual, often
privileged, per-checkout step that `compass env configure` cannot safely
re-apply on every regeneration. Canonical log lives at
doc/knowledge/architecture/environment_activity_log.org; ORES_ENV_ACTIVITY
in .env records the highest activity number a checkout has acknowledged.
See that doc's own "Detail" section for the full rationale.
"""

import argparse
import sys
from datetime import datetime, timezone
from pathlib import Path

from env_init import _read_env  # private, shared on purpose (see env_create.py:53)


def _activity_log_doc(project_root: Path) -> Path:
    return (project_root / "doc" / "knowledge" / "architecture" /
            "environment_activity_log.org")


def _parse_activity_rows(doc: Path):
    """Parse the activity table: [(number:int, date:str, title:str, recipe_id:str), ...]."""
    rows = []
    for line in doc.read_text().splitlines():
        s = line.strip()
        if not s.startswith("|"):
            continue
        cells = [c.strip() for c in s.strip("|").split("|")]
        if cells and cells[0].isdigit():
            rows.append((int(cells[0]),
                         cells[1] if len(cells) > 1 else "",
                         cells[2] if len(cells) > 2 else "",
                         cells[3] if len(cells) > 3 else ""))
    return rows


def current_activity(project_root: Path) -> int:
    """Highest activity number in the log — 0 if the table is empty."""
    rows = _parse_activity_rows(_activity_log_doc(project_root))
    if not rows:
        return 0
    return max(r[0] for r in rows)


def checkout_activity(project_root: Path) -> int:
    """A checkout's own acknowledged activity number, from .env (0 if unset/missing)."""
    env = _read_env(project_root / ".env")
    raw = env.get("ORES_ENV_ACTIVITY", "0")
    try:
        return int(raw)
    except ValueError:
        return 0


def pending(project_root: Path, current_checkout: int):
    """Activity rows strictly newer than a checkout's acknowledged number."""
    rows = _parse_activity_rows(_activity_log_doc(project_root))
    return sorted((r for r in rows if r[0] > current_checkout), key=lambda r: r[0])


def new_activity(project_root: Path, title: str, recipe_id: str) -> int:
    """compass env activity new — append a new activity row.

    Computes the next number (current + 1), appends a dated row linking to
    the recipe that carries the actual steps, and writes it back. The recipe
    should already exist — this only records that checkouts need to run it.
    """
    if "|" in title:
        print("Error: title must not contain '|' (it is a literal org-table "
              "column separator and would corrupt the log's row parsing).",
              file=sys.stderr)
        return 1
    doc = _activity_log_doc(project_root)
    if not doc.is_file():
        print(f"Error: activity log not found: {doc}", file=sys.stderr)
        return 1
    lines = doc.read_text().splitlines()
    table_idx = [i for i, ln in enumerate(lines) if ln.strip().startswith("|")]
    if not table_idx:
        print(f"Error: no activity table found in {doc}", file=sys.stderr)
        return 1
    nxt = current_activity(project_root) + 1
    date = datetime.now(timezone.utc).strftime("%Y-%m-%d")
    row = f"| {nxt} | {date} | {title} | {recipe_id.lower()} |"
    lines.insert(table_idx[-1] + 1, row)
    doc.write_text("\n".join(lines) + "\n")
    print(f"Recorded environment activity {nxt} ({date}): {title}")
    print(f"Other checkouts will see this in 'compass bearings' until they run "
          f"'compass env activity ack {nxt}'.")
    return 0


def ack(project_root: Path, n: int) -> int:
    """compass env activity ack <N> — record this checkout as having performed
    activity N (and everything at or below it). Deliberate per-checkout
    opt-in, never bumped automatically by `compass env configure`."""
    env_file = project_root / ".env"
    if not env_file.is_file():
        print(f"Error: {env_file} does not exist. Run 'compass env configure' first.",
              file=sys.stderr)
        return 1
    required = current_activity(project_root)
    if n > required:
        print(f"Error: activity {n} does not exist (highest recorded is {required}).",
              file=sys.stderr)
        return 1
    lines = env_file.read_text().splitlines()
    out = []
    found = False
    for line in lines:
        if line.startswith("ORES_ENV_ACTIVITY="):
            out.append(f"ORES_ENV_ACTIVITY={n}")
            found = True
        else:
            out.append(line)
    if not found:
        out.append(f"ORES_ENV_ACTIVITY={n}")
    env_file.write_text("\n".join(out) + "\n")
    env_file.chmod(0o600)
    print(f"Acknowledged environment activity {n}.")
    return 0


def run(argv: list[str], project_root: Path) -> int:
    """compass env activity [new|ack] — inspect or record environment activities."""
    if argv and argv[0] == "new":
        np = argparse.ArgumentParser(prog="compass env activity new",
                                     description="Record a new environment activity, "
                                                 "linking to the recipe with its steps.")
        np.add_argument("title", help="Short title (becomes the log row's description)")
        np.add_argument("recipe", help="UUID/prefix of the recipe carrying the steps")
        nargs = np.parse_args(argv[1:])
        return new_activity(project_root, nargs.title, nargs.recipe)
    if argv and argv[0] == "ack":
        ap = argparse.ArgumentParser(prog="compass env activity ack",
                                     description="Record this checkout as having "
                                                 "performed an environment activity.")
        ap.add_argument("number", type=int, help="Activity number to acknowledge")
        aargs = ap.parse_args(argv[1:])
        return ack(project_root, aargs.number)
    # Bare `activity`: list what's outstanding for this checkout.
    ap = argparse.ArgumentParser(prog="compass env activity",
                                 description="List environment activities outstanding "
                                             "for this checkout.")
    ap.parse_args(argv)
    current = checkout_activity(project_root)
    required = current_activity(project_root)
    print(f"This checkout: activity {current}  (log highest: {required})")
    outstanding = pending(project_root, current)
    if not outstanding:
        print("Nothing outstanding.")
        return 0
    print("Outstanding:")
    for number, date, title, recipe_id in outstanding:
        print(f"  {number}. {title}  ({date})  — compass show {recipe_id}")
    print(f"After performing them: compass env activity ack {required}")
    return 0
