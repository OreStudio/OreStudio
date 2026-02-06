#!/usr/bin/env python3
"""CI Health Dashboard for OreStudio GitHub Actions.

Produces an at-a-glance terminal report combining step timing,
cache health, and alerts using the ``gh`` CLI.

Requires: Python 3.10+, ``gh`` CLI authenticated with repo access.
No pip dependencies - stdlib only.
"""

import argparse
import json
import re
import subprocess
import statistics
import sys
from datetime import datetime, timezone
from typing import Any

# ── Constants ──────────────────────────────────────────────────────────

CI_WORKFLOWS = [
    "Canary Linux",
    "Continuous Linux",
    "Continuous Windows",
    "Continuous MacOS",
    "Nightly Build",
]

CACHE_LIMIT_BYTES = 10 * 1024**3  # 10 GiB GitHub limit

# Width of the report
WIDTH = 62

# ── Data fetching ──────────────────────────────────────────────────────


def get_repo_slug() -> str:
    """Derive owner/repo from git remote origin."""
    result = subprocess.run(
        ["git", "remote", "get-url", "origin"],
        capture_output=True, text=True, check=True,
    )
    url = result.stdout.strip()
    # Handle SSH (git@github.com:owner/repo.git) and HTTPS
    m = re.search(r"[/:]([^/:]+/[^/:]+?)(?:\.git)?$", url)
    if not m:
        sys.exit(f"Cannot parse repo slug from remote: {url}")
    return m.group(1)


def gh_api(
    endpoint: str,
    repo_slug: str | None = None,
    paginate: bool = False,
) -> Any:
    """Call ``gh api`` and return parsed JSON."""
    if repo_slug and not endpoint.startswith("/"):
        endpoint = f"/repos/{repo_slug}/{endpoint}"
    cmd = ["gh", "api", endpoint]
    if paginate:
        cmd.insert(2, "--paginate")
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"[WARN] gh api failed for {endpoint}: {result.stderr.strip()}",
              file=sys.stderr)
        return None
    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError:
        # --paginate can concatenate multiple JSON objects for array endpoints
        # Try to parse as newline-delimited JSON and merge
        items: list[Any] = []
        for line in result.stdout.strip().split("\n"):
            line = line.strip()
            if line:
                try:
                    items.append(json.loads(line))
                except json.JSONDecodeError:
                    pass
        return items if items else None


def fetch_workflows(repo_slug: str) -> list[dict]:
    """Fetch workflow definitions, filtered to CI_WORKFLOWS."""
    data = gh_api("actions/workflows", repo_slug)
    if not data or "workflows" not in data:
        return []
    return [
        w for w in data["workflows"]
        if w.get("name") in CI_WORKFLOWS
    ]


def fetch_recent_runs(
    repo_slug: str, workflow_id: int, count: int = 5,
) -> list[dict]:
    """Fetch the most recent *count* runs for a workflow."""
    data = gh_api(
        f"actions/workflows/{workflow_id}/runs?per_page={count}",
        repo_slug,
    )
    if not data or "workflow_runs" not in data:
        return []
    return data["workflow_runs"][:count]


def fetch_job_steps(repo_slug: str, run_id: int) -> list[dict]:
    """Fetch jobs (with steps) for a single run."""
    data = gh_api(f"actions/runs/{run_id}/jobs?per_page=100", repo_slug)
    if not data or "jobs" not in data:
        return []
    return data["jobs"]


def fetch_caches(repo_slug: str) -> list[dict]:
    """Fetch the full cache inventory via the REST API."""
    data = gh_api("actions/caches?per_page=100", repo_slug, paginate=True)
    if not data:
        return []
    # Handle paginated results
    if isinstance(data, list):
        caches: list[dict] = []
        for page in data:
            if isinstance(page, dict) and "actions_caches" in page:
                caches.extend(page["actions_caches"])
        return caches
    return data.get("actions_caches", [])


def fetch_run_log(run_id: int) -> str:
    """Fetch full run log text (slow). Used with ``--logs``."""
    result = subprocess.run(
        ["gh", "run", "view", str(run_id), "--log"],
        capture_output=True, text=True,
    )
    if result.returncode != 0:
        return ""
    return result.stdout


# ── Analysis ───────────────────────────────────────────────────────────


def parse_iso(ts: str | None) -> datetime | None:
    """Parse an ISO-8601 timestamp from the GitHub API."""
    if not ts:
        return None
    ts = ts.replace("Z", "+00:00")
    return datetime.fromisoformat(ts)


def compute_step_durations(jobs: list[dict]) -> list[dict]:
    """Return a flat list of {job, step, duration_s} sorted by duration."""
    entries: list[dict] = []
    for job in jobs:
        job_name = job.get("name", "unknown")
        for step in job.get("steps", []):
            start = parse_iso(step.get("started_at"))
            end = parse_iso(step.get("completed_at"))
            if start and end:
                dur = (end - start).total_seconds()
                entries.append({
                    "job": job_name,
                    "step": step.get("name", "unknown"),
                    "duration_s": dur,
                })
    entries.sort(key=lambda e: e["duration_s"], reverse=True)
    return entries


def compute_run_duration(run: dict) -> float | None:
    """Return wall-clock duration of a run in seconds."""
    start = parse_iso(run.get("run_started_at") or run.get("created_at"))
    end = parse_iso(run.get("updated_at"))
    if start and end:
        return (end - start).total_seconds()
    return None


def classify_cache(key: str) -> str:
    """Classify a cache key into a human-readable type."""
    lower = key.lower()
    if lower.startswith("vcpkg"):
        return "vcpkg"
    if lower.startswith("sccache") or "ccache" in lower:
        return "sccache"
    if "qt" in lower or lower.startswith("install-qt"):
        return "Qt"
    # get-cmake caches are often purely numeric keys
    if lower.startswith("cmake") or re.match(r"^\d+$", key):
        return "cmake"
    return "other"


def compute_trend(durations: list[float]) -> tuple[str, str]:
    """Compute a trend indicator from a list of durations (newest first).

    Returns (symbol, description).
    """
    if len(durations) < 2:
        return ("\u2192", "stable")

    latest = durations[0]
    previous = durations[1:]
    if not previous:
        return ("\u2192", "stable")

    med = statistics.median(previous)
    if med == 0:
        return ("\u2192", "stable")

    pct = (latest - med) / med * 100
    delta = abs(latest - med)

    if pct > 25:
        return ("\u2191", f"+{format_duration(delta)} ({abs(pct):.0f}% slower)")
    if pct < -25:
        return ("\u2193", f"-{format_duration(delta)} ({abs(pct):.0f}% faster)")
    return ("\u2192", "stable")


def parse_sccache_stats(log_text: str) -> dict[str, Any]:
    """Extract sccache hit-rate information from log text."""
    stats: dict[str, Any] = {}
    # Look for sccache summary lines
    hit_match = re.search(
        r"cache hit rate:\s*([\d.]+)\s*%", log_text, re.IGNORECASE,
    )
    if hit_match:
        stats["hit_rate"] = float(hit_match.group(1))

    # Also look for ccache-action summary
    hit_match2 = re.search(
        r"Hits:\s*(\d+)\s*\|\s*Misses:\s*(\d+)", log_text, re.IGNORECASE,
    )
    if hit_match2:
        hits = int(hit_match2.group(1))
        misses = int(hit_match2.group(2))
        total = hits + misses
        if total > 0:
            stats["hit_rate"] = hits / total * 100
            stats["hits"] = hits
            stats["misses"] = misses
    return stats


def detect_xgha_warnings(log_text: str) -> bool:
    """Return True if x-gha removal warnings are found in logs."""
    return bool(re.search(r"x-gha.*removed|removed.*x-gha", log_text,
                          re.IGNORECASE))


def detect_alerts(
    workflows_data: list[dict],
    caches: list[dict],
    log_data: dict[str, Any] | None = None,
) -> list[dict]:
    """Generate alert entries from all collected data.

    Each alert is {severity, message} where severity is OK|INFO|WARN|FAIL.
    """
    alerts: list[dict] = []

    # ── Build failure ──
    for wd in workflows_data:
        latest = wd.get("latest_run")
        if latest and latest.get("conclusion") not in ("success", None):
            conclusion = latest.get("conclusion") or "unknown"
            alerts.append({
                "severity": "WARN",
                "message": (
                    f"{wd['name']} latest run {conclusion}"
                ),
            })

    # ── Slow build ──
    for wd in workflows_data:
        durations = wd.get("durations", [])
        if len(durations) >= 3:
            latest = durations[0]
            med = statistics.median(durations[1:])
            if med > 0 and latest > 2 * med:
                alerts.append({
                    "severity": "WARN",
                    "message": (
                        f"{wd['name']} latest run "
                        f"({format_duration(latest)}) is >2x median "
                        f"({format_duration(med)})"
                    ),
                })

    # ── Cache health ──
    cache_keys = {c.get("key", "") for c in caches}
    total_bytes = sum(c.get("size_in_bytes", 0) for c in caches)

    # Expected vcpkg cache configs
    expected_vcpkg = [
        "linux-gcc-debug", "linux-gcc-release",
        "linux-clang-debug", "linux-clang-release",
        "windows-msvc-debug", "windows-msvc-release",
        "macos-clang-debug", "macos-clang-release",
    ]
    missing_vcpkg = []
    for cfg in expected_vcpkg:
        if not any(k.startswith(f"vcpkg-{cfg}") for k in cache_keys):
            missing_vcpkg.append(cfg)
    if missing_vcpkg:
        lines = ", ".join(missing_vcpkg)
        alerts.append({
            "severity": "WARN",
            "message": f"vcpkg cache missing for: {lines}",
        })

    # Cache nearly full
    if total_bytes > 0.8 * CACHE_LIMIT_BYTES:
        alerts.append({
            "severity": "WARN",
            "message": (
                f"Cache usage at {format_size(total_bytes)} "
                f"({total_bytes / CACHE_LIMIT_BYTES * 100:.0f}% "
                f"of {format_size(CACHE_LIMIT_BYTES)} limit)"
            ),
        })
    else:
        alerts.append({
            "severity": "OK",
            "message": (
                f"Cache usage well within "
                f"{format_size(CACHE_LIMIT_BYTES)} limit"
            ),
        })

    # Stale caches (not accessed in >7 days)
    now = datetime.now(timezone.utc)
    stale: list[str] = []
    for c in caches:
        last = parse_iso(c.get("last_accessed_at"))
        if last and (now - last).days > 7:
            stale.append(c.get("key", "?"))
    if stale:
        alerts.append({
            "severity": "INFO",
            "message": (
                f"{len(stale)} stale cache(s) not accessed in >7 days"
            ),
        })

    # ── Log-based alerts (only when --logs is used) ──
    if log_data:
        found_xgha = False
        for wf_name, ld in log_data.items():
            if ld.get("xgha_warning"):
                found_xgha = True
                alerts.append({
                    "severity": "FAIL",
                    "message": (
                        f"x-gha removal warning found in {wf_name} logs"
                    ),
                })
            sccache = ld.get("sccache_stats", {})
            hit_rate = sccache.get("hit_rate")
            if hit_rate is not None and hit_rate < 80:
                alerts.append({
                    "severity": "WARN",
                    "message": (
                        f"sccache hit rate dropped to {hit_rate:.0f}% "
                        f"for {wf_name} (usually >90%)"
                    ),
                })
        if not found_xgha:
            alerts.append({
                "severity": "OK",
                "message": "No x-gha warnings in recent logs",
            })

    return alerts


# ── Formatting ─────────────────────────────────────────────────────────


def format_duration(seconds: float | None) -> str:
    """Format seconds into human-readable duration."""
    if seconds is None:
        return "-"
    total = int(seconds)
    if total < 0:
        return "-"
    h, remainder = divmod(total, 3600)
    m, s = divmod(remainder, 60)
    if h > 0:
        return f"{h}:{m:02d}:{s:02d}"
    return f"{m}:{s:02d}"


def format_size(size_bytes: int | float) -> str:
    """Format bytes into human-readable size."""
    if size_bytes >= 1024**3:
        return f"{size_bytes / 1024**3:.1f} GiB"
    if size_bytes >= 1024**2:
        return f"{size_bytes / 1024**2:.0f} MiB"
    if size_bytes >= 1024:
        return f"{size_bytes / 1024:.0f} KiB"
    return f"{size_bytes:.0f} B"


def format_age(iso_timestamp: str | None) -> str:
    """Format an ISO timestamp as a relative age string."""
    dt = parse_iso(iso_timestamp)
    if not dt:
        return "-"
    now = datetime.now(timezone.utc)
    delta = now - dt
    total_seconds = int(delta.total_seconds())
    if total_seconds < 0:
        return "just now"
    if total_seconds < 60:
        return f"{total_seconds}s ago"
    minutes = total_seconds // 60
    if minutes < 60:
        return f"{minutes}m ago"
    hours = minutes // 60
    if hours < 24:
        return f"{hours}h ago"
    days = hours // 24
    return f"{days}d ago"


def truncate(text: str, width: int) -> str:
    """Truncate text to width, adding ellipsis if needed."""
    if len(text) <= width:
        return text
    return text[: width - 3] + "..."


def print_header() -> None:
    print("\u2550" * WIDTH)
    title = "CI Health Report"
    pad = (WIDTH - len(title)) // 2
    print(" " * pad + title)
    print("\u2550" * WIDTH)
    print()


def print_section(title: str) -> None:
    dashes = WIDTH - len(title) - 4
    print(f"\u2500\u2500 {title} " + "\u2500" * max(dashes, 1))


def print_overview(workflows_data: list[dict]) -> None:
    print_section("Build Overview")
    # Header
    print(
        f"{'Workflow':<22} {'Branch':<20} {'Status':<8} "
        f"{'Duration':<10} Trend (last 5)"
    )
    for wd in workflows_data:
        name = truncate(wd["name"], 21)
        latest = wd.get("latest_run")
        if not latest:
            print(f"{name:<22} {'?':<20} {'?':<8} {'-':<10} -")
            continue

        branch = truncate(latest.get("head_branch", "?"), 19)
        conclusion = latest.get("conclusion") or "running"
        status = "pass" if conclusion == "success" else conclusion.upper()

        dur = wd["durations"][0] if wd.get("durations") else None
        dur_str = format_duration(dur) if status == "pass" else status

        durations = wd.get("durations", [])
        symbol, trend_desc = compute_trend(durations)

        print(
            f"{name:<22} {branch:<20} {status:<8} "
            f"{dur_str:<10} {symbol} {trend_desc}"
        )
    print()


def print_step_breakdown(
    workflow_name: str, jobs: list[dict],
) -> None:
    steps = compute_step_durations(jobs)
    if not steps:
        return

    total_s = sum(e["duration_s"] for e in steps)
    if total_s == 0:
        return

    # Group by job
    jobs_seen: dict[str, list[dict]] = {}
    for entry in steps:
        jobs_seen.setdefault(entry["job"], []).append(entry)

    for job_name, job_steps in jobs_seen.items():
        job_total = sum(e["duration_s"] for e in job_steps)
        if job_total == 0:
            continue
        print_section(f"Step Breakdown ({truncate(job_name, 35)})")
        print(
            f"{'Step':<32} {'Duration':<11} {'%':<6} Bar"
        )
        bar_max = 20
        for entry in job_steps[:15]:  # top 15 steps
            name = truncate(entry["step"], 31)
            dur = format_duration(entry["duration_s"])
            pct = entry["duration_s"] / job_total * 100
            bar_len = int(pct / 100 * bar_max)
            bar = "\u2588" * bar_len if bar_len > 0 else "\u258f"
            print(f"{name:<32} {dur:<11} {pct:<6.1f}{bar}")
        print()


def print_cache_inventory(caches: list[dict]) -> None:
    print_section("Cache Inventory")
    if not caches:
        print("  No caches found.")
        print()
        return

    print(
        f"{'Type':<10} {'Key':<30} {'Size':<10} "
        f"{'Age':<10} Last Used"
    )

    # Sort by type then key
    decorated = [
        (classify_cache(c.get("key", "")), c) for c in caches
    ]
    decorated.sort(key=lambda x: (x[0], x[1].get("key", "")))

    total_bytes = 0
    for cache_type, c in decorated:
        key = truncate(c.get("key", "?"), 29)
        size = format_size(c.get("size_in_bytes", 0))
        age = format_age(c.get("created_at"))
        last_used = format_age(c.get("last_accessed_at"))
        total_bytes += c.get("size_in_bytes", 0)
        print(
            f"{cache_type:<10} {key:<30} {size:<10} "
            f"{age:<10} {last_used}"
        )

    print("\u2500" * WIDTH)
    limit_str = format_size(CACHE_LIMIT_BYTES)
    total_str = format_size(total_bytes)
    pct = total_bytes / CACHE_LIMIT_BYTES * 100 if CACHE_LIMIT_BYTES else 0
    print(f"Total: {total_str} / {limit_str} ({pct:.0f}%)")
    print()


def print_alerts(alerts: list[dict]) -> None:
    print_section("Alerts")
    if not alerts:
        print("  [OK]   All checks passed")
        print()
        return

    # Sort: FAIL first, then WARN, then INFO, then OK
    severity_order = {"FAIL": 0, "WARN": 1, "INFO": 2, "OK": 3}
    alerts.sort(key=lambda a: severity_order.get(a["severity"], 99))

    for a in alerts:
        sev = a["severity"]
        msg = a["message"]
        # Wrap long messages
        prefix = f"[{sev}]"
        print(f"{prefix:<9}{msg}")
    print()


# ── Main ───────────────────────────────────────────────────────────────


def main() -> None:
    parser = argparse.ArgumentParser(
        description="CI Health Dashboard for OreStudio GitHub Actions.",
    )
    parser.add_argument(
        "--logs",
        action="store_true",
        help="Parse run logs for sccache stats and vcpkg warnings (slower)",
    )
    parser.add_argument(
        "--workflow",
        metavar="NAME",
        help="Show step breakdown only for this workflow (default: all)",
    )
    parser.add_argument(
        "--runs",
        type=int,
        default=5,
        help="Number of recent runs for trend analysis (default: 5)",
    )
    args = parser.parse_args()

    # ── Preflight ──
    repo_slug = get_repo_slug()

    # ── Fetch workflows ──
    workflows = fetch_workflows(repo_slug)
    if not workflows:
        sys.exit("No CI workflows found. Is `gh` authenticated?")

    # Order workflows to match CI_WORKFLOWS
    wf_order = {name: i for i, name in enumerate(CI_WORKFLOWS)}
    workflows.sort(key=lambda w: wf_order.get(w["name"], 99))

    # ── Collect data per workflow ──
    workflows_data: list[dict] = []
    for wf in workflows:
        wf_id = wf["id"]
        wf_name = wf["name"]
        runs = fetch_recent_runs(repo_slug, wf_id, args.runs)

        durations: list[float] = []
        for r in runs:
            d = compute_run_duration(r)
            if d is not None:
                durations.append(d)

        latest_run = runs[0] if runs else None
        jobs: list[dict] = []
        if latest_run:
            jobs = fetch_job_steps(repo_slug, latest_run["id"])

        workflows_data.append({
            "name": wf_name,
            "id": wf_id,
            "latest_run": latest_run,
            "durations": durations,
            "jobs": jobs,
        })

    # ── Fetch caches ──
    caches = fetch_caches(repo_slug)

    # ── Optional log analysis ──
    log_data: dict[str, Any] | None = None
    if args.logs:
        log_data = {}
        for wd in workflows_data:
            latest = wd.get("latest_run")
            if not latest:
                continue
            print(
                f"  Fetching logs for {wd['name']}...",
                file=sys.stderr, flush=True,
            )
            log_text = fetch_run_log(latest["id"])
            log_data[wd["name"]] = {
                "sccache_stats": parse_sccache_stats(log_text),
                "xgha_warning": detect_xgha_warnings(log_text),
            }

    # ── Alerts ──
    alerts = detect_alerts(workflows_data, caches, log_data)

    # ── Render ──
    print()
    print_header()
    print_overview(workflows_data)

    for wd in workflows_data:
        if args.workflow and wd["name"] != args.workflow:
            continue
        print_step_breakdown(wd["name"], wd.get("jobs", []))

    print_cache_inventory(caches)
    print_alerts(alerts)


if __name__ == "__main__":
    main()
