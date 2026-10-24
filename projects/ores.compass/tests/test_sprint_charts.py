"""compass sprint charts must count every commit and every PR of the window.

The collectors used to lose data in four ways: a bare date in --since means
that date at the current time of day, so the early hours of the first day were
dropped; plain --since stops walking at the first old commit of a rebased
history; the PR bar counted merge commits, which include merges of main into a
branch; and the PR cycle chart kept only the twenty slowest PRs. The cumulative
chart also counted ABANDONED stories as done, and the gnuplot scripts read
their CSVs from a fixed directory whatever --output-dir said.

Run with:  python -m pytest projects/ores.compass/tests/test_sprint_charts.py -v
No live database, network or gnuplot required.
"""

import datetime
import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
SRC = ROOT / "projects" / "ores.compass" / "src"
sys.path.insert(0, str(SRC))

import compass  # noqa: E402

D = datetime.date


def _git(repo, *args, env=None):
    full_env = dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t",
                    GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@t",
                    TZ="UTC")
    full_env.update(env or {})
    subprocess.check_call(["git", *args], cwd=repo, env=full_env,
                          stdout=subprocess.DEVNULL)


def _commit(repo, name, when, lines=1):
    (repo / name).write_text("x\n" * lines)
    _git(repo, "add", name)
    _git(repo, "commit", "-q", "-m", name,
         env={"GIT_AUTHOR_DATE": when, "GIT_COMMITTER_DATE": when})


@pytest.fixture
def repo(tmp_path, monkeypatch):
    _git(tmp_path, "init", "-q", "-b", "main")
    monkeypatch.setattr(compass, "PROJECT_ROOT", tmp_path)
    monkeypatch.setenv("TZ", "UTC")
    return tmp_path


def test_first_hour_of_first_day_and_last_hour_of_last_day_count(repo):
    _commit(repo, "a", "2026-10-12T23:59:00+00:00")
    _commit(repo, "b", "2026-10-13T00:05:00+00:00", lines=3)
    _commit(repo, "c", "2026-10-14T23:59:00+00:00")
    _commit(repo, "d", "2026-10-15T00:01:00+00:00")
    metrics = compass._sc_collect_daily_metrics(D(2026, 10, 13), D(2026, 10, 14), "main")
    assert metrics[D(2026, 10, 13)]["commits"] == 1
    assert metrics[D(2026, 10, 13)]["added"] == 3
    assert metrics[D(2026, 10, 14)]["commits"] == 1
    assert D(2026, 10, 12) not in metrics and D(2026, 10, 15) not in metrics


def test_a_day_with_no_commit_has_a_zero_row(repo):
    _commit(repo, "a", "2026-10-13T10:00:00+00:00")
    _commit(repo, "b", "2026-10-15T10:00:00+00:00")
    metrics = compass._sc_collect_daily_metrics(D(2026, 10, 13), D(2026, 10, 15), "main")
    assert sorted(metrics) == [D(2026, 10, 13), D(2026, 10, 14), D(2026, 10, 15)]
    assert metrics[D(2026, 10, 14)] == {"commits": 0, "prs": 0, "added": 0, "deleted": 0}


def test_a_rebased_history_with_an_old_commit_in_the_middle_is_fully_counted(repo):
    _commit(repo, "a", "2026-10-13T10:00:00+00:00")
    _commit(repo, "old", "2026-09-01T10:00:00+00:00")
    _commit(repo, "b", "2026-10-14T10:00:00+00:00")
    _commit(repo, "c", "2026-10-14T11:00:00+00:00")
    metrics = compass._sc_collect_daily_metrics(D(2026, 10, 13), D(2026, 10, 14), "main")
    assert sum(m["commits"] for m in metrics.values()) == 3


def test_merged_prs_are_bucketed_by_local_day_and_all_are_kept(monkeypatch):
    raw = [{"number": n, "title": "t", "createdAt": "2026-10-13T08:00:00Z",
            "mergedAt": merged}
           for n, merged in enumerate([
               "2026-10-12T22:30:00Z",
               "2026-10-13T09:00:00Z",
               "2026-10-13T09:00:00Z",
               "2026-10-14T23:30:00Z",
               "2026-10-15T00:30:00Z",
           ], start=1)]
    monkeypatch.setattr(compass.shutil, "which", lambda _: "/usr/bin/gh")
    monkeypatch.setattr(compass.subprocess, "check_output",
                        lambda *a, **k: json.dumps(raw))
    monkeypatch.setenv("TZ", "UTC")
    compass.time.tzset()
    prs = compass._sc_get_merged_prs(D(2026, 10, 13), D(2026, 10, 14))
    assert [pr["number"] for pr in prs] == [2, 3, 4]
    assert prs[0]["merged_at"] == "2026-10-13T09:00"
    assert prs[0]["merged_day"] == D(2026, 10, 13)


def test_more_than_twenty_prs_reach_the_cycle_csv(tmp_path):
    prs = [{"number": n, "title": "t", "merged_day": D(2026, 10, 13),
            "merged_at": f"2026-10-13T{n % 24:02d}:00", "cycle_hours": 0.0 if n % 2 else 1.5}
           for n in range(1, 101)]
    compass._sc_write_progress({}, prs, tmp_path, D(2026, 10, 13), D(2026, 10, 14))
    rows = (tmp_path / "pr_cycle_times.csv").read_text().splitlines()
    assert len(rows) == 101
    assert rows[0].split("\t") == ["pr_number", "title", "merged_at", "cycle_hours"]


SPRINT = """#+title: Sprint 99

* Stories

| Story | State | Start | End | Description |
|-------+-------+-------+-----+-------------|
| [[id:AAAA0000-0000-0000-0000-000000000001][One]] | DONE | | 2026-10-14 | d |
| [[id:AAAA0000-0000-0000-0000-000000000002][Two]] | ABANDONED | | 2026-10-14 | d |
| [[id:AAAA0000-0000-0000-0000-000000000003][Three]] | STARTED | 2026-10-13 | | d |
| [[id:AAAA0000-0000-0000-0000-000000000004][Four]] | DONE | | 2026-10-16 | d |
"""


def test_only_done_stories_count_and_every_day_has_a_row(tmp_path, monkeypatch):
    sprint_dir = tmp_path / "doc/agile/versions/v0/sprint_99"
    sprint_dir.mkdir(parents=True)
    (sprint_dir / "sprint.org").write_text(SPRINT)
    monkeypatch.setattr(compass, "PROJECT_ROOT", tmp_path)
    start, end = D(2026, 10, 13), D(2026, 10, 16)
    done = compass._sc_parse_sprint_stories(99, start, end)
    assert dict(done) == {D(2026, 10, 14): 1, D(2026, 10, 16): 1}
    compass._sc_write_progress(done, [], tmp_path, start, end)
    rows = (tmp_path / "sprint_progress.csv").read_text().splitlines()
    assert rows == ["day\tcumulative_done", "2026-10-13\t0", "2026-10-14\t1",
                    "2026-10-15\t1", "2026-10-16\t2"]


def test_gnuplot_reads_the_directory_the_csvs_were_written_to(tmp_path, monkeypatch):
    sprint_dir = tmp_path / "doc/agile/versions/v0/sprint_99"
    sprint_dir.mkdir(parents=True)
    (sprint_dir / "sprint.org").write_text(SPRINT)
    scripts = tmp_path / "scripts"
    scripts.mkdir()
    (scripts / "sprint_prs_commits.gnuplot").write_text("")
    out = tmp_path / "elsewhere"
    calls = []
    monkeypatch.setattr(compass, "PROJECT_ROOT", tmp_path)
    monkeypatch.setattr(compass.shutil, "which", lambda _: "/usr/bin/x")
    monkeypatch.setattr(compass, "_sc_collect_daily_metrics",
                        lambda s, e, b: {s: {"commits": 0, "prs": 0, "added": 0, "deleted": 0}})
    monkeypatch.setattr(compass, "_sc_get_merged_prs", lambda s, e: [])
    monkeypatch.setattr(compass.subprocess, "check_call",
                        lambda cmd, **k: calls.append(cmd))
    args = compass.argparse.Namespace(sprint=99, start_date="2026-10-13",
                                      end_date="2026-10-13", output_dir=str(out),
                                      branch=None)
    assert compass.cmd_sprint_charts(args) == 0
    assert (out / "sprint_activity.csv").exists()
    assert f"datadir='{out.resolve()}'" in calls[0]
