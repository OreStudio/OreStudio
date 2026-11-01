"""check_connections waits for the backends it kills to close."""
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))
import compass_db


def serve(counts, killed):
    """A stand-in for psql: each count reads the next value, each kill is recorded."""
    def fake(env, *args, **kwargs):
        sql = args[-1]
        if "pg_terminate_backend" in sql:
            killed.append(sql)
            return subprocess.CompletedProcess(args, 0, stdout="t\n", stderr="")
        value = counts.pop(0) if len(counts) > 1 else counts[0]
        return subprocess.CompletedProcess(args, 0, stdout=f"{value}\n", stderr="")
    return fake


def test_a_backend_still_closing_after_the_kill_does_not_fail_the_recreate(monkeypatch):
    killed = []
    monkeypatch.setattr(compass_db, "_psql", serve(["1", "0"], killed))
    monkeypatch.setattr(compass_db.time, "sleep", lambda _: None)
    assert compass_db.check_connections({}, "db", kill=True)
    assert len(killed) == 2
    assert all("pg_terminate_backend(pid, 2000)" in sql for sql in killed)


def test_a_backend_that_never_closes_fails_after_the_deadline(monkeypatch):
    killed = []
    clock = iter(range(0, 1000, 3))
    monkeypatch.setattr(compass_db, "_psql", serve(["1"], killed))
    monkeypatch.setattr(compass_db.time, "sleep", lambda _: None)
    monkeypatch.setattr(compass_db.time, "monotonic", lambda: next(clock))
    assert not compass_db.check_connections({}, "db", kill=True)
    assert 1 < len(killed) < 10


def test_without_kill_an_active_connection_fails_at_once(monkeypatch):
    killed = []
    monkeypatch.setattr(compass_db, "_psql", serve(["1"], killed))
    assert not compass_db.check_connections({}, "db", kill=False)
    assert killed == []
