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
    step = 3
    reads = []

    def clock():
        reads.append(len(reads) * step)
        return reads[-1]

    monkeypatch.setattr(compass_db, "_psql", serve(["1"], killed))
    monkeypatch.setattr(compass_db.time, "sleep", lambda _: None)
    monkeypatch.setattr(compass_db.time, "monotonic", clock)
    assert not compass_db.check_connections({}, "db", kill=True)
    # The wait spends the window before it gives up, and gives up soon after:
    # what matters is how long it waited, not how many rounds it took.
    assert max(reads) >= compass_db.DRAIN_SECONDS
    assert max(reads) < compass_db.DRAIN_SECONDS + 2 * step


def test_the_wait_reports_the_connections_it_is_waiting_for(monkeypatch, capsys):
    killed = []
    monkeypatch.setattr(compass_db, "_psql", serve(["2", "1", "0"], killed))
    monkeypatch.setattr(compass_db.time, "sleep", lambda _: None)
    assert compass_db.check_connections({}, "db", kill=True)
    said = capsys.readouterr().out
    # One line per count it saw, and no line for a count that had not moved.
    assert said.count("Waiting for") == 2
    assert "2 connection(s)" in said
    assert "1 connection(s)" in said


def test_a_wait_that_gives_up_does_not_tell_a_killing_caller_to_kill(monkeypatch, capfd):
    killed = []
    monkeypatch.setattr(compass_db, "_psql", serve(["1", "1"], killed))
    monkeypatch.setattr(compass_db.time, "sleep", lambda _: None)
    # A window of nothing spends itself on the first round.
    monkeypatch.setattr(compass_db, "DRAIN_SECONDS", 0)
    assert not compass_db.check_connections({}, "db", kill=True)
    err = capfd.readouterr().err
    assert "--kill" not in err
    assert "did not close within" in err


def test_without_kill_an_active_connection_fails_at_once(monkeypatch):
    killed = []
    monkeypatch.setattr(compass_db, "_psql", serve(["1"], killed))
    assert not compass_db.check_connections({}, "db", kill=False)
    assert killed == []
