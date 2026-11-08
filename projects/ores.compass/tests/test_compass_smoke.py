"""Tests for the ticks smoke check's verdict and log parsing.

The check needs a database and every service, so the part that decides is
kept pure: these tests feed it the evidence a run would collect and assert the
verdict. The 2026-10-23 failure is the case that matters: the service read no
routes, so the supervisor sent no demand and the check must say so.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_smoke.py -v
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass_smoke
from compass_smoke import demand, evidence, evaluate, latest_demand, parse_demand


def healthy():
    """The evidence of a system that ticks."""
    return evidence(
        tenant="acme_corporation",
        routes=4,
        subscriptions=4,
        ticks=128,
        first_tick="2026-10-24 10:00:00+00",
        last_tick="2026-10-24 10:00:05+00",
        future_ticks=0,
        demand=[demand("ore-simulated", 52, 0, 52)],
        waited_seconds=30,
    )


def test_a_healthy_system_passes():
    assert evaluate(healthy()) == []


def test_the_supervisor_line_is_read():
    text = (
        "2026-10-24 10:00:00 [info] Adapter 'ore-simulated' accepted 52 and rejected 0 of 52 id(s)\n"
        "2026-10-24 10:00:01 [info] Adapter 'bloomberg-b' accepted 3 and rejected 2 of 5 id(s)\n")

    lines = parse_demand(text)

    assert [(x.adapter, x.accepted, x.rejected, x.demanded) for x in lines] == [
        ("ore-simulated", 52, 0, 52),
        ("bloomberg-b", 3, 2, 5),
    ]


def test_the_last_reply_of_an_adapter_is_the_one_that_counts():
    text = (
        "Adapter 'ore-simulated' accepted 0 and rejected 0 of 0 id(s)\n"
        "Adapter 'ore-simulated' accepted 52 and rejected 0 of 52 id(s)\n")

    latest = latest_demand(parse_demand(text))

    assert len(latest) == 1
    assert latest[0].accepted == 52


def test_a_text_with_no_demand_line_yields_no_demand():
    assert parse_demand("nothing to see") == []


def test_no_demand_fails_as_the_route_defect_did():
    """The 2026-10-23 defect: routes invisible, so the supervisor sent nothing."""
    ev = healthy()
    ev.demand = []

    reasons = evaluate(ev)

    assert any("no adapter got a demand set" in reason for reason in reasons)


def test_an_empty_demand_fails():
    ev = healthy()
    ev.demand = [demand("ore-simulated", 0, 0, 0)]

    reasons = evaluate(ev)

    assert any("empty" in reason for reason in reasons)
    assert not any("no tick" in reason for reason in reasons)


def test_fewer_ids_accepted_than_demanded_fails():
    ev = healthy()
    ev.demand = [demand("ore-simulated", 40, 12, 52)]

    reasons = evaluate(ev)

    assert any("accepted 40 of the 52" in reason for reason in reasons)


def test_a_tenant_with_no_route_fails_before_the_tick_check():
    ev = healthy()
    ev.routes = 0
    ev.ticks = 0

    reasons = evaluate(ev)

    assert reasons[0].startswith("tenant 'acme_corporation' has no route")
    assert any("no tick arrived" in reason for reason in reasons)


def test_no_tick_fails():
    ev = healthy()
    ev.ticks = 0
    ev.first_tick = ""
    ev.last_tick = ""

    reasons = evaluate(ev)

    assert any("no tick arrived within 30s" in reason for reason in reasons)


def test_a_future_dated_tick_fails():
    ev = healthy()
    ev.future_ticks = 2

    reasons = evaluate(ev)

    assert any("2 tick(s) are dated in the future" in reason for reason in reasons)


def service_text_from(tmp_path, monkeypatch, files):
    """What the check reads when the journal is empty and @p files are on disk."""
    log_dir = tmp_path / "build" / "output" / "p" / "publish" / "log"
    log_dir.mkdir(parents=True)
    for name, text in files.items():
        (log_dir / name).write_text(text)

    class no_journal:
        returncode = 1
        stdout = ""

    monkeypatch.setattr(compass_smoke, "_run", lambda *a, **k: no_journal())
    env = {"ORES_PRESET": "p", "ORES_CHECKOUT_LABEL": "x"}
    return compass_smoke._service_text(tmp_path, env, 0.0)


def line_of(adapter):
    return f"Adapter '{adapter}' accepted 1 and rejected 0 of 1 id(s)\n"


def test_the_log_of_a_single_replica_service_is_read(tmp_path, monkeypatch):
    text = service_text_from(tmp_path, monkeypatch, {
        "ores.marketdata.service.0.log": line_of("demo-a"),
    })

    assert [x.adapter for x in parse_demand(text)] == ["demo-a"]


def test_every_replica_log_is_read(tmp_path, monkeypatch):
    text = service_text_from(tmp_path, monkeypatch, {
        "ores.marketdata.service.1.log": line_of("demo-a"),
        "ores.marketdata.service.2.log": line_of("demo-b"),
    })

    assert sorted(x.adapter for x in parse_demand(text)) == ["demo-a", "demo-b"]


def test_the_log_of_another_service_is_not_read(tmp_path, monkeypatch):
    text = service_text_from(tmp_path, monkeypatch, {
        "ores.marketdata.official.0.log": line_of("demo-a"),
    })

    assert parse_demand(text) == []
