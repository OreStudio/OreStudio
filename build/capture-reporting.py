"""Capture the reporting screens from the running BFF, in headless Chrome.

Signs in through the BFF's own /api/session route (the headless form is not
driven), selects the party, then loads the Reporting area page, its list and
one definition's page, and writes a PNG of each. Uses the DevTools helpers that
compass open-as already ships, so nothing new is installed.
"""

import base64
import json
import os
import shutil
import subprocess
import sys
import time

ROOT = "/home/marco/Development/OreStudio/ores_dev_bright_faraday"
sys.path.insert(0, os.path.join(ROOT, "projects/ores.compass/src"))
import compass_open_as as oa  # noqa: E402

WEB = "http://127.0.0.1:20602"
PROFILE = "/tmp/reporting-capture-profile"
SHOTS = os.path.join(ROOT, "build/screenshots")
USER = "tenant_admin@acme_corporation"
PASSWORD = "Secure-Password-123"
PARTY = "dc4de0fb-7736-4378-8d1f-4979df171062"


def call(dt, method, params=None, timeout=90):
    dt.next_id += 1
    dt.send(
        json.dumps({"id": dt.next_id, "method": method, "params": params or {}}).encode()
    )
    deadline = time.time() + timeout
    while time.time() < deadline:
        answer = json.loads(oa.read_message(dt.source, lambda data: dt.send(data, 0xA)))
        if answer.get("id") != dt.next_id:
            continue
        if "error" in answer:
            raise RuntimeError(f"{method}: {answer['error']}")
        return answer.get("result", {})
    raise TimeoutError(method)


def script(dt, expression, timeout=90):
    result = call(
        dt,
        "Runtime.evaluate",
        {"expression": expression, "returnByValue": True, "awaitPromise": True},
        timeout,
    )
    if "exceptionDetails" in result:
        raise RuntimeError(json.dumps(result["exceptionDetails"])[:800])
    return result.get("result", {}).get("value")


def shot(dt, path):
    result = call(
        dt, "Page.captureScreenshot", {"format": "png", "captureBeyondViewport": True}
    )
    with open(path, "wb") as handle:
        handle.write(base64.b64decode(result["data"]))
    print("wrote", path, os.path.getsize(path), "bytes")


def wait_for_text(dt, needle, seconds=40):
    deadline = time.time() + seconds
    while time.time() < deadline:
        text = script(dt, "document.body.innerText")
        if text and needle in text:
            return text
        time.sleep(1)
    return script(dt, "document.body.innerText")


def wait_rows(dt, seconds=40):
    """Waits for the table to hold a row, so a shot is not of a loading list."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        count = script(dt, "document.querySelectorAll('tbody tr').length")
        if count:
            time.sleep(0.5)
            return count
        time.sleep(1)
    return 0


def trail(dt):
    """The words of the breadcrumb, so the capture states the path a person took."""
    return script(
        dt,
        "(document.querySelector('nav[aria-label=\"Breadcrumb\"]') || {innerText: '(no trail)'}).innerText"
        ".replace(/\\s+/g, ' ').trim()",
    )


def cards(dt):
    """The cards of an area page: the name, the line and the address of each."""
    return script(
        dt,
        "Array.from(document.querySelectorAll('main a[href]')).map(a => "
        "[a.innerText.split('\\n').map(w => w.trim()).filter(w => w), a.getAttribute('href')])",
    )


def main():
    shutil.rmtree(PROFILE, ignore_errors=True)
    os.makedirs(PROFILE)
    chrome = subprocess.Popen(
        [
            oa.chrome_binary(),
            "--headless=new",
            "--disable-gpu",
            "--no-sandbox",
            "--hide-scrollbars",
            "--no-first-run",
            "--no-default-browser-check",
            f"--user-data-dir={PROFILE}",
            "--remote-debugging-port=0",
            "--window-size=1600,1400",
            "about:blank",
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    try:
        for _ in range(60):
            if oa.devtools_port(PROFILE):
                break
            time.sleep(0.5)
        dt = oa.DevTools(oa.page_socket(PROFILE))
        call(dt, "Page.enable")
        call(dt, "Runtime.enable")
        call(dt, "Page.navigate", {"url": f"{WEB}/login"})
        time.sleep(3)

        origin = script(dt, "location.origin")
        print("origin:", origin)

        login = script(
            dt,
            "fetch('/api/session', {method:'POST', headers:{'Content-Type':'application/json'},"
            f" body: JSON.stringify({{username:'{USER}', password:'{PASSWORD}'}})}})"
            ".then(async (r) => r.status + ' ' + (await r.text()).slice(0, 200))",
        )
        print("login:", login)

        party = script(
            dt,
            "fetch('/api/session/party', {method:'POST', headers:{'Content-Type':'application/json'},"
            f" body: JSON.stringify({{partyId:'{PARTY}'}})}})"
            ".then(async (r) => r.status + ' ' + (await r.text()).slice(0, 200))",
        )
        print("party:", party)


        call(dt, "Page.navigate", {"url": f"{WEB}/reporting"})
        text = wait_for_text(dt, "Reports")
        print("=== area page text (first 1200 chars) ===")
        print(text[:1200])
        print("area trail:", trail(dt))
        print("area cards:", json.dumps(cards(dt), indent=1))
        shot(dt, os.path.join(SHOTS, "reporting-area.png"))

        call(dt, "Page.navigate", {"url": f"{WEB}/reporting/reports"})
        text = wait_for_text(dt, "parties you work in own")
        print("=== list page text (first 2200 chars) ===")
        print(text[:2200])
        print("list trail:", trail(dt))
        shot(dt, os.path.join(SHOTS, "reporting-list.png"))

        rows = script(
            dt,
            "fetch('/api/reporting/reports?limit=500').then(r => r.json()).then(a => ({"
            " total: a.total,"
            " unread: a.unread,"
            " rows: a.rows.map(x => ({id: x.id, name: x.name, partyName: x.partyName,"
            "  partyCode: x.partyCode, health: x.health, latestRun: x.latestRun})) }))",
        )
        print("list read:", json.dumps({k: rows[k] for k in ("total", "unread")}))
        print("parties in scope:", json.dumps(sorted({r["partyName"] for r in rows["rows"]})))
        print("names held by more than one party:", json.dumps(
            sorted({n for n in {r["name"] for r in rows["rows"]}
                    if len([r for r in rows["rows"] if r["name"] == n]) > 1})))
        print("first three rows:", json.dumps(rows["rows"][:3], indent=1))

        drawn = script(
            dt,
            "Array.from(document.querySelectorAll('tbody tr')).slice(0, 4).map("
            "r => Array.from(r.querySelectorAll('td')).map(c => c.innerText.trim()))",
        )
        print("first drawn rows:", json.dumps(drawn, indent=1))
        print("row href:", script(
            dt,
            "(document.querySelector('tbody tr a[href^=\"/reporting/reports/\"]') || "
            "{getAttribute: () => null}).getAttribute('href')",
        ))

        picked = script(
            dt,
            "fetch('/api/me/parties').then(r => r.json()).then(a => a.parties.map(p => "
            "[p.partyId, p.name, p.shortCode]))",
        )
        print("parties the session works in:", json.dumps(picked))

        # The party filter: narrow the list to the party that is not the login one.
        target = next((p for p in picked if p[0] != PARTY), None)
        if target is not None:
            call(dt, "Page.navigate", {"url": f"{WEB}/reporting/reports?f.party={target[0]}"})
            wait_rows(dt)
            print("=== list narrowed to", target[1], "===")
            print(script(dt, "document.body.innerText")[:900])
            kept = script(
                dt,
                "Array.from(document.querySelectorAll('tbody tr')).map("
                "r => r.innerText.replace(/\\s+/g, ' ').trim())",
            )
            print("rows after the filter:", json.dumps(kept, indent=1))
            shot(dt, os.path.join(SHOTS, "reporting-list-party.png"))

        # The identity proof: one report name, two parties, two definitions.
        shared = sorted({n for n in {r["name"] for r in rows["rows"]}
                         if len([r for r in rows["rows"] if r["name"] == n]) > 1})
        print("names held by more than one party:", json.dumps(shared))
        name = shared[0] if shared else None
        print("shared name:", name)
        if name:
            same = [r for r in rows["rows"] if r["name"] == name]
            print("definitions of that name:", json.dumps(
                [{k: r[k] for k in ("id", "partyName", "partyCode")} for r in same], indent=1))
            for index, row in enumerate(same[:2]):
                call(dt, "Page.navigate", {"url": f"{WEB}/reporting/reports/{row['id']}"})
                page = wait_for_text(dt, "Definition")
                print(f"=== detail {index} of {name}, in", row["partyName"], "===")
                print(page[:1000])
                print("detail trail:", trail(dt))
                print("detail title:", script(
                    dt,
                    "(document.querySelector('h1') || {innerText: '(no title)'}).innerText",
                ))
                print("detail tabs:", json.dumps(script(
                    dt,
                    "Array.from(document.querySelectorAll('[role=\"tab\"]'))"
                    ".map(a => a.innerText.trim())",
                )))
                print("detail fields:", json.dumps(script(
                    dt,
                    "Array.from(document.querySelectorAll('dt')).map("
                    "d => [d.innerText.trim(), (d.nextElementSibling || {innerText: ''}).innerText.trim()])",
                ), indent=1))
                shot(dt, os.path.join(
                    SHOTS, f"reporting-detail-{row['partyCode'] or index}.png"))

        # A shared name: the page is a chooser, not a refusal. The name holds a
        # slash, which is the case a name-based address has to survive.
        call(dt, "Page.navigate", {"url": f"{WEB}/reporting/reports/CVA%2FDVA%2FFVA"})
        page = wait_for_text(dt, "Choose the one you meant")
        print("=== shared name chooser ===")
        print(page[:1200])
        print("chooser trail:", trail(dt))
        print("chooser title:", script(
            dt, "(document.querySelector('h1') || {innerText: '(no title)'}).innerText"))
        print("chooser rows:", json.dumps(script(
            dt,
            "Array.from(document.querySelectorAll('tbody tr')).map(r => ({"
            " text: r.innerText.replace(/\\s+/g, ' ').trim(),"
            " href: (r.querySelector('a[href]') || {getAttribute: () => null})"
            ".getAttribute('href') }))",
        ), indent=1))
        print("chooser toast:", script(
            dt, "document.body.innerText.includes('Dismiss all')"))
        shot(dt, os.path.join(SHOTS, "reporting-choose-shared-name.png"))

        # A name no party has: the page says so, and no toast fires.
        call(dt, "Page.navigate", {"url": f"{WEB}/reporting/reports/No%2FSuch%2FReport"})
        page = wait_for_text(dt, "There is no such record")
        print("=== name nobody has ===")
        print(page[:900])
        print("not-found trail:", trail(dt))
        print("not-found toast:", script(
            dt, "document.body.innerText.includes('Dismiss all')"))
        shot(dt, os.path.join(SHOTS, "reporting-name-not-found.png"))

        # A name one definition carries: the record lands and the address
        # settles on the id's own.
        unique = next(
            (r for r in rows["rows"]
             if len([x for x in rows["rows"] if x["name"] == r["name"]]) == 1),
            None,
        )
        if unique is not None:
            from urllib.parse import quote
            call(dt, "Page.navigate", {
                "url": f"{WEB}/reporting/reports/{quote(unique['name'], safe='')}"})
            wait_for_text(dt, "Definition")
            time.sleep(1)
            print("=== unique name resolves to its id ===")
            print("asked for:", unique["name"], "expected id:", unique["id"])
            print("address after:", script(dt, "location.pathname"))
            print("detail title:", script(
                dt, "(document.querySelector('h1') || {innerText: '(no title)'}).innerText"))
            print("resolution toast:", script(
                dt, "document.body.innerText.includes('Dismiss all')"))
            shot(dt, os.path.join(SHOTS, "reporting-name-resolves-to-id.png"))
        dt.close()
    finally:
        chrome.terminate()
        try:
            chrome.wait(timeout=10)
        except subprocess.TimeoutExpired:
            chrome.kill()


main()
