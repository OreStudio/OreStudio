"""Capture a name-based report address that settles on the definition's id.

Signs in through the BFF, chooses the party, opens a name one definition
carries, then reports the address the browser ends on. Reuses the DevTools
helpers compass open-as ships.
"""

import base64
import json
import os
import shutil
import subprocess
import sys
import time
from urllib.parse import quote

ROOT = "/home/marco/Development/OreStudio/ores_dev_bright_faraday"
sys.path.insert(0, os.path.join(ROOT, "projects/ores.compass/src"))
import compass_open_as as oa  # noqa: E402

WEB = "http://127.0.0.1:20602"
PROFILE = "/tmp/reporting-name-profile"
SHOTS = os.path.join(ROOT, "build/screenshots")
USER = "tenant_admin@acme_corporation"
PASSWORD = "Secure-Password-123"
PARTY = "dc4de0fb-7736-4378-8d1f-4979df171062"
NAME = "Board Risk Dashboard"


def call(dt, method, params=None, timeout=90):
    dt.next_id += 1
    dt.send(json.dumps({"id": dt.next_id, "method": method, "params": params or {}}).encode())
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
    result = call(dt, "Page.captureScreenshot", {"format": "png", "captureBeyondViewport": True})
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
        time.sleep(2)
        print("login:", script(
            dt,
            "fetch('/api/session', {method:'POST', headers:{'Content-Type':'application/json'},"
            f" body: JSON.stringify({{username:'{USER}', password:'{PASSWORD}'}})}})"
            ".then(async (r) => r.status)",
        ))
        print("party:", script(
            dt,
            "fetch('/api/session/party', {method:'POST', headers:{'Content-Type':'application/json'},"
            f" body: JSON.stringify({{partyId:'{PARTY}'}})}})"
            ".then(async (r) => r.status)",
        ))

        asked = f"{WEB}/reporting/reports/{quote(NAME, safe='')}"
        print("asked for:", asked)
        call(dt, "Page.navigate", {"url": asked})
        time.sleep(1)
        page = wait_for_text(dt, "Definition")
        print("=== unique name resolves ===")
        print(page[:900])
        print("address after:", script(dt, "location.pathname"))
        print("title:", script(
            dt, "(document.querySelector('h1') || {innerText: '(no title)'}).innerText"))
        print("trail:", script(
            dt,
            "(document.querySelector('nav[aria-label=\"Breadcrumb\"]') || {innerText: ''})"
            ".innerText.replace(/\\s+/g, ' ').trim()",
        ))
        print("toast:", script(dt, "document.body.innerText.includes('Dismiss all')"))
        shot(dt, os.path.join(SHOTS, "reporting-name-resolves-to-id.png"))
        dt.close()
    finally:
        chrome.terminate()
        try:
            chrome.wait(timeout=10)
        except subprocess.TimeoutExpired:
            chrome.kill()


main()
