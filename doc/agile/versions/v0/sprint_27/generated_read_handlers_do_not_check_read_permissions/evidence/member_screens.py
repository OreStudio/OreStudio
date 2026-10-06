#!/usr/bin/env python3
"""Sign in to the web BFF as a member and read every route a member screen uses.

Run against a running deployment:

    python3 member_screens.py http://localhost:21402 ashley.moore@acme_corporation Ashley-Pass-123

Every route should answer 200: the member holds Member, and Member holds the
reads these screens make. A route that answers 403 or 500 names a read the
member's roles do not cover.
"""
import http.cookiejar
import json
import sys
import urllib.error
import urllib.request

base, username, password = sys.argv[1], sys.argv[2], sys.argv[3]
jar = http.cookiejar.CookieJar()
opener = urllib.request.build_opener(urllib.request.HTTPCookieProcessor(jar))


def call(method, path, body=None):
    data = None if body is None else json.dumps(body).encode()
    request = urllib.request.Request(base + path, data=data, method=method,
                                     headers={"content-type": "application/json"})
    try:
        with opener.open(request) as response:
            return response.status, json.loads(response.read() or b"null")
    except urllib.error.HTTPError as error:
        return error.code, error.read().decode(errors="replace")[:160]


status, _ = call("POST", "/api/session", {"username": username, "password": password})
print(f"{status}  POST /api/session")

status, resources = call("GET", "/api/refdata")
print(f"{status}  GET /api/refdata")
keys = [r["key"] for r in resources["resources"]] if status == 200 else []
status, lists = call("GET", "/api/classifications")
print(f"{status}  GET /api/classifications")
list_keys = []
if status == 200:
    rows = lists.get("lists", lists) if isinstance(lists, dict) else lists
    list_keys = [entry["key"] for entry in rows]

routes = [
    "/api/me/access", "/api/roles", "/api/permissions", "/api/me/contact-information",
    "/api/me/sessions", "/api/password-policy", "/api/change-reasons", "/api/image-map",
    "/api/labels", "/api/history?entityType=ores.refdata.currency&entityId=AED",
    f"/api/accounts/{username.split('@')[0]}/picture",
]
routes += [f"/api/refdata/{key}?offset=0&limit=5" for key in keys]
routes += [f"/api/classifications/{key}" for key in list_keys]
failures = 0
for route in routes:
    status, body = call("GET", route)
    if status != 200:
        failures += 1
    print(f"{status}  GET {route}" + ("" if status == 200 else f"  {body}"))
print(f"{len(routes)} routes read, {failures} not answered 200")
sys.exit(1 if failures else 0)
