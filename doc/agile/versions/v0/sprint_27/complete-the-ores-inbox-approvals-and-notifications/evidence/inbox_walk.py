#!/usr/bin/env python3
"""Walk the inbox approval and notification flow through the real BFF."""
import http.cookiejar
import json
import urllib.error
import urllib.request

BASE = "http://localhost:21402"


class Client:
    def __init__(self):
        self.jar = http.cookiejar.CookieJar()
        self.opener = urllib.request.build_opener(
            urllib.request.HTTPCookieProcessor(self.jar))

    def call(self, method, path, body=None):
        data = None if body is None else json.dumps(body).encode()
        request = urllib.request.Request(
            BASE + path, data=data, method=method,
            headers={"Content-Type": "application/json"})
        try:
            with self.opener.open(request, timeout=20) as answer:
                text = answer.read().decode()
                return answer.status, (json.loads(text) if text else None)
        except urllib.error.HTTPError as error:
            return error.code, json.loads(error.read().decode() or "null")

    def sign_in(self, username, password):
        status, payload = self.call("POST", "/api/session",
                                    {"username": username, "password": password})
        if status != 200:
            raise SystemExit(f"login failed for {username}: {status} {payload}")
        if payload.get("outcome") == "active":
            return payload["session"]
        party = payload["availableParties"][0]["id"]
        status, payload = self.call("POST", "/api/session/party", {"partyId": party})
        if status != 200:
            raise SystemExit(f"party selection failed: {status} {payload}")
        return payload


def find_requestable_role(client, held):
    _, roles = client.call("GET", "/api/roles")
    for role in roles["roles"]:
        if role["service"] or "*" in role["permissionCodes"] or role["id"] in held:
            continue
        return role
    raise SystemExit("no requestable role found")


def main():
    member = Client()
    access = member.sign_in("ashley.moore@acme_corporation", "Ashley-Pass-123")
    held = {role["roleId"] for role in member.call("GET", "/api/me/access")[1]["roles"]}
    role = find_requestable_role(member, held)
    print(f"member {access['username']} holds {len(held)} roles; asks for {role['name']}")

    status, asked = member.call("POST", "/api/me/requests",
                                {"roleIds": [role["id"]], "reason": "Covers the settlement desk"})
    print(f"ask: {status} {asked}")
    request_id = asked["requestId"]

    _, mine = member.call("GET", "/api/me/requests?offset=0&limit=20")
    row = next(item for item in mine["items"] if item["id"] == request_id)
    print(f"my requests: state={row['stateCode']} reason={row['reason']!r} "
          f"roles={[r['name'] for r in row['roles']]}")

    admin = Client()
    admin.sign_in("tenant_admin@acme_corporation", "Secure-Password-123")
    _, queue = admin.call("GET", "/api/requests?offset=0&limit=20")
    queued = next(item for item in queue["items"] if item["id"] == request_id)
    print(f"queue total={queue['total']} row: who={queued['requestedBy']} "
          f"roles={[r['name'] for r in queued['roles']]}")

    status, _ = admin.call("POST", f"/api/requests/{request_id}/decision",
                           {"version": queued["version"], "decisionCode": "approve",
                            "comment": ""})
    print(f"decide: {status}")

    _, mine = member.call("GET", "/api/me/requests?offset=0&limit=20")
    row = next(item for item in mine["items"] if item["id"] == request_id)
    print(f"my requests after: state={row['stateCode']} decision={row['decision']}")

    _, notifications = member.call("GET", "/api/me/notifications?unreadOnly=false&offset=0&limit=20")
    for note in notifications["items"]:
        values = {argument["name"]: argument["value"] for argument in note["arguments"]}
        print(f"notification: kind={note['kindCode']} key={note['messageKey']} "
              f"unread={note['readAt'] == ''} link={note['linkRoute']} values={values}")
    print(f"unread count: {member.call('GET', '/api/me/notifications/unread-count')[1]}")

    _, roles = member.call("GET", "/api/me/access")
    granted = role["name"] in {r["name"] for r in roles["roles"]}
    print(f"role granted: {granted}")


if __name__ == "__main__":
    main()
