#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil; python-indent-offset: 4 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""How often a small model reaches the operation that answers a request, and in
how many steps, browsing ORE's catalogue or searching it through MCP.

Each request runs twice against the running ores.http: once with the browsing
tools (list_areas, list_entities, list_operations) and once with
find_operations, each beside call_operation. A request is reached when the
model calls call_operation with the expected operation; its steps are the tool
calls it made until then. The model is served by llama.cpp with an
OpenAI-compatible API.

Usage: run.py <repository root> --model-url http://127.0.0.1:PORT/v1
              [--model NAME] [--steps 8] [--out results.json]

It signs in as the .env's shell login, creates a read-only agent grant for the
run, and revokes its key at the end. It prints no key or token.
"""
import argparse
import json
import pathlib
import sys
import time
import urllib.error
import urllib.request

BROWSE = ["list_areas", "list_entities", "list_operations", "call_operation"]
SEARCH = ["find_operations", "call_operation"]
SYSTEM = ("You act for a person in ORE Studio, a system for financial trading and risk. "
          "Use the tools to find the one operation that answers the person's request, "
          "then call it with call_operation. Call one tool at a time.")


def read_env(root):
    env = {}
    for line in (root / ".env").read_text().splitlines():
        if "=" in line and not line.lstrip().startswith("#"):
            key, value = line.split("=", 1)
            env[key.strip().removeprefix("export ").strip()] = value.strip().strip('"').strip("'")
    return env


class ores:
    def __init__(self, base):
        self.base = base
        self.cookie = ""

    def call(self, method, path, body=None, bearer=None, headers=None):
        data = None if body is None else json.dumps(body).encode()
        req = urllib.request.Request(self.base + path, data=data, method=method)
        req.add_header("Accept", "application/json, text/event-stream")
        if data is not None:
            req.add_header("Content-Type", "application/json")
        if bearer:
            req.add_header("Authorization", "Bearer " + bearer)
        elif self.cookie:
            req.add_header("Cookie", self.cookie)
            req.add_header("x-ores-request", "1")
            req.add_header("Origin", self.base)
        for key, value in (headers or {}).items():
            req.add_header(key, value)
        try:
            with urllib.request.urlopen(req, timeout=120) as answer:
                for header in answer.headers.get_all("Set-Cookie") or []:
                    self.cookie = header.split(";", 1)[0]
                text = answer.read().decode()
                return answer.status, answer.headers, (json.loads(text) if text else None)
        except urllib.error.HTTPError as error:
            return error.code, error.headers, error.read().decode()


class mcp:
    def __init__(self, server, key):
        self.server, self.key, self.next_id = server, key, 1
        _, headers, _ = self.send("initialize", {"protocolVersion": "2025-11-25", "capabilities": {},
                                                 "clientInfo": {"name": "catalogue-reach", "version": "1"}})
        self.session = headers.get("Mcp-Session-Id")
        self.server.call("POST", "/mcp", {"jsonrpc": "2.0", "method": "notifications/initialized"},
                         bearer=self.key, headers={"Mcp-Session-Id": self.session})

    def send(self, method, params):
        self.next_id += 1
        headers = {"Mcp-Session-Id": self.session} if getattr(self, "session", None) else {}
        return self.server.call("POST", "/mcp", {"jsonrpc": "2.0", "id": self.next_id, "method": method,
                                                 "params": params}, bearer=self.key, headers=headers)

    def tools(self):
        return self.send("tools/list", {})[2]["result"]["tools"]

    def call_tool(self, name, arguments):
        _, _, body = self.send("tools/call", {"name": name, "arguments": arguments})
        result = body.get("result", {}) if isinstance(body, dict) else {}
        content = result.get("content") or [{"text": str(body)}]
        return content[0].get("text", "")


def chat(url, model, messages, tools):
    body = {"model": model, "messages": messages, "tools": tools, "temperature": 0}
    req = urllib.request.Request(url.rstrip("/") + "/chat/completions", data=json.dumps(body).encode(),
                                 method="POST", headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=600) as answer:
        return json.load(answer)["choices"][0]["message"]


def run_one(session, url, model, request, offered, steps):
    tools = [{"type": "function", "function": {"name": t["name"], "description": t["description"],
                                               "parameters": t["inputSchema"]}} for t in offered]
    messages = [{"role": "system", "content": SYSTEM}, {"role": "user", "content": request}]
    trail = []
    for step in range(1, steps + 1):
        reply = chat(url, model, messages, tools)
        calls = reply.get("tool_calls") or []
        if not calls:
            return {"reached": None, "steps": step - 1, "trail": trail, "ended": "answered without a call"}
        messages.append({"role": "assistant", "content": reply.get("content") or "", "tool_calls": calls[:1]})
        call = calls[0]
        name = call["function"]["name"]
        try:
            arguments = json.loads(call["function"].get("arguments") or "{}")
        except ValueError:
            arguments = {}
        trail.append({"tool": name, "arguments": arguments})
        if name == "call_operation":
            return {"reached": arguments.get("name"), "steps": step, "trail": trail, "ended": "called"}
        text = session.call_tool(name, arguments) if name in [t["name"] for t in offered] else \
            f"No tool is named {name}."
        messages.append({"role": "tool", "tool_call_id": call.get("id", str(step)), "content": text[:4000]})
    return {"reached": None, "steps": steps, "trail": trail, "ended": "ran out of steps"}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("root", type=pathlib.Path)
    parser.add_argument("--model-url", required=True)
    parser.add_argument("--model", default="lfm3-1.7b")
    parser.add_argument("--steps", type=int, default=8)
    parser.add_argument("--out", type=pathlib.Path)
    options = parser.parse_args()

    env = read_env(options.root)
    server = ores(f"http://localhost:{env['ORES_HTTP_PORT']}")
    status, _, login = server.call("POST", "/api/session", {"username": env["ORES_SHELL_LOGIN_USERNAME"],
                                                            "password": env["ORES_SHELL_LOGIN_PASSWORD"]})
    server.call("POST", "/api/session/party", {"partyId": (login or {}).get("defaultPartyId", "")})
    _, _, made = server.call("POST", "/api/v1/iam/agent_grants/put",
                             {"agent_name": "catalogue-reach-" + str(int(time.time())), "role": "AgentReader",
                              "valid_days": 1})
    key = made.get("key") if isinstance(made, dict) else None
    if not key:
        print("could not create a read-only agent grant", file=sys.stderr)
        return 1
    try:
        session = mcp(server, key)
        offered = {t["name"]: t for t in session.tools()}
        cases = json.loads((pathlib.Path(__file__).parent / "requests.json").read_text())["requests"]
        results = []
        for mode, names in (("browse", BROWSE), ("search", SEARCH)):
            for case in cases:
                started = time.monotonic()
                outcome = run_one(session, options.model_url, options.model, case["request"],
                                  [offered[n] for n in names], options.steps)
                outcome.update({"mode": mode, "request": case["request"], "expected": case["expected"],
                                "seconds": round(time.monotonic() - started, 1)})
                outcome["hit"] = outcome["reached"] == case["expected"]
                results.append(outcome)
                print(f"{mode:6} {'HIT ' if outcome['hit'] else 'MISS'} {outcome['steps']} steps "
                      f"{case['expected']} <- {outcome['reached']} ({outcome['ended']})", flush=True)
        summary = {}
        for mode in ("browse", "search"):
            rows = [r for r in results if r["mode"] == mode]
            hits = [r for r in rows if r["hit"]]
            summary[mode] = {"reached": len(hits), "of": len(rows),
                             "mean_steps_when_reached": round(sum(r["steps"] for r in hits) / len(hits), 2)
                             if hits else None}
        print(json.dumps(summary, indent=2))
        if options.out:
            options.out.write_text(json.dumps({"model": options.model, "summary": summary, "results": results},
                                              indent=2) + "\n")
    finally:
        key_id = key.split("_")[3]
        server.call("POST", "/api/v1/iam/ops/revoke_agent_key",
                    {"key_id": f"{key_id[:8]}-{key_id[8:12]}-{key_id[12:16]}-{key_id[16:20]}-{key_id[20:]}",
                     "reason": "catalogue-reach run"})
    return 0


if __name__ == "__main__":
    sys.exit(main())
