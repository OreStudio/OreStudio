#!/usr/bin/env python3
"""Run a component's generated HTTP recipes against the live gateway.

The shell surface answers V04 by replaying every generated command against the
live fleet.  This is the same check for the HTTP surface: every generated
recipe is a request the gateway must answer, so every recipe is replayed and
given a verdict.

The recipes are the tangled Hurl files under
projects/ores.http/scripts/library/<group>, one file per endpoint.  Each is
replayed on its own, behind a sign-in preamble this script performs once, so
one failing recipe cannot hide the verdict of the next.

A recipe names no host, no port and no credential: it reads =base_url= and
=token= as Hurl variables, and this script supplies both.

Usage:
    1. Bootstrap a local administrator once, in bootstrap mode.  The gateway
       exposes it::

        curl -sX POST http://localhost:20800/api/v1/iam/bootstrap/create-admin \\
            -H 'Content-Type: application/json' \\
            -d '{"principal":"admin","email":"admin@example.com","password":"..."}'

    2. Export the same credential and run this script, naming the recipe
       groups to replay::

        export ORES_HTTP_PRINCIPAL=admin ORES_HTTP_PASSWORD=<password>
        projects/ores.codegen/scripts/check_http_recipes.py tags images

A destructive recipe is named in --skip and recorded NOT_REPLAYED rather than
run.

Writes a TSV and a per-recipe log under --out-dir, which defaults to
.audit/http-recipes/. The .audit tree is not committed, so rerun this script
to regenerate the evidence.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import urllib.error
import urllib.request
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]
LIBRARY = REPO / "projects/ores.http/scripts/library"
PREFIX = "projects/ores.http/scripts/library"
DEFAULT_OUT_DIR = REPO / ".audit/http-recipes"
DEFAULT_BASE_URL = "http://localhost:20800"
DEFAULT_GROUPS = ("tags",)

# The login route, and the field the request names the caller by.  The field is
# `principal` rather than `username`, which is what the hand-written recipes
# beside these still say.
LOGIN_PATH = "/api/v1/iam/auth/login"
LOGIN_PRINCIPAL_FIELD = "principal"

# A verdict per recipe.  REJECTED is separated from FAILED because the gateway
# refusing the caller says something different about the deployment than the
# endpoint answering wrongly: the first is a wiring or credential defect, the
# second is an endpoint defect.
PASSED = "PASSED"
REJECTED = "REJECTED"
FAILED = "FAILED"
ERROR = "ERROR"
NOT_REPLAYED = "NOT_REPLAYED"


def hurl_binary() -> str | None:
    """The Hurl to run, from PATH or from a build-tree download.

    CI installs it as a package; a developer machine may only have the static
    release unpacked under build/output, which is gitignored.
    """
    found = shutil.which("hurl")
    if found:
        return found
    candidates = sorted((REPO / "build/output/tools").glob("hurl-*/bin/hurl"))
    return str(candidates[-1]) if candidates else None


def recipe_paths(groups: tuple[str, ...], skip: set[str]) -> list[Path]:
    found: list[Path] = []
    for group in groups:
        directory = LIBRARY / group
        if not directory.is_dir():
            raise SystemExit(f"no recipe group at {directory}")
        found.extend(sorted(p for p in directory.glob("*.hurl") if p.stem not in skip))
    return found


def sign_in(base_url: str, principal: str, password: str) -> str:
    """The token every recipe is replayed with, or a refusal naming the cause.

    The body is sent as JSON rather than form-encoded because the route decodes
    a request object, and the answer is read rather than trusted: a 200 that
    carries success = false is a refused sign-in, not a session.
    """
    payload = json.dumps(
        {LOGIN_PRINCIPAL_FIELD: principal, "password": password}).encode()
    request = urllib.request.Request(
        base_url + LOGIN_PATH,
        data=payload,
        headers={"Content-Type": "application/json",
                 "Accept": "application/json"},
        method="POST")
    try:
        with urllib.request.urlopen(request, timeout=30) as response:
            body = json.loads(response.read().decode())
    except urllib.error.HTTPError as error:
        raise SystemExit(
            f"sign-in refused with HTTP {error.code}: "
            f"{error.read().decode()[:200]}")
    except urllib.error.URLError as error:
        raise SystemExit(f"could not reach the gateway at {base_url}: {error}")
    if not body.get("success"):
        raise SystemExit(
            f"sign-in refused: {body.get('error_message') or body.get('message')}")
    token = body.get("token") or ""
    if not token:
        raise SystemExit("sign-in answered success with no token")
    return token


def classify(completed: subprocess.CompletedProcess, recipe: Path) -> tuple[str, str]:
    """Return (status, reason) for one recipe run.

    Hurl reports its own verdict on stdout.  A run that failed while the
    transcript carries no assertion failure did not get that far, so it is an
    error rather than a failing endpoint.
    """
    transcript = (completed.stdout or "") + (completed.stderr or "")
    if completed.returncode == 0:
        return PASSED, "ok"
    if "actual value is <401>" in transcript or "HTTP 401" in transcript:
        return REJECTED, "the gateway refused the caller"
    if "Assert status code" in transcript or "Assert " in transcript:
        for line in transcript.splitlines():
            if line.strip().startswith("error:"):
                return FAILED, line.strip()[:200]
        return FAILED, "an assertion failed"
    for line in transcript.splitlines():
        if line.strip().startswith("error:"):
            return ERROR, line.strip()[:200]
    return ERROR, f"hurl exited {completed.returncode} without an assertion failure"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "groups",
        nargs="*",
        default=list(DEFAULT_GROUPS),
        help=f"recipe library directories to replay (default: {' '.join(DEFAULT_GROUPS)})",
    )
    parser.add_argument(
        "--base-url",
        default=os.environ.get("ORES_HTTP_BASE_URL", DEFAULT_BASE_URL),
        help=f"the gateway to replay against (default: {DEFAULT_BASE_URL})",
    )
    parser.add_argument(
        "--out-dir",
        default=str(DEFAULT_OUT_DIR),
        help="where the TSV and the per-recipe logs go "
             f"(default: {DEFAULT_OUT_DIR.relative_to(REPO)})",
    )
    parser.add_argument(
        "--skip",
        default="",
        help="comma-separated recipe stems to record as NOT_REPLAYED, such as a "
             "destructive request the fleet must not be asked to run",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=60,
        help="seconds to allow one recipe (default: 60)",
    )
    args = parser.parse_args()

    out_dir = Path(args.out_dir)
    if not out_dir.is_absolute():
        out_dir = REPO / out_dir
    out_dir = out_dir.resolve()
    log_dir = out_dir / "logs"
    tsv = out_dir / "recipes.tsv"
    skip = {s.strip() for s in args.skip.split(",") if s.strip()}

    principal = os.environ.get("ORES_HTTP_PRINCIPAL")
    password = os.environ.get("ORES_HTTP_PASSWORD")
    if not principal or not password:
        print("ORES_HTTP_PRINCIPAL and ORES_HTTP_PASSWORD must be set",
              file=sys.stderr)
        return 2
    hurl = hurl_binary()
    if not hurl:
        print("hurl not found: install it, or unpack a release under "
              "build/output/tools/", file=sys.stderr)
        return 2

    log_dir.mkdir(parents=True, exist_ok=True)
    recipes = recipe_paths(tuple(args.groups), skip)
    token = sign_in(args.base_url, principal, password)
    rows: list[tuple[str, str, str, str]] = []

    for recipe in recipes:
        completed = subprocess.run(
            [hurl, "--test", "--no-output",
             "--variable", f"base_url={args.base_url}",
             "--variable", f"token={token}",
             str(recipe)],
            capture_output=True,
            text=True,
            timeout=args.timeout,
            cwd=str(REPO),
        )
        status, reason = classify(completed, recipe)
        rel = f"{PREFIX}/{recipe.parent.name}/{recipe.name}"
        (log_dir / f"{recipe.name}.log").write_text(
            (completed.stdout or "") + (completed.stderr or ""))
        rows.append((rel, recipe.stem, status, reason))
        print(f"{status:14} {recipe.name:32} {reason[:90]}")

    for stem in sorted(skip):
        rows.append((f"{PREFIX}/*/{stem}.hurl", stem, NOT_REPLAYED,
                     "not replayed: named in --skip"))
        print(f"{NOT_REPLAYED:14} {stem:32} not replayed: named in --skip")

    with tsv.open("w") as handle:
        handle.write("recipe\tname\tstatus\treason\n")
        for rel, name, status, reason in rows:
            handle.write(f"{rel}\t{name}\t{status}\t{reason}\n")

    tally: dict[str, int] = {}
    for _, _, status, _ in rows:
        tally[status] = tally.get(status, 0) + 1
    print(f"\n{len(rows)} recipes: "
          + ", ".join(f"{k}={v}" for k, v in sorted(tally.items())))
    print(f"evidence: {tsv.relative_to(REPO)}")
    failing = (REJECTED, FAILED, ERROR)
    return 1 if any(tally.get(k) for k in failing) else 0


if __name__ == "__main__":
    raise SystemExit(main())
