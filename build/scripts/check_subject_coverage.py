#!/usr/bin/env python3
"""Every subject the protocol models declare has a responder on the running bus.

The contract this proves, and why it needs a live server
--------------------------------------------------------
A model names the subject an operation is published on. Whether anything
subscribes it is not visible in the tree: half the subscriptions are generated
per entity family, the step-command and history families declare theirs through
tables rather than string literals, and the registrar that subscribes the rest
cannot be run outside a service process because it creates a JetStream stream
and claims it once per process. A source-level scan can therefore pass on a
tree where a declared subject has no subscriber, which is worse than no check.

So this check asks the bus. For every subject a model declares it publishes a
request with no credentials, and reads the answer:

  * any answer at all, including the refusal an unauthenticated request earns,
    means something is subscribed: this check asks whether a responder exists,
    not whether the caller is allowed through;
  * `No responders are available` means nobody subscribes the subject, and the
    check fails naming the subject, the message and the model file.

A subject whose message declares no `:response:` is published rather than
requested: the bus carries it to whoever is listening, and demanding a
responder would fail on a healthy tree. Those are counted and skipped, which
is also what happens to `{component}.v1.history.get`, the one templated
subject: the generator names it once per entity family, so a model cannot say
what the concrete subject is.

What it does with a subject that is known to be unserved
--------------------------------------------------------
`build/scripts/subject_coverage_exceptions.json` records the subjects the tree
declares and nothing serves today, each with the reason it stays and the task
that owns it. An exception that names a subject which now answers is itself a
failure, so the file cannot outlive the defect it records.

What it does when there is no server
------------------------------------
It fails, with the command to run. It does not skip, for the reason
check_step_consumers.py gives: a check that answers "nothing to do" when it
cannot reach the thing it checks silently stops checking, and it does so in
exactly the run that was meant to prove the contract.

Why it is not in the ordinary phase
-----------------------------------
The subjects answer only while the fleet is up. The heavy phase is where the
fleet is expected, and `--exclude-heavy` keeps this out of the ordinary run.

Usage:
  python3 build/scripts/check_subject_coverage.py [--root PATH] [--timeout 2]
"""
from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

# One `** name` block of a modeling document, and the subject it states.
BLOCK = re.compile(r"^\*\* (\w+)\n(.*?)(?=^\*\* |\Z)", re.M | re.S)
SUBJECT_LINE = re.compile(r"^:subject: (\S+)", re.M)
RESPONSE_LINE = re.compile(r"^:response: \S+", re.M)

# What the CLI prints when it gets no reply, and when it gets one.
NO_RESPONDER = "No responders are available"
ANSWERED = "Received with rtt"
TIMED_OUT = "timeout"

EXCEPTIONS = Path("build/scripts/subject_coverage_exceptions.json")

SERVED = "served"
ABSENT = "absent"
SILENT = "silent"


def load_env(root: Path) -> dict[str, str]:
    """The environment's values, from .env, without running compass."""
    values: dict[str, str] = {}
    env_file = root / ".env"
    if not env_file.is_file():
        return values
    for line in env_file.read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, value = line.partition("=")
        values[key.strip()] = value.strip()
    return values


def nats_command(env: dict[str, str], root: Path) -> list[str] | None:
    """The nats CLI, pointed at this environment, or None when unusable."""
    port = env.get("ORES_NATS_PORT", "4222")
    ca = env.get("ORES_NATS_TLS_CA", "")
    cert = env.get("ORES_NATS_TLS_CERT", "")
    key = env.get("ORES_NATS_TLS_KEY", "")
    if not (ca and cert and key):
        return None
    if not all(Path(path).is_file() for path in (ca, cert, key)):
        return None
    return ["nats", "--server", f"tls://localhost:{port}",
            "--tlsca", ca, "--tlscert", cert, "--tlskey", key]


def subject_prefix(env: dict[str, str]) -> str | None:
    """The prefix every subject carries in this environment."""
    return env.get("ORES_NATS_SUBJECT_PREFIX") or None


def load_exceptions(root: Path) -> dict[str, dict]:
    """The subjects recorded as unserved, each with why and who owns the fix."""
    path = root / EXCEPTIONS
    if not path.is_file():
        return {}
    data = json.loads(path.read_text())
    return {entry["subject"]: entry for entry in data.get("exceptions", [])}


def declared_subjects(root: Path) -> tuple[list[tuple[str, str, str]], list[str], int]:
    """Every subject the models declare as a request, and what was left out.

    Each entry is (subject, model name, file), so a finding can name the
    message and the document a reader has to open. The second list names the
    templated subjects, and the count is how many were published rather than
    requested.
    """
    subjects: list[tuple[str, str, str]] = []
    templated: list[str] = []
    published = 0
    for path in sorted((root / "projects").glob("*/modeling/*.org")):
        text = path.read_text(errors="ignore")
        for block in BLOCK.finditer(text):
            found = SUBJECT_LINE.search(block.group(2))
            if not found:
                continue
            subject = found.group(1)
            relative = str(path.relative_to(root))
            if "{" in subject:
                templated.append(f"{subject} ({relative} names {block.group(1)})")
                continue
            if not RESPONSE_LINE.search(block.group(2)):
                published += 1
                continue
            subjects.append((subject, block.group(1), relative))
    return subjects, templated, published


def classify(stderr: str, returncode: int) -> str:
    """Whether the bus answered, named nobody, or said nothing at all."""
    if NO_RESPONDER in stderr:
        return ABSENT
    if ANSWERED in stderr:
        return SERVED
    if TIMED_OUT in stderr.lower() or returncode != 0:
        return SILENT
    return SERVED


def ask(nats: list[str], subject: str, timeout: int) -> str:
    """Publishes an empty request and reports how the bus answered.

    The answer is a wire payload, so it is read as bytes: only the CLI's own
    progress lines on stderr are text, and they are what says whether a
    responder exists.
    """
    try:
        done = subprocess.run(nats + ["request", subject, "", "--timeout", f"{timeout}s"],
                              capture_output=True, timeout=timeout + 10)
    except (OSError, subprocess.TimeoutExpired):
        return SILENT
    return classify(done.stderr.decode("utf-8", "replace"), done.returncode)


def check(root: Path, nats: list[str], prefix: str, timeout: int,
          ask_fn=ask) -> tuple[int, int, list[tuple[str, str, str, str]], list[str], int]:
    """Asks about every declared request. Returns the counts and the findings."""
    subjects, templated, published = declared_subjects(root)
    findings = []
    served = 0
    for subject, name, path in subjects:
        answer = ask_fn(nats, f"{prefix}.{subject}", timeout)
        if answer == SERVED:
            served += 1
            continue
        findings.append((subject, name, path, answer))
    return len(subjects), served, findings, templated, published


def triage(findings: list[tuple[str, str, str, str]],
           exceptions: dict[str, dict]) -> tuple[list[tuple[str, str, str, str]], list[str]]:
    """Splits the findings into the failures and the exceptions that now answer.

    An exception whose subject answers is a failure of its own: the file would
    otherwise keep excusing a defect that is gone.
    """
    unexcused = [finding for finding in findings if finding[0] not in exceptions]
    answered = sorted(subject for subject in exceptions
                      if subject not in {finding[0] for finding in findings})
    return unexcused, answered


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--root", default=str(REPO_ROOT),
                        help="the checkout whose models are read (default: this repo)")
    parser.add_argument("--timeout", type=int, default=2,
                        help="seconds to wait for one answer (default: 2)")
    args = parser.parse_args(argv)
    root = Path(args.root).resolve()

    env = load_env(root)
    nats = nats_command(env, root)
    prefix = subject_prefix(env)
    if nats is None or prefix is None:
        print("FAIL: no NATS client configuration. .env must name the subject"
              " prefix and the CA and client certificate, and the files must"
              " exist.", file=sys.stderr)
        print("  Run: ./compass.sh services start", file=sys.stderr)
        return 2

    total, served, findings, templated, published = check(root, nats, prefix, args.timeout)
    exceptions = load_exceptions(root)

    unexcused, answered = triage(findings, exceptions)
    for subject in sorted(exceptions):
        excused = exceptions[subject]
        where = next((f"declared by {name} in {path}"
                      for found, name, path, _ in findings if found == subject), "")
        if where:
            print(f"KNOWN {subject}: {where}; {excused['reason']}"
                  f" (task {excused['task']})")

    failures = 0
    for subject, name, path, answer in unexcused:
        where = f"declared by {name} in {path}"
        failures += 1
        if answer == ABSENT:
            print(f"FAIL {subject}: {where}, and nothing subscribes it.")
            print("     Serve it from the registrar of the component that owns"
                  " the model, or stop declaring it.")
        else:
            print(f"FAIL {subject}: {where}, and the bus neither answered nor"
                  f" named a missing responder before {args.timeout}s.")
            print("     A subscriber that cannot answer an empty request in time"
                  " still leaves a caller waiting; give it the same treatment.")

    for subject in answered:
        failures += 1
        print(f"FAIL {subject}: recorded as unserved, and it answers now."
              f" Remove it from {EXCEPTIONS}.")

    for entry in templated:
        print(f"SKIP {entry}: the generator names this subject per family.")

    print()
    print(f"{served} of {total} declared request(s) answered;"
          f" {len(findings)} did not; {published} published subject(s) and"
          f" {len(templated)} templated subject(s) skipped.")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
