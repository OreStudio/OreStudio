#!/usr/bin/env python3
"""compass notify — tell a person about something the app shows them.

    compass notify scenario <doc> --to <username@tenant> [--from <username@tenant>]
                                [--reason <text>]

A test scenario is written so that somebody runs it. This raises the inbox
notification that tells that person, and prints the id and the link so a caller
can act on either.

The command reads the scenario doc by path or by :ID:, and builds the one link
the Development area answers: /development/tests/<:ID:>. The bell uses that
route as it stands, because a scenario notification names the whole screen
rather than a row on it, so it carries an empty link id.

Raising goes through the inbox operation the rest of the platform uses
(inbox.v1.ops.raise_notification), driven through ores.shell rather than
straight to the database: the operation resolves the recipient, writes the
notification, its message arguments, one recipient row per person and each
person's in-application delivery, in one transaction, and it refuses a caller
without inbox::notifications:write. A direct insert would skip all of that.

The raiser is the signed-in account, so --from picks whose session raises it.
It defaults to the tenant administrator the environment already carries
(ORES_SHELL_LOGIN_USERNAME and ORES_SHELL_LOGIN_PASSWORD in .env), which holds
the write permission by its wildcard grant.
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

UUID_TOKEN = re.compile(r"^[0-9A-Fa-f][0-9A-Fa-f-]{5,}$")
PERSONA = re.compile(r"=\s*([\w.+-]+@[\w.+-]+)\s*=")
FIRST_STEP = re.compile(r"^\*\*[ \t]+(.*?)[ \t]*$")
HEADING = re.compile(r"^\*+[ \t]+.*$")


# --- the scenario doc -------------------------------------------------------


def resolve_scenario(project_root, token):
    """The doc a caller named, by path or by :ID:.

    A path is taken as written and then as repo-relative, so either the
    checkout or a directory inside it works. An id is looked up in the doc
    graph, which is what `compass show` reads; a unique prefix is enough.
    """
    import doc_index

    candidate = Path(token).expanduser()
    if not candidate.is_absolute():
        candidate = (Path.cwd() / candidate)
    if candidate.is_file():
        return _read_doc(candidate)

    repo_relative = project_root / token
    if repo_relative.is_file():
        return _read_doc(repo_relative)

    if UUID_TOKEN.match(token):
        docs = doc_index.load_all()
        doc = doc_index.resolve_id(token, docs)
        if doc is None:
            ambiguous = doc_index.find_ambiguous(token, docs)
            if ambiguous:
                names = ", ".join(d.rel_path for d in ambiguous[:5])
                raise SystemExit(f"error: {token} matches more than one doc: {names}")
            raise SystemExit(f"error: no doc has the id {token}.")
        return doc

    raise SystemExit(
        f"error: no file at {candidate}, and {token!r} is not a document id."
    )


def _read_doc(path):
    """The doc at a path, read the way the rest of compass reads one."""
    import doc_index

    try:
        doc = doc_index.parse_doc(path)
    except ValueError:
        raise SystemExit(
            f"error: {path} is outside the checkout. A scenario doc lives in "
            f"{doc_index.REPO_ROOT}."
        )
    if doc is None:
        raise SystemExit(f"error: {path} has no :ID: in its properties drawer.")
    return doc


def persona_of(text):
    """The account a scenario's first step logs in as, when it names one.

    The scaffold states its first step as "Log in as <persona>", so a scenario
    that was just written names none: the caller gets the placeholder until
    somebody fills the step in. A filled-in step states the account between two
    '=' marks.

    A '#' line is the template's advice to the author and not part of the step,
    and that advice names an example account, so the scan skips it: reading an
    example as the persona would aim the notification at somebody the scenario
    never mentions.
    """
    lines = text.splitlines()
    in_first_step = False
    body = []
    for line in lines:
        match = FIRST_STEP.match(line)
        if match and not in_first_step:
            in_first_step = True
            body.append(match.group(1))
            continue
        if in_first_step:
            if HEADING.match(line) and not FIRST_STEP.match(line):
                break
            if line.lstrip().startswith("#"):
                continue
            body.append(line)
    found = PERSONA.search("\n".join(body))
    return found.group(1) if found else ""


def notify_suggestion(doc_path, project_root):
    """The ready-to-run notify command for a scenario that was just written."""
    try:
        text = Path(doc_path).read_text(encoding="utf-8")
    except OSError:
        return ""
    try:
        shown = str(Path(doc_path).relative_to(project_root))
    except ValueError:
        shown = str(doc_path)
    recipient = persona_of(text) or "<username@tenant>"
    return f"compass notify scenario {shown} --to {recipient}"


# --- the recipient ----------------------------------------------------------


def sql_literal(value):
    """One SQL string literal, single quotes doubled."""
    return "'" + value.replace("'", "''") + "'"


def account_id(env, who):
    """The account a `username@tenant` names, read from the database.

    The address is the form the app signs in with, so the caller never has to
    know a UUID. The read is the tenant-qualified one: a username is unique
    inside its tenant, not across them.
    """
    import compass_db

    username, _, tenant = who.partition("@")
    if not username or not tenant:
        raise SystemExit(
            f"error: {who!r} is not an account address. State it as "
            f"username@tenant, for example tenant_admin@acme_corporation."
        )
    sql = (
        "select distinct a.id::text from ores_iam_accounts_tbl a "
        "join ores_iam_tenants_tbl t on t.id = a.tenant_id "
        "and t.valid_to = ores_utility_infinity_timestamp_fn() "
        f"where a.username = {sql_literal(username)} "
        f"and t.code = {sql_literal(tenant)} "
        "and a.valid_to = ores_utility_infinity_timestamp_fn();"
    )
    result = compass_db._psql(
        env,
        "-At",
        "-c",
        sql,
        user=env.get("ORES_DB_DDL_USER", ""),
        password=env.get("ORES_DB_DDL_PASSWORD", ""),
        database=env.get("ORES_DATABASE_NAME", ""),
        check=False,
        capture=True,
    )
    if result.returncode != 0:
        raise SystemExit(f"error: the account read failed: {(result.stderr or '').strip()}")
    found = [line.strip() for line in (result.stdout or "").splitlines() if line.strip()]
    if not found:
        raise SystemExit(f"error: no account {who!r}. Check `compass db status` and the name.")
    if len(found) > 1:
        raise SystemExit(f"error: {who!r} names {len(found)} accounts: {', '.join(found)}")
    return found[0]


# --- raising ----------------------------------------------------------------


def shell_binary(project_root, env):
    """The ores.shell this checkout published, resolved the way compass shell does."""
    preset = env.get("ORES_PRESET", "")
    if not preset:
        raise SystemExit("error: ORES_PRESET is not set in .env. Run compass env configure.")
    name = "ores.shell.exe" if sys.platform == "win32" else "ores.shell"
    binary = project_root / "build" / "output" / preset / "publish" / "bin" / name
    if not binary.is_file():
        raise SystemExit(f"error: ores.shell not found: {binary}\n   Build it: compass build ores.shell")
    return binary


def raise_script(kind_code, link_route, link_id, recipients, arguments):
    """The shell command that raises the notification, as one line of a script.

    A value with a space in it is quoted, because the shell splits a line on
    whitespace and keeps a quoted run together. A '=' inside a value is safe:
    only the first one separates an argument's name from its value.
    """
    def quote(value):
        return f'"{value}"' if value == "" or " " in value or "\t" in value else value

    ids = ",".join(recipients) if recipients else "__none__"
    parts = [
        "notification_operations",
        "raise-notification",
        quote(kind_code),
        quote(link_route),
        quote(ids),
    ]
    # An empty link id is an absent flag: the tokenizer drops an empty
    # argument, so the command line cannot state one.
    if link_id:
        parts += ["--link_id", quote(link_id)]
    for name, value in arguments.items():
        parts += ["--argument", quote(f"{name}={value}")]
    return parts


def run_raise(project_root, env, sender, script_lines):
    """Run the raise in a signed-in shell session and read its answer.

    The shell prints the operation's answer as one JSON line, so the line with
    the notification id in it is the answer and everything else is the session
    talking to itself.
    """
    command = " ".join(script_lines)
    with tempfile.NamedTemporaryFile(
        "w", suffix=".ores", prefix="compass_notify_", delete=False, encoding="utf-8"
    ) as handle:
        handle.write("# Raised by compass notify.\n")
        handle.write(command + "\n")
        script_path = Path(handle.name)

    try:
        binary = shell_binary(project_root, env)
        cmd = [str(binary)]
        for key, flag in (
            ("ORES_SHELL_NATS_TLS_CA", "--nats-tls-ca"),
            ("ORES_SHELL_NATS_TLS_CERT", "--nats-tls-cert"),
            ("ORES_SHELL_NATS_TLS_KEY", "--nats-tls-key"),
        ):
            if env.get(key):
                cmd += [flag, env[key]]
        cmd += [
            "--login-username",
            sender["username"],
            "--login-password",
            sender["password"],
            "--load",
            str(script_path),
        ]
        child_env = {**os.environ, **env}
        return subprocess.run(
            cmd, cwd=str(project_root), env=child_env, capture_output=True, text=True
        )
    finally:
        script_path.unlink(missing_ok=True)


def answer_of(completed):
    """The operation's answer, read from the last JSON line the shell printed."""
    for line in reversed((completed.stdout or "").splitlines()):
        line = line.strip()
        if not line.startswith("{") or "notification_id" not in line:
            continue
        try:
            return json.loads(line)
        except json.JSONDecodeError:
            continue
    return None


# --- the command ------------------------------------------------------------


def run(argv, project_root, env_file=None):
    ap = argparse.ArgumentParser(
        prog="compass notify",
        description="Tell a person about something the app shows them.",
        epilog="The notification names the whole screen it links to, so the link "
        "carries no row id.",
    )
    sub = ap.add_subparsers(dest="target", required=True)

    scenario = sub.add_parser(
        "scenario",
        help="Tell a person that a test scenario is ready to run.",
    )
    scenario.add_argument("doc", help="The scenario doc: a path, or its :ID: (a prefix is enough).")
    scenario.add_argument("--to", required=True, metavar="USERNAME@TENANT",
                          help="Who is told, e.g. henry.jones@acme_corporation.")
    scenario.add_argument("--from", dest="sender", default="", metavar="USERNAME@TENANT",
                          help="Who raises it (default: ORES_SHELL_LOGIN_USERNAME from .env).")
    scenario.add_argument("--reason", default="",
                          help="Why it is raised, in words (default: the scenario's description).")
    scenario.add_argument("--dry-run", action="store_true",
                          help="Print what would be raised, and raise nothing.")

    args = ap.parse_args(argv)

    import compass_db

    env = compass_db.load_env(project_root, env_file)

    doc = resolve_scenario(project_root, args.doc)
    if doc.doctype != "test_scenario":
        raise SystemExit(
            f"error: {doc.rel_path} is a {doc.doctype or 'doc'}, not a test_scenario. "
            "This command notifies people about a scenario; it does not read anything else."
        )

    default_sender = env.get("ORES_SHELL_LOGIN_USERNAME", "")
    sender_address = args.sender or default_sender
    if not sender_address:
        raise SystemExit(
            "error: no --from given, and ORES_SHELL_LOGIN_USERNAME is not set in .env."
        )
    password = env.get("ORES_SHELL_LOGIN_PASSWORD", "")
    if not password and sender_address == default_sender:
        password = os.environ.get("ORES_SHELL_LOGIN_PASSWORD", "")
    if not password:
        raise SystemExit(
            "error: no password for the raising account. ORES_SHELL_LOGIN_PASSWORD is "
            "not set in .env (or in the environment)."
        )

    recipient_id = account_id(env, args.to)
    link_route = f"/development/tests/{doc.id.upper()}"
    title = doc.title
    for prefix in ("Test scenario:", "test scenario:"):
        if title.startswith(prefix):
            title = title[len(prefix):].strip()
            break
    reason = args.reason or doc.description or "no reason given"
    # The message names the raiser the way a person reads it, so the tenant is
    # not repeated in it: the notification already belongs to that tenant.
    raiser_name = sender_address.partition("@")[0]

    script = raise_script(
        kind_code="scenario.to_run",
        link_route=link_route,
        link_id="",
        recipients=[recipient_id],
        arguments={"scenario": title, "reason": reason, "raiser": raiser_name},
    )

    web_port = env.get("ORES_WEB_PORT", "")
    app_url = f"http://localhost:{web_port}{link_route}" if web_port else link_route

    if args.dry_run:
        print(f"Would raise scenario.to_run as {sender_address} to {args.to} ({recipient_id})")
        print(f"  link: {link_route}")
        print(f"  scenario: {title}")
        print(f"  reason: {reason}")
        print("  shell: " + " ".join(script))
        return 0

    # The principal is the whole address: the login reads the tenant out of it.
    sender = {"username": sender_address, "password": password}
    completed = run_raise(project_root, env, sender, script)
    answer = answer_of(completed)

    if answer is None or completed.returncode != 0:
        detail = (completed.stdout or "").strip() or (completed.stderr or "").strip()
        print(f"❌ The raise did not finish. ores.shell exited {completed.returncode}.", file=sys.stderr)
        if detail:
            print(detail, file=sys.stderr)
        return 1

    result = answer.get("result") or {}
    if result.get("outcome") not in (None, "ok"):
        print(f"❌ {result.get('code', 'refused')}: {result.get('message', '')}", file=sys.stderr)
        return 1

    notification_id = answer.get("notification_id", "")
    recipient_count = answer.get("recipient_count", 0)
    if not notification_id:
        print(f"❌ The raise answered without a notification id: {json.dumps(answer)}", file=sys.stderr)
        return 1

    print(f"✅ Notified {args.to} ({recipient_id}) as {sender_address}")
    print(f"   kind:         scenario.to_run")
    print(f"   notification: {notification_id}")
    print(f"   recipients:   {recipient_count}")
    print(f"   link:         {link_route}")
    print(f"   app:          {app_url}")
    return 0
