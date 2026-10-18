#!/usr/bin/env python3
"""Every durably consumed subject is covered by exactly one stream, and no two
streams claim overlapping subjects.

Why this is a static check
--------------------------
The two defects it names cannot be seen on a running broker. The server refuses
a consumer whose filter its stream does not cover, and it refuses a second
stream whose subjects overlap an existing one. A broker that is up is therefore
already in the state this check proves, and a runtime audit of it can only pass.
The defect appears before the server is asked: in the declarations the services
send at start, or in the timeout a publisher sees when nothing holds its
subject. The check reads those declarations and applies the server's own two
rules to them.

What it reads
-------------
It reads every declaration site in the tree, in three shapes:

  * ``ensure_stream(name, filters)`` creates a stream. The name is the argument
    of ``make_stream_name``, and the filters are ``covering_subjects`` over the
    subjects named.
  * ``serve_step_commands(nats, service, routes)`` creates the service's step
    stream and the one durable consumer over it, both derived from the routes
    table's subjects, so the two cannot disagree.
  * ``js_queue_subscribe(subject, durable, group, handler, stream, bound)``
    reads one subject durably on a stream that something else must ensure.

A declaration it cannot resolve is a failure, not a skip. This is the rule that
keeps the check from going blind: a new way to name a subject or a stream fails
the check until it is taught, rather than passing unread. The transport's own
headers are read for the same reason: a fourth entry point that creates a
stream or a durable consumer fails the check until the check reads it. A
service that calls the cnats API directly is outside any source check's reach,
and the check does not claim otherwise.

What it proves
--------------
  * Every subject a durable consumer reads is matched by exactly one stream.
  * Every consumer that names its stream names one that something ensures, and
    that stream covers the subject.
  * No two streams (with different names) have filters that overlap. Two
    declarations of one stream name must carry the same filters, because the
    later ensure replaces the earlier one's subjects.
  * The scan found something. An empty catalogue is a failure, not a pass.

It does not need a broker, a database, or the fleet. It reads the tree only.

Usage:
  python3 build/scripts/check_stream_coverage.py
  python3 build/scripts/check_stream_coverage.py --print-catalogue
"""
from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

# The transport defines the calls this check reads, so its own source is not a
# declaration site. The step-command helper is the one place a service's stream
# and consumer are derived, and the check models that derivation rather than
# reading the helper's body as a call site.
EXCLUDED_PREFIXES = ("projects/ores.nats/",)
STEP_HELPER = "projects/ores.service/include/ores.service/messaging/step_command_stream.hpp"
SKIP_DIRS = {"venv", "node_modules", ".venv", "build", "target", ".git"}

CONST = re.compile(
    r"(?:inline\s+)?constexpr\s+(?:static\s+)?"
    r"(?:std::string_view|auto|char\s+const\s*\*|const\s+char\s*\*)\s+"
    r"(\w+)\s*=\s*\"([^\"]*)\""
)
MEMBER = re.compile(r"nats_subject\s*=\s*\"([^\"]*)\"")
TYPE_START = re.compile(r"(?:struct|class)\s+(\w+)[^;{]*\{")
ARRAY_START = re.compile(
    r"(?:static\s+)?constexpr\s+"
    r"(?:std::array\s*(?:<[^;{}]*>)?|std::string_view|std::string"
    r"|char\s+const\s*\*)"
    r"\s*(\w+)\s*(?:\[\s*\])?\s*(?:=\s*)?\{"
)
STEP_ROUTES = re.compile(r"step_routes\s+(\w+)\s*(?:\{\s*\"([^\"]*)\"\s*\})?\s*;")
FILE_LOCAL_STRING = re.compile(
    r"(?:static\s+)?(?:const|constexpr)\s+(?:auto|std::string_view)\s+(\w+)\s*=\s*\"([^\"]*)\""
)
STREAM_NAME_VAR = re.compile(
    r"(?:const\s+auto|const\s+std::string|std::string)\s+(\w+)\s*"
    r"(?:=\s*|\(\s*)nats\.make_stream_name\s*\(\s*\"([^\"]*)\"\s*\)"
)
IDENTIFIER = re.compile(r"^[A-Za-z_]\w*(?:::[A-Za-z_]\w*)*$")
COMMENT_OR_STRING = re.compile(r'"(\\.|[^"\\])*"|//[^\n]*|/\*.*?\*/', re.DOTALL)
BLANK = {code: (10 if code == 10 else 32) for code in range(256)}


class Unresolved(Exception):
    """A declaration this check cannot read, which is a failure."""


@dataclass
class Stream:
    suffix: str
    filters: tuple[str, ...]
    where: list[str] = field(default_factory=list)


@dataclass
class Consumer:
    durable: str
    subjects: tuple[str, ...]
    named_stream: str | None
    where: str


# ---------------------------------------------------------------------------
# C++ text reading
# ---------------------------------------------------------------------------


def project_sources(root: Path) -> list[Path]:
    files: list[Path] = []
    for path in sorted((root / "projects").glob("**/*")):
        if path.suffix not in (".cpp", ".hpp"):
            continue
        rel = path.relative_to(root).as_posix()
        parts = set(rel.split("/"))
        if parts & SKIP_DIRS or "/tests/" in rel or rel.startswith(EXCLUDED_PREFIXES):
            continue
        files.append(path)
    return files


def strip_comments(text: str) -> str:
    """Blank comments, keeping every offset and newline in place."""

    def blank(match: re.Match[str]) -> str:
        token = match.group(0)
        if token.startswith('"'):
            return token
        return token.translate(BLANK)

    return COMMENT_OR_STRING.sub(blank, text)


def split_arguments(text: str, open_at: int) -> list[str]:
    """The arguments of the call whose '(' is at @p open_at, comma-split."""
    args: list[str] = []
    depth = 0
    start = open_at + 1
    i = open_at
    n = len(text)
    while i < n:
        c = text[i]
        if c == '"':
            i += 1
            while i < n and text[i] != '"':
                i += 2 if text[i] == "\\" else 1
        elif c in "({[":
            depth += 1
        elif c in ")}]":
            depth -= 1
            if depth == 0:
                args.append(text[start:i])
                return args
        elif c == "," and depth == 1:
            args.append(text[start:i])
            start = i + 1
        i += 1
    raise Unresolved("a call does not close")


def string_literal(expr: str) -> str | None:
    m = re.fullmatch(r'\s*"([^"]*)"\s*', expr)
    return m.group(1) if m else None


def line_of(text: str, offset: int) -> int:
    return text.count("\n", 0, offset) + 1


def find_call_sites(text: str, name: str) -> list[int]:
    return [m.start() for m in re.finditer(r"\b" + re.escape(name) + r"\s*\(", text)]


def first_argument(text: str, open_at: int) -> str:
    return split_arguments(text, open_at)[0]


def brace_body(text: str, open_at: int) -> tuple[str, int]:
    """The text between the braces whose '{' is at @p open_at, and its end."""
    depth = 0
    i = open_at
    n = len(text)
    while i < n:
        c = text[i]
        if c == '"':
            i += 1
            while i < n and text[i] != '"':
                i += 2 if text[i] == "\\" else 1
        elif c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
            if depth == 0:
                return text[open_at + 1 : i], i
        i += 1
    raise Unresolved("a braced list does not close")


# ---------------------------------------------------------------------------
# The declarations in the tree
# ---------------------------------------------------------------------------


class Tree:
    """The string constants, subject members and subject arrays of the tree.

    The header index reads only the files that can hold a subject value, and a
    file's arrays and comments are read when a declaration site names it. The
    tree is large, and an index of every line in it makes the gate slow for no
    gain.
    """

    def __init__(self, root: Path, raw: dict[str, str]) -> None:
        self.root = root
        self.raw = raw
        self._stripped: dict[str, str] = {}
        self.values: dict[str, set[str]] = {}
        self.members: dict[str, set[str]] = {}
        self._arrays: dict[str, dict[str, list[str]]] = {}
        self._file_consts: dict[str, dict[str, str]] = {}
        for rel, text in raw.items():
            if not rel.endswith(".hpp"):
                continue
            if "nats_subject" in text or CONST.search(text):
                self._index_globals(rel, self.text(rel))

    def text(self, rel: str) -> str:
        stripped = self._stripped.get(rel)
        if stripped is None:
            stripped = strip_comments(self.raw[rel])
            self._stripped[rel] = stripped
        return stripped

    def file_consts(self, rel: str) -> dict[str, str]:
        consts = self._file_consts.get(rel)
        if consts is None:
            consts = {}
            for m in CONST.finditer(self.text(rel)):
                consts.setdefault(m.group(1), m.group(2))
            self._file_consts[rel] = consts
        return consts

    def arrays(self, rel: str) -> dict[str, list[str]]:
        arrays = self._arrays.get(rel)
        if arrays is not None:
            return arrays
        text = self.text(rel)
        arrays = {}
        for m in ARRAY_START.finditer(text):
            body, _ = brace_body(text, m.end() - 1)
            arrays[m.group(1)] = [
                item.strip() for item in split_arguments("(" + body + ")", 0) if item.strip()
            ]
        self._arrays[rel] = arrays
        return arrays

    def _index_globals(self, rel: str, text: str) -> None:
        for m in CONST.finditer(text):
            self.values.setdefault(m.group(1), set()).add(m.group(2))
            self._file_consts.setdefault(rel, {})[m.group(1)] = m.group(2)

        if "nats_subject" not in text:
            return
        starts = list(TYPE_START.finditer(text))
        for index, m in enumerate(starts):
            end = starts[index + 1].start() if index + 1 < len(starts) else len(text)
            member = MEMBER.search(text[m.end() : end])
            if member:
                self.members.setdefault(m.group(1), set()).add(member.group(1))

    def resolve(self, expr: str, rel: str) -> list[str]:
        """The subject or stream-value strings @p expr names."""
        expr = re.sub(r"\s+", "", expr.strip())
        literal = string_literal(expr)
        if literal is not None:
            return [literal]
        if not IDENTIFIER.match(expr):
            raise Unresolved(f"the expression {expr!r} is not a name this check reads")

        parts = expr.split("::")
        last = parts[-1]

        if len(parts) >= 2 and last == "nats_subject":
            owner = parts[-2]
            if owner in self.members:
                values = self.members[owner]
                if len(values) == 1:
                    return [next(iter(values))]
                raise Unresolved(f"{owner}::nats_subject has {len(values)} values")

        local = self.file_consts(rel).get(last)
        if local is not None:
            return [local]
        if last in self.members and len(self.members[last]) == 1:
            return [next(iter(self.members[last]))]
        if last in self.values:
            values = self.values[last]
            if len(values) == 1:
                return [next(iter(values))]
            raise Unresolved(f"the name {last!r} has {len(values)} values: {sorted(values)}")
        raise Unresolved(f"the name {last!r} is not a literal string this check can resolve")

    def resolve_array(self, name: str, rel: str) -> list[str]:
        arrays = self.arrays(rel)
        if name not in arrays:
            raise Unresolved(f"the array {name!r} is not declared in {Path(rel).name}")
        subjects: list[str] = []
        for item in arrays[name]:
            subjects.extend(self.resolve(item, rel))
        return subjects

    def stream_suffix(self, expr: str, rel: str) -> str:
        """The logical suffix of a stream name expression."""
        expr = re.sub(r"\s+", "", expr)
        m = re.fullmatch(r'nats\.make_stream_name\("([^"]*)"\)', expr)
        if m:
            return m.group(1)
        if not IDENTIFIER.match(expr):
            raise Unresolved(f"the stream name {expr!r} is not a name this check reads")
        local = self.file_consts(rel)
        if expr in local:
            return local[expr]
        for m in STREAM_NAME_VAR.finditer(self.text(rel)):
            if m.group(1) == expr:
                return m.group(2)
        raise Unresolved(
            f"the stream name {expr!r} is not a make_stream_name call this check reads"
        )

    def service_name(self, expr: str, rel: str, position: int) -> str:
        expr = re.sub(r"\s+", "", expr)
        literal = string_literal(expr)
        if literal is not None:
            return literal
        if not IDENTIFIER.match(expr):
            raise Unresolved(f"the service name {expr!r} is not a name this check reads")
        for m in FILE_LOCAL_STRING.finditer(self.text(rel)):
            if m.group(1) == expr:
                return m.group(2)
        return self._bound_service(expr, rel, position)

    def _bound_service(self, param: str, rel: str, position: int) -> str:
        """The value a caller passes for a function parameter."""
        function, params = enclosing_function(self.text(rel), position)
        if param not in params:
            raise Unresolved(f"{param!r} is neither a constant here nor a parameter")
        index = params.index(param)
        found: set[str] = set()
        for other_rel, other_text in self.raw.items():
            if function not in other_text:
                continue
            for at in find_call_sites(other_text, function):
                if other_rel == rel:
                    continue
                args = split_arguments(other_text, other_text.index("(", at))
                if index >= len(args):
                    continue
                found.update(self._service_arg(args[index], other_rel))
        if len(found) == 1:
            return next(iter(found))
        raise Unresolved(
            f"the parameter {param!r} of {function} resolves to {sorted(found) or 'nothing'}"
        )

    def _service_arg(self, expr: str, rel: str) -> list[str]:
        literal = string_literal(expr)
        if literal is not None:
            return [literal]
        expr = re.sub(r"\s+", "", expr)
        for m in FILE_LOCAL_STRING.finditer(self.text(rel)):
            if m.group(1) == expr:
                return [m.group(2)]
        return []


def enclosing_function(text: str, position: int) -> tuple[str, list[str]]:
    """The name and parameter names of the function around @p position."""
    candidates = [
        m
        for m in re.finditer(r"(?m)^([A-Za-z_]\w*(?:::\w+)*)\s*\(", text)
        if m.start() < position
    ]
    if not candidates:
        raise Unresolved("the call is not inside a function this check reads")
    match = candidates[-1]
    args = split_arguments(text, match.end() - 1)
    params: list[str] = []
    for arg in args:
        names = re.findall(r"[A-Za-z_]\w*", arg)
        if names:
            params.append(names[-1])
    return match.group(1).split("::")[-1], params


def covering(subjects: list[str]) -> tuple[str, ...]:
    filters: list[str] = []
    for subject in subjects:
        filters.append(subject)
        filters.append(subject + ".>")
    return tuple(filters)


def stream_suffix_for(service: str, group: str) -> str:
    name = service
    if name.startswith("ores."):
        name = name[len("ores.") :]
    if name.endswith(".service"):
        name = name[: -len(".service")]
    if group:
        name += "_" + group
    return name + "_steps"


# ---------------------------------------------------------------------------
# The server's two rules
# ---------------------------------------------------------------------------


def filter_matches(pattern: str, subject: str) -> bool:
    """Whether a stream filter captures a message published on @p subject."""
    p = pattern.split(".")
    s = subject.split(".")
    i = 0
    while i < len(p):
        if p[i] == ">":
            return i < len(s) and i == len(p) - 1
        if i >= len(s):
            return False
        if p[i] != "*" and p[i] != s[i]:
            return False
        i += 1
    return i == len(s)


def filters_overlap(a: str, b: str) -> bool:
    """Whether two filters capture one common subject."""
    ta = a.split(".")
    tb = b.split(".")
    i = 0
    while i < len(ta) and i < len(tb):
        if ta[i] == ">" or tb[i] == ">":
            return True
        if ta[i] != "*" and tb[i] != "*" and ta[i] != tb[i]:
            return False
        i += 1
    return i == len(ta) and i == len(tb)


def has_wildcard(subject: str) -> bool:
    return "*" in subject or ">" in subject


# ---------------------------------------------------------------------------
# The scan
# ---------------------------------------------------------------------------


@dataclass
class Catalogue:
    streams: dict[str, Stream] = field(default_factory=dict)
    consumers: list[Consumer] = field(default_factory=list)
    problems: list[str] = field(default_factory=list)

    def add_stream(self, suffix: str, filters: tuple[str, ...], where: str) -> None:
        existing = self.streams.get(suffix)
        if existing is None:
            self.streams[suffix] = Stream(suffix, filters, [where])
            return
        if set(existing.filters) != set(filters):
            self.problems.append(
                f"one stream, two filter sets: '{suffix}' is ensured with "
                f"{sorted(existing.filters)} at {existing.where[0]} and with "
                f"{sorted(filters)} at {where}\n"
                f"     ensure_stream replaces the subject set, so the later start "
                f"silently drops the subjects only the earlier one covers."
            )
            return
        existing.where.append(where)


def resolve_covering(expr: str, rel: str, tree: Tree) -> list[str]:
    """The subjects inside a covering_subjects(...) argument."""
    expr = re.sub(r"\s+", " ", expr.strip())
    head = re.match(r"nats\s*\.\s*covering_subjects\s*\(", expr)
    if not head:
        raise Unresolved(f"the filter argument {expr!r} is not a covering_subjects call")
    args = split_arguments(expr, head.end() - 1)
    if len(args) != 1:
        raise Unresolved(f"covering_subjects takes one argument, this call has {len(args)}")
    inner = args[0].strip()
    if inner.startswith("{"):
        subjects: list[str] = []
        for item in split_arguments("(" + inner[1:-1] + ")", 0):
            if item.strip():
                subjects.extend(tree.resolve(item, rel))
        return subjects
    if IDENTIFIER.match(re.sub(r"\s+", "", inner)):
        return tree.resolve_array(re.sub(r"\s+", "", inner), rel)
    raise Unresolved(f"covering_subjects is given {inner!r}, which this check cannot read")


def resolve_routes(text: str, rel: str, tree: Tree, routes: str) -> tuple[str, list[str]]:
    """The group and subjects of the step_routes table named @p routes."""
    declaration = None
    for m in STEP_ROUTES.finditer(text):
        if m.group(1) == routes:
            declaration = m
            break
    if declaration is None:
        raise Unresolved(f"the routes table {routes!r} is not declared in {Path(rel).name}")
    group = declaration.group(2) or ""
    serve = text.find("serve_step_commands", declaration.end())
    region = text[declaration.end() : serve if serve >= 0 else len(text)]

    subjects: list[str] = []
    seen = False
    for m in re.finditer(r"\.on\s*\(", region):
        seen = True
        expr = first_argument(region, m.end() - 1).strip()
        try:
            subjects.extend(tree.resolve(expr, rel))
            continue
        except Unresolved:
            pass
        loop = re.search(
            r"for\s*\([^)]*\b" + re.escape(expr) + r"\s*:\s*([A-Za-z_]\w*)\s*\)", region
        )
        if not loop:
            raise Unresolved(f"the subject {expr!r} of {routes} cannot be resolved")
        subjects.extend(tree.resolve_array(loop.group(1), rel))
    if not seen:
        raise Unresolved(f"the routes table {routes!r} declares no subject")
    return group, subjects


TRANSPORT_HEADERS = (
    "projects/ores.nats/include/ores.nats/service/client.hpp",
    "projects/ores.nats/include/ores.nats/service/jetstream_admin.hpp",
)
KNOWN_JS_CALLS = ("js_publish", "js_queue_subscribe", "js_queue_subscribe_multi")


def transport_problems(root: Path) -> list[str]:
    """A transport entry point this check does not read.

    The check knows the JetStream calls a service uses. A fourth call would
    carry declarations this check never sees, so the check fails until the call
    is taught. A service that calls the cnats
    API directly is outside any source check's reach; that is stated, not
    papered over.
    """
    problems: list[str] = []
    client = root / TRANSPORT_HEADERS[0]
    if client.is_file():
        text = strip_comments(client.read_text(errors="replace"))
        for name in sorted(set(re.findall(r"\bjs_[a-z_]+\b", text))):
            if name not in KNOWN_JS_CALLS:
                problems.append(
                    f"the transport offers a JetStream call this check does not "
                    f"read: '{name}' ({TRANSPORT_HEADERS[0]})\n"
                    f"     Teach check_stream_coverage.py the subject and stream it "
                    f"declares, or remove the call."
                )
    admin = root / TRANSPORT_HEADERS[1]
    if admin.is_file():
        text = strip_comments(admin.read_text(errors="replace"))
        for name in sorted(set(re.findall(r"\b(?:ensure|create|add)_\w*stream\w*\s*\(", text))):
            name = name.rstrip("( \n")
            if name != "ensure_stream":
                problems.append(
                    f"the transport offers a stream-creation entry point this check "
                    f"does not read: '{name}' ({TRANSPORT_HEADERS[1]})\n"
                    f"     Teach check_stream_coverage.py the filters it declares, or "
                    f"remove the entry point."
                )
    return problems


def scan(root: Path) -> Catalogue:
    raw = {
        path.relative_to(root).as_posix(): path.read_text(errors="replace")
        for path in project_sources(root)
    }
    tree = Tree(root, raw)
    catalogue = Catalogue()

    for rel, source in raw.items():
        if rel == STEP_HELPER:
            text = tree.text(rel)
        else:
            text = tree.text(rel)
            if "ensure_stream" in text:
                for at in find_call_sites(text, "ensure_stream"):
                    where = f"{rel}:{line_of(text, at)}"
                    try:
                        args = split_arguments(text, text.index("(", at))
                        if len(args) < 2:
                            raise Unresolved("ensure_stream needs a name and a filter")
                        suffix = tree.stream_suffix(args[0], rel)
                        subjects = resolve_covering(args[1], rel, tree)
                        if not subjects:
                            raise Unresolved("ensure_stream covers no subject")
                        catalogue.add_stream(suffix, covering(subjects), where)
                    except Unresolved as error:
                        catalogue.problems.append(f"{where}: ensure_stream: {error}")

            if "serve_step_commands" in text:
                for at in find_call_sites(text, "serve_step_commands"):
                    where = f"{rel}:{line_of(text, at)}"
                    try:
                        args = split_arguments(text, text.index("(", at))
                        if len(args) != 3:
                            raise Unresolved("serve_step_commands takes three arguments")
                        service = tree.service_name(args[1], rel, at)
                        routes = re.sub(r"\s+", "", args[2])
                        if not IDENTIFIER.match(routes):
                            raise Unresolved(f"the routes argument {args[2]!r} is not a table")
                        group, subjects = resolve_routes(text, rel, tree, routes)
                        suffix = stream_suffix_for(service, group)
                        catalogue.add_stream(suffix, covering(subjects), where)
                        catalogue.consumers.append(
                            Consumer(f"{suffix}_consumer", tuple(subjects), suffix, where)
                        )
                    except Unresolved as error:
                        catalogue.problems.append(
                            f"{where}: serve_step_commands: {error}"
                        )

        for call_name in ("js_queue_subscribe", "js_queue_subscribe_multi"):
            # The step helper is modelled through serve_step_commands, so its
            # own generic call is not a declaration to resolve.
            if rel == STEP_HELPER or call_name not in text:
                continue
            for at in find_call_sites(text, call_name):
                where = f"{rel}:{line_of(text, at)}"
                try:
                    args = split_arguments(text, text.index("(", at))
                    if len(args) < 7:
                        raise Unresolved(f"{call_name} takes seven arguments")
                    subjects = tuple(tree.resolve(args[0], rel))
                    named = tree.stream_suffix(args[4], rel)
                    catalogue.consumers.append(
                        Consumer(f"at {where}", subjects, named, where)
                    )
                except Unresolved as error:
                    catalogue.problems.append(f"{where}: js_queue_subscribe: {error}")

    return catalogue


# ---------------------------------------------------------------------------
# The verdict
# ---------------------------------------------------------------------------


def check(root: Path) -> tuple[list[str], Catalogue]:
    catalogue = scan(root)
    findings = list(catalogue.problems)
    findings.extend(transport_problems(root))

    if not catalogue.streams:
        findings.append(
            "no stream declaration found; the scan is not reading the tree's "
            "ensure_stream sites, so it cannot vouch for anything."
        )

    streams = sorted(catalogue.streams.values(), key=lambda s: s.suffix)
    for i, left in enumerate(streams):
        for right in streams[i + 1 :]:
            pair = next(
                (
                    (a, b)
                    for a in left.filters
                    for b in right.filters
                    if a == b or filters_overlap(a, b)
                ),
                None,
            )
            if pair is None:
                continue
            findings.append(
                f"stream subject overlap: '{left.suffix}' filter "
                f"'{pair[0]}' and '{right.suffix}' filter '{pair[1]}'\n"
                f"     {left.where[0]} and {right.where[0]}\n"
                f"     NATS refuses the second with JSStreamSubjectOverlapErr "
                f"(10065), so that service does not start."
            )

    reported_multiple: set[tuple[str, tuple[str, ...]]] = set()
    for consumer in catalogue.consumers:
        for subject in consumer.subjects:
            if has_wildcard(subject):
                findings.append(
                    f"durable subject this check cannot prove: '{subject}'\n"
                    f"     {consumer.where} consumes a filter with a wildcard, and "
                    f"this check proves only named subjects."
                )
                continue
            covering_streams = [
                stream
                for stream in streams
                if any(filter_matches(f, subject) for f in stream.filters)
            ]
            if not covering_streams:
                findings.append(
                    f"durable subject with no stream: '{subject}'\n"
                    f"     {consumer.where} consumes it durably, and no declared "
                    f"stream filter matches it.\n"
                    f"     A publish to it has no stream to hold it, so the step "
                    f"times out after five seconds. Ensure the stream that covers "
                    f"it in the owning service's start path."
                )
                continue
            if len(covering_streams) > 1:
                key = (subject, tuple(s.suffix for s in covering_streams))
                if key not in reported_multiple:
                    reported_multiple.add(key)
                    names = ", ".join(key[1])
                    findings.append(
                        f"durable subject with more than one stream: '{subject}'\n"
                        f"     {consumer.where} consumes it durably, and these "
                        f"streams cover it: {names}."
                    )
            if consumer.named_stream is None:
                continue
            if consumer.named_stream not in catalogue.streams:
                findings.append(
                    f"durable consumer on an unensured stream: "
                    f"'{consumer.named_stream}'\n"
                    f"     {consumer.where} consumes '{subject}' on it, and nothing "
                    f"in the tree ensures that stream."
                )
            elif not any(
                filter_matches(f, subject)
                for f in catalogue.streams[consumer.named_stream].filters
            ):
                findings.append(
                    f"durable consumer outside its stream: '{subject}'\n"
                    f"     {consumer.where} reads it on '{consumer.named_stream}', "
                    f"whose filters do not cover it.\n"
                    f"     The server refuses the consumer filter as not a subset "
                    f"of its stream's subjects."
                )

    return findings, catalogue


def print_catalogue(catalogue: Catalogue) -> None:
    for stream in sorted(catalogue.streams.values(), key=lambda s: s.suffix):
        print(f"stream {stream.suffix}")
        for subject_filter in stream.filters:
            print(f"  {subject_filter}")
        for where in stream.where:
            print(f"  declared at {where}")
    for consumer in catalogue.consumers:
        print(f"consumer {consumer.durable}")
        for subject in consumer.subjects:
            print(f"  {subject}")
        print(f"  read at {consumer.where}")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="check_stream_coverage",
        description="Fail when a durable subject has no stream, or two streams overlap.",
    )
    parser.add_argument(
        "--print-catalogue",
        action="store_true",
        help="Print every stream and durable consumer the check read, then the verdict.",
    )
    args = parser.parse_args(argv)

    findings, catalogue = check(REPO_ROOT)

    if args.print_catalogue:
        print_catalogue(catalogue)
        print()

    if findings:
        print(f"❌ stream coverage: {len(findings)} defect(s):\n")
        for finding in findings:
            print(f"  {finding}\n")
        print(
            "A durable subject needs one stream whose filter covers it, ensured by "
            "the service that reads it (pattern_stream_provisioning.org). Two "
            "streams must not claim one subject (NATS error 10065)."
        )
        return 1

    subjects = {s for consumer in catalogue.consumers for s in consumer.subjects}
    print(
        f"✅ stream coverage: {len(catalogue.streams)} stream(s), "
        f"{len(catalogue.consumers)} durable consumer(s), {len(subjects)} "
        f"subject(s); each durable subject has exactly one stream and no two "
        f"streams overlap"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
