"""Org-mode entity loader.

Parses a restricted subset of org-mode into the same dict structure
produced by the JSON entity loader, so the rest of codegen does not have
to know whether it is reading JSON or org.

The supported subset:

- Frontmatter lines: ``#+key: value`` before the first heading.
- Property drawers: ``:PROPERTIES:`` ... ``:END:`` with ``:KEY: VALUE``
  entries, immediately following a heading or appearing at the top of
  the file.
- Headings: ``* Foo`` (level 1), ``** Bar`` (level 2), etc.
- Plain prose body for each heading.
- Named babel source blocks::

      #+begin_src cpp :name some_name
      ...code...
      #+end_src

- Org tables (pipe-delimited).
- Plain bullet lists (``- item``).

Output is a dict with the same top-level shape as the JSON entity loader:
``{"domain_entity": {...}}`` for compatibility with existing templates.
"""
from __future__ import annotations

import re
from dataclasses import dataclass, field
from functools import lru_cache
from pathlib import Path
from typing import Any

# --------------------------------------------------------------------------
# Profile binding (ORE Studio Variability Model § "Profiles")
#
# A :profile: property on an entity's root Flags drawer names one of the
# catalogued profiles (projects/modeling/variability_<slug>.org). Its
# Assignments table -- Feature | Value -- is the single source of truth for
# the feature defaults it supplies; nothing here duplicates that data.
# Which of the entity dict's three namespaces (root / sql / qt) a feature
# belongs to is fixed by the feature catalogue itself
# (projects/modeling/variability_features.org's grouping).

_PROFILES_DIR = Path(__file__).resolve().parents[3] / "modeling"

# feature name -> which sub-dict of the domain_entity a profile default for
# it is merged into. "" means the root of the domain_entity dict itself.
_FEATURE_NAMESPACE: dict[str, str] = {
    "has_tenant_id": "",
    "has_workspace_id": "",
    "has_parent_id": "sql",
    "system_scope": "sql",
    "nullable_tenant_id": "sql",
    "extra_checks": "sql",
    "extra_delete_sets": "sql",
    "fk_copy_validations": "sql",
    "text_code_validations": "sql",
    "party_id_from_book_id": "sql",
    "party_id_from_session": "sql",
    "rls_tenant_isolation": "sql",
    "rls_party_isolation": "sql",
    "rls_system_tenant_visible": "sql",
    "has_pagination": "qt",
    "has_uuid_primary_key": "qt",
    "has_change_reason_cache": "qt",
    "has_explorer_api": "qt",
    "parent_entity_singular": "qt",
    "has_csv_xml_io": "qt",
    "has_export_macro": "qt",
    "has_version_navigation": "qt",
    "has_readonly_paginated_list": "qt",
    "has_parent_scoped_list": "qt",
    "parent_key_field": "qt",
    "parent_key_param": "qt",
}

_LINK_RE = re.compile(r"\[\[id:[0-9A-Fa-f-]+\]\[([^\]]+)\]\]")


def _profile_feature_name(cell: str) -> str:
    """Extract the plain feature name from an Assignments table's Feature
    cell -- either an ``[[id:...][name]]`` link (linked, real feature) or
    plain text (=name= or bare, structural property listed for context)."""
    m = _LINK_RE.search(cell)
    if m:
        return m.group(1).strip()
    return cell.strip().strip("=").strip()


def _profile_literal_value(raw: str) -> Any | None:
    """Parse an Assignments table's Value cell into a concrete default, or
    None if the cell is prose (a per-entity-required placeholder, not a
    fixed literal -- e.g. parent_entity_singular's row in fk-scoped-child)
    rather than an actual value every adopting entity shares."""
    v = raw.strip()
    if v in ("true", "false"):
        return v == "true"
    # A literal string value is wrapped in =...= or is a bare identifier
    # with no spaces/parens -- anything else (parenthetical prose, "set,
    # e.g. ...") is a placeholder documenting that the feature is required,
    # not a value to default in.
    if v.startswith("=") and v.endswith("="):
        return v.strip("=")
    if v and " " not in v and "(" not in v:
        return v
    return None


@lru_cache(maxsize=None)
def _load_profile_assignments(slug: str) -> tuple[tuple[str, Any], ...]:
    """Parse projects/modeling/variability_<slug>.org's Assignments table
    into (feature_name, value) pairs, skipping non-literal (per-entity
    required, no fixed default) rows. Cached: the same profile is resolved
    for many entities in one codegen run."""
    path = _PROFILES_DIR / f"variability_{slug.replace('-', '_')}.org"
    if not path.is_file():
        raise ValueError(
            f"unknown profile '{slug}': {path} does not exist "
            f"(see projects/modeling/variability_profiles.org for the catalogue)"
        )
    doc = parse_org(path.read_text(encoding="utf-8"))
    assignments = _section(doc.root, "Assignments")
    if not assignments:
        return ()
    rows = _parse_org_table_rows(assignments)
    out: list[tuple[str, Any]] = []
    for row in rows:
        name = _profile_feature_name(row.get("Feature", ""))
        value = _profile_literal_value(row.get("Value", ""))
        if name and value is not None:
            out.append((name, value))
    return tuple(out)


def _parse_physical_space_table(root: OrgNode) -> dict[str, bool]:
    """Parse a ``* Physical space`` heading's ``| Address | Enabled |`` table
    (if present) into ``{address: enabled}`` -- the same shape
    ``_enabled_overrides`` (physical_space.py) reads from an ``ores.*.enabled``
    drawer property, just table-authored for HTML-export visibility instead
    of buried in a properties drawer. One row per address whose default
    admission this doc wants to override; an address not listed here is left
    to whatever else resolves it (a more specific/general override, or the
    node's own ``#+default:``)."""
    section = _section(root, "Physical space")
    if not section:
        return {}
    out: dict[str, bool] = {}
    for row in _parse_org_table_rows(section):
        addr = row.get("Address", "").strip().strip("=")
        val = row.get("Enabled", "").strip().lower()
        if addr and val in ("true", "false"):
            out[addr] = val == "true"
    return out


@lru_cache(maxsize=None)
def _load_profile_address_overrides(slug: str) -> tuple[tuple[str, bool], ...]:
    """A profile's own ``* Physical space`` table, as ``(address, enabled)``
    pairs a bound entity inherits as defaults (its own table, if any, always
    wins -- see :func:`read_physical_space_overrides`). Cached like
    :func:`_load_profile_assignments`."""
    path = _PROFILES_DIR / f"variability_{slug.replace('-', '_')}.org"
    if not path.is_file():
        raise ValueError(
            f"unknown profile '{slug}': {path} does not exist "
            f"(see projects/modeling/variability_profiles.org for the catalogue)"
        )
    doc = parse_org(path.read_text(encoding="utf-8"))
    return tuple(_parse_physical_space_table(doc.root).items())


def _ensure_profile_binding(doc: "OrgDocument") -> None:
    """Reject :profile: outside the * Flags section -- the single canonical
    binding point for profiles.

    Two binding points with different behaviour is a footgun -- a single,
    canonical place is simpler and already the convention every entity
    model follows. Every reader calls this so a file-level :profile: fails
    loudly on all paths, not just the physical-space override pass."""
    if "profile" in doc.file_properties:
        raise ValueError(
            ":profile: found in file-level :PROPERTIES: drawer — "
            "move it to the * Flags section's :PROPERTIES: drawer instead. "
            "Only * Flags is the canonical binding point for profiles.")


def read_physical_space_overrides(doc: "OrgDocument") -> dict[str, bool]:
    """An entity doc's effective ``ores.*.enabled`` overrides from the
    ``* Physical space`` table mechanism: its bound profile's table (if any)
    supplies defaults, its own table (if any) wins over those -- the same
    "explicit beats profile default" rule every other profile-carried value
    follows. Returns ``{"<address>.enabled": bool}`` keys ready to merge into
    the raw drawer-properties dict :func:`resolve_targets` uses to decide
    which archetypes are even attempted, so this must run *before* any
    model-specific parsing (see the call site in generate.py's
    ``_read_drawer_properties``)."""
    _ensure_profile_binding(doc)

    merged: dict[str, bool] = {}
    sources: dict[str, str] = {}

    # Read :profile: from the * Flags section — the single canonical binding
    # point, consistent with how _profile_namespace_defaults reads it for
    # feature assignments.
    flags = _section(doc.root, "Flags")
    flags_profile = flags.properties.get("profile") if flags else None
    for slug in _parse_profile_list(flags_profile):
        for addr, enabled in _load_profile_address_overrides(slug):
            key = f"{addr}.enabled"
            if key in merged and merged[key] != enabled:
                raise ValueError(_profile_conflict_message(
                    addr, sources[key], merged[key], slug, enabled))
            merged[key] = enabled
            sources[key] = slug

    # The doc's own table always wins over any profile default -- same
    # "explicit beats profile" rule every other profile-carried value follows.
    for addr, enabled in _parse_physical_space_table(doc.root).items():
        merged[f"{addr}.enabled"] = enabled
    return merged


def _parse_profile_list(raw: str | list[str] | None) -> list[str]:
    """Parse a ``:profile:`` drawer value into an ordered list of slugs.

    Comma-separated to bind against more than one profile at once -- each
    should compose an orthogonal trait (e.g. a shape profile like
    ``simple-lookup`` plus an unrelated enablement profile like
    ``artefact-staging-only``); see :func:`_apply_profile`'s conflict check
    for what happens if two disagree on the same feature."""
    if not raw:
        return []
    if isinstance(raw, list):
        return raw
    return [s.strip() for s in raw.split(",") if s.strip()]


def _profile_conflict_message(
    key: str, slug_a: str, value_a: Any, slug_b: str, value_b: Any,
) -> str:
    return (
        f"conflicting profiles on '{key}': '{slug_a}' says {value_a!r}, "
        f"'{slug_b}' says {value_b!r} -- profiles bound together must "
        "compose orthogonal traits, not disagree on the same feature"
    )


def _profile_namespace_defaults(
    slugs: str | list[str] | None, namespace: str,
) -> dict[str, Any]:
    """The subset of a profile list's Assignments that belongs to one
    namespace (``""``/``"sql"``/``"qt"``), as a plain dict -- used to seed a
    facet's raw properties *before* that facet derives any computed flag
    from them (e.g. ``ores.cpp.qt``'s ``has_toolbar`` from
    ``has_version_navigation``), so the derivation sees the profile's
    values rather than their absence."""
    out: dict[str, Any] = {}
    sources: dict[str, str] = {}
    for slug in _parse_profile_list(slugs):
        for feature, value in _load_profile_assignments(slug):
            if _FEATURE_NAMESPACE.get(feature) != namespace:
                continue
            if feature in out and out[feature] != value:
                raise ValueError(_profile_conflict_message(
                    feature, sources[feature], out[feature], slug, value))
            out[feature] = value
            sources[feature] = slug
    return out


def _apply_profile(de: dict[str, Any]) -> None:
    """If de['profile'] names one or more catalogued profiles, merge their
    Assignments as defaults into de (root / sql / qt namespaces per
    feature), in list order. An already-explicit value at the entity level
    always wins -- a profile supplies defaults, it never overrides what the
    model author wrote. Two profiles bound together that disagree on the
    same feature raise rather than silently picking one -- see
    :func:`_profile_conflict_message`.

    This is the final safety-net pass over the whole ``de`` dict; namespaces
    whose facet derives computed flags from these features (currently
    ``qt``) must also seed those defaults *before* that facet's own parsing
    via :func:`_profile_namespace_defaults`, since by the time this runs the
    derivation has already happened and setdefault here is too late to
    affect it."""
    slugs = _parse_profile_list(de.get("profile"))
    if not slugs:
        return
    # Tracks only what the profiles-in-this-list have contributed among
    # themselves, independent of de's own pre-existing state -- an entity's
    # own explicit value (which setdefault always leaves alone regardless)
    # must never trip this check, only two *profiles* disagreeing with
    # each other should.
    applied: dict[str, tuple[str, Any]] = {}
    for slug in slugs:
        for feature, value in _load_profile_assignments(slug):
            if feature in applied:
                prev_slug, prev_value = applied[feature]
                if prev_value != value:
                    raise ValueError(_profile_conflict_message(
                        feature, prev_slug, prev_value, slug, value))
                continue  # same value repeated by a later profile: no-op
            applied[feature] = (slug, value)
            namespace = _FEATURE_NAMESPACE.get(feature)
            if namespace is None:
                # Not in the catalogue's namespace map (e.g. a structural
                # property incidentally listed for context) -- skip rather
                # than guess where it belongs.
                continue
            if namespace == "":
                de.setdefault(feature, value)
            else:
                de.setdefault(namespace, {})
                de[namespace].setdefault(feature, value)


# --------------------------------------------------------------------------
# Low-level parsing: org file -> tree of nodes


@dataclass
class SrcBlock:
    """A named babel source block with its header arguments."""
    name: str
    lang: str
    code: str
    implements: str | None = None  # kind UUID this block fills (if any)


@dataclass
class OrgNode:
    """A heading-rooted node in the org tree."""
    level: int = 0
    title: str = ""
    org_id: str | None = None
    properties: dict[str, str] = field(default_factory=dict)
    body_lines: list[str] = field(default_factory=list)
    # Block lookup by name; the value is the raw code string for backwards
    # compatibility with callers that only need the code body.
    src_blocks: dict[str, str] = field(default_factory=dict)
    # Full list of all source blocks under this node, including their
    # ``:implements`` header arg for kind-UUID matching.
    src_blocks_list: list[SrcBlock] = field(default_factory=list)
    tables: list[list[dict[str, str]]] = field(default_factory=list)
    bullet_lists: list[list[str]] = field(default_factory=list)
    children: list["OrgNode"] = field(default_factory=list)


@dataclass
class OrgDocument:
    frontmatter: dict[str, str] = field(default_factory=dict)
    file_properties: dict[str, str] = field(default_factory=dict)
    root: OrgNode = field(default_factory=OrgNode)


_HEADING_RE = re.compile(r"^(\*+)\s+(.+?)\s*$")
_FRONTMATTER_RE = re.compile(r"^#\+([A-Za-z_][A-Za-z_0-9]*)\s*:\s*(.*)$")
# Keys may contain dots and hyphens so MASD activation keys such as
# ``:ores.cpp.service-app.enabled:`` are captured, not just plain identifiers.
_DRAWER_PROP_RE = re.compile(r"^\s*:([A-Za-z_][A-Za-z0-9_.-]*)\s*:\s*(.*)$")
_SRC_BEGIN_RE = re.compile(
    r"^\s*#\+begin_src\s+(\S+)(?:\s+(.*))?\s*$", re.IGNORECASE
)
_SRC_END_RE = re.compile(r"^\s*#\+end_src\s*$", re.IGNORECASE)
_COMMENT_BEGIN_RE = re.compile(r"^\s*#\+begin_comment\s*$", re.IGNORECASE)
_COMMENT_END_RE = re.compile(r"^\s*#\+end_comment\s*$", re.IGNORECASE)
_SRC_NAME_RE = re.compile(r":name\s+(\S+)")
_SRC_IMPLEMENTS_RE = re.compile(r":implements\s+(\S+)")
_BULLET_RE = re.compile(r"^\s*-\s+(.*)$")
_TABLE_ROW_RE = re.compile(r"^\s*\|(.+)\|\s*$")
_TABLE_SEP_RE = re.compile(r"^\s*\|[-+|]+\|\s*$")


def parse_org(text: str) -> OrgDocument:
    """Parse an org-mode source string into an OrgDocument tree."""
    doc = OrgDocument()
    lines = text.splitlines()

    # Stack of nodes; doc.root is always at the bottom.
    stack: list[OrgNode] = [doc.root]
    current = doc.root
    in_drawer = False
    in_src_block = False
    in_comment_block = False
    src_lang: str | None = None
    src_name: str | None = None
    src_implements: str | None = None
    src_lines: list[str] = []
    pending_table: list[list[str]] = []  # list of raw row token-lists

    seen_first_heading = False

    def close_table_if_open() -> None:
        nonlocal pending_table
        if pending_table:
            current.tables.append(_normalise_table(pending_table))
            pending_table = []

    def close_bullet_list_if_open() -> None:
        # Bullet lists are flushed when a non-bullet line appears.
        # We accumulate into a single list when consecutive.
        pass  # handled inline

    accumulating_bullets: list[str] = []

    def flush_bullets() -> None:
        nonlocal accumulating_bullets
        if accumulating_bullets:
            current.bullet_lists.append(accumulating_bullets)
            accumulating_bullets = []

    for raw_line in lines:
        line = raw_line.rstrip("\n")

        # Inside a source block: collect verbatim.
        if in_src_block:
            if _SRC_END_RE.match(line):
                code = "\n".join(src_lines)
                block = SrcBlock(
                    name=src_name or "",
                    lang=src_lang or "",
                    code=code,
                    implements=src_implements,
                )
                current.src_blocks_list.append(block)
                if src_name is not None:
                    current.src_blocks[src_name] = code
                in_src_block = False
                src_lang = None
                src_name = None
                src_implements = None
                src_lines = []
            else:
                src_lines.append(line)
            continue

        # Inside a comment block: author-only prose, never emitted into
        # generated code. Ignore every line until #+end_comment.
        if in_comment_block:
            if _COMMENT_END_RE.match(line):
                in_comment_block = False
            continue

        # Start of a comment block.
        if _COMMENT_BEGIN_RE.match(line):
            close_table_if_open()
            flush_bullets()
            in_comment_block = True
            continue

        # Inside a property drawer.
        if in_drawer:
            if line.strip().upper() == ":END:":
                in_drawer = False
                continue
            m = _DRAWER_PROP_RE.match(line)
            if m:
                key, val = m.group(1), m.group(2).strip()
                # Org-mode property keys are case-insensitive — normalise
                # only the ID detection (the rest stays case-preserving).
                is_id = key.upper() == "ID"
                # File-level drawer goes into doc.file_properties
                # before any heading.
                if not seen_first_heading and current is doc.root:
                    doc.file_properties[key] = val
                    if is_id:
                        doc.root.org_id = val
                else:
                    current.properties[key] = val
                    if is_id:
                        current.org_id = val
            continue

        # Start of a property drawer.
        if line.strip().upper() == ":PROPERTIES:":
            in_drawer = True
            continue

        # Start of a source block.
        m = _SRC_BEGIN_RE.match(line)
        if m:
            close_table_if_open()
            flush_bullets()
            in_src_block = True
            src_lang = m.group(1)
            rest = m.group(2) or ""
            name_match = _SRC_NAME_RE.search(rest)
            # Header arg values may be quoted (e.g. :name "foo") — strip
            # surrounding single or double quotes so downstream lookups
            # work whether the author quoted the value or not.
            src_name = name_match.group(1).strip("\"'") if name_match else None
            impl_match = _SRC_IMPLEMENTS_RE.search(rest)
            src_implements = (
                impl_match.group(1).strip("\"'") if impl_match else None
            )
            src_lines = []
            continue

        # Frontmatter line (before first heading).
        if not seen_first_heading:
            fm = _FRONTMATTER_RE.match(line)
            if fm:
                doc.frontmatter[fm.group(1)] = fm.group(2).strip()
                continue

        # Heading.
        hm = _HEADING_RE.match(line)
        if hm:
            close_table_if_open()
            flush_bullets()
            seen_first_heading = True
            level = len(hm.group(1))
            title = hm.group(2).strip()

            # Pop stack until parent of this heading.
            while stack and stack[-1].level >= level:
                stack.pop()
            parent = stack[-1] if stack else doc.root
            node = OrgNode(level=level, title=title)
            parent.children.append(node)
            stack.append(node)
            current = node
            continue

        # Table row.
        if _TABLE_SEP_RE.match(line):
            # Separator after header — ignore.
            continue
        tm = _TABLE_ROW_RE.match(line)
        if tm:
            cells = [c.strip() for c in tm.group(1).split("|")]
            pending_table.append(cells)
            continue
        else:
            close_table_if_open()

        # Bullet item. Bullets are tracked both as a structured
        # bullet_lists entry (for semantic uses) AND as raw body lines
        # (so _strip_body preserves them inside description prose at
        # their original position).
        bm = _BULLET_RE.match(line)
        if bm:
            accumulating_bullets.append(bm.group(1).strip())
            current.body_lines.append(line)
            continue
        else:
            flush_bullets()

        # Plain prose body.
        current.body_lines.append(line)

    # Final flushes.
    close_table_if_open()
    flush_bullets()
    return doc


def _normalise_table(rows: list[list[str]]) -> list[dict[str, str]]:
    """Convert an org table (first row is the header) into list[dict]."""
    if not rows:
        return []
    headers = rows[0]
    out: list[dict[str, str]] = []
    for row in rows[1:]:
        entry: dict[str, str] = {}
        for i, header in enumerate(headers):
            val = row[i] if i < len(row) else ""
            entry[header] = val
        out.append(entry)
    return out


# --------------------------------------------------------------------------
# Higher level: org tree -> codegen model dict


def _section(node: OrgNode, title: str) -> OrgNode | None:
    """Find a child section by case-insensitive title."""
    for c in node.children:
        if c.title.lower() == title.lower():
            return c
    return None


# org verbatim markup is single-line by spec, but _strip_body's default
# keeps the historical cross-line matching: entity/model prose uses
# multi-line ``=...=`` spans and the generated files encode that
# behaviour. Callers that strip prose containing unpaired ``=`` (e.g.
# "asset_class=ir") pass per_line=True instead, so a bare ``=`` never
# pairs with a later one across a newline.
#
# The marker must sit at a word boundary on both sides, as real org
# requires: an ``=`` glued to a word (e.g. the equals signs in
# "1=Inactive, 2=Unsent") is prose, not verbatim markup. Without the
# boundary guards, that prose pairs its ``1=`` with the following
# ``2=`` and the stripped text silently drops the equals signs.
_ORG_VERBATIM_RE = re.compile(r"(?<!\w)=([^=\s][^=]*?)=(?!\w)")


def _strip_org_markup(text: str) -> str:
    """Strip org-mode inline markup that should not appear in codegen output.

    Currently strips ``=foo=`` verbatim markers, leaving the bare token.
    Other markup (``*bold*``, ``/italic/``, ``~code~``) could be added if
    we adopt them in models."""
    return _ORG_VERBATIM_RE.sub(r"\1", text)


def _strip_body(node: OrgNode, per_line: bool = False) -> str:
    """Return the prose body of a node, trimmed of leading/trailing blank
    lines and with org-mode verbatim markup stripped.

    per_line strips the markup line-by-line: an unpaired ``=`` in prose
    (e.g. "asset_class=ir") must not pair with a later ``=`` on a
    different line. The default keeps the historical cross-line matching
    (see _ORG_VERBATIM_RE)."""
    lines = node.body_lines
    while lines and not lines[0].strip():
        lines = lines[1:]
    while lines and not lines[-1].strip():
        lines = lines[:-1]
    if per_line:
        return "\n".join(_strip_org_markup(line) for line in lines)
    return _strip_org_markup("\n".join(lines))


def _parse_typed(value: str) -> Any:
    """Decode a property value into Python types where obvious.

    ``str.isdigit()`` is wrong for negative numbers and floats (e.g.
    ``-5``, ``3.14`` both return False), so we round-trip through ``int``
    then ``float`` and fall through to the raw string."""
    low = value.lower()
    if low == "true":
        return True
    if low == "false":
        return False
    try:
        return int(value)
    except ValueError:
        try:
            return float(value)
        except ValueError:
            return value


def _includes_from_named_block(node: OrgNode) -> list[str]:
    """Extract include tokens from a named ``includes`` babel block.

    The babel block is real C++ with ``#include`` directives. We strip the
    ``#include `` prefix so the returned tokens match the angle-bracket /
    double-quote form the JSON model stored. Non-``#include`` lines
    (comments, blanks) are ignored — otherwise they would be re-emitted by
    the include-list template as ``#include // foo``, which is invalid
    C++."""
    code = node.src_blocks.get("includes", "")
    out: list[str] = []
    for raw in code.splitlines():
        line = raw.strip()
        if line.startswith("#include"):
            out.append(line[len("#include"):].strip())
    return out


def _description_and_detail(node: OrgNode) -> tuple[str, str]:
    """Split prose body into description (first paragraph) and detail (rest).

    Within each paragraph, wrapped lines are joined with a single space.
    Paragraph breaks (blank lines) between paragraphs are preserved so
    multi-paragraph detail renders as multi-paragraph C++ comments."""
    body = _strip_body(node)
    parts = re.split(r"\n\s*\n", body, maxsplit=1)
    description = re.sub(r"\s*\n\s*", " ", parts[0]).strip()
    if len(parts) > 1:
        paragraphs = re.split(r"\n\s*\n", parts[1])
        detail = "\n\n".join(
            re.sub(r"\s*\n\s*", " ", p).strip() for p in paragraphs
        )
    else:
        detail = ""
    return description, detail


_SQL_STRING_TYPES = {"text", "string", "varchar", "char"}


def _sql_quote_default(raw: str) -> str:
    """SQL-quote a raw (unquoted) default literal for a text-typed column.

    Authors write the plain value (e.g. ``ACT/360``) in the org model —
    quoting is a SQL-syntax concern the template/loader owns, not
    something every field author should have to get right. Function
    calls / expressions (contain '(') and values already quoted are
    passed through unchanged. Embedded single quotes are doubled per
    SQL string-literal escaping."""
    stripped = raw.strip()
    if not stripped or "(" in stripped or stripped[0] in "'\"":
        return raw
    return "'" + stripped.replace("'", "''") + "'"


def _column_node_to_dict(node: OrgNode) -> dict[str, Any]:
    """Convert a column or natural-key heading into a model column dict."""
    out: dict[str, Any] = {}
    out["name"] = node.title  # caller may rename to 'column' for natural_keys
    for k, v in node.properties.items():
        key = k.lower()
        # Mustache treats numeric 0/0.0 as falsy; keep the raw string for keys
        # whose template guards on presence ({{#default}}, {{#default_value}})
        # so a zero value (e.g. :default: 0 on a SQL column) is not skipped.
        # The table/junction column parsers already do this; the unified
        # domain-entity parser must too.
        if key in ("default", "default_value"):
            out[key] = v
        else:
            out[key] = _parse_typed(v)
    # is_enum columns: default/default_value feeds the C++ struct's member
    # initializer directly (cpp_domain_type_class.hpp.mustache emits it
    # unescaped), so it must be the scoped-enum expression (e.g.
    # domain::asset_class::fx), never SQL-quoted -- SQL-quoting a bare word
    # produces a single-quoted string/char literal, which does not compile
    # against an enum class member. Checked and confirmed: domain_entity's
    # SQL template does not currently emit a DEFAULT clause from this field
    # at all, so there is no competing SQL-side consumer to keep quoted.
    if out.get("is_enum") and out.get("cpp_type"):
        for key in ("default", "default_value"):
            if key in out and isinstance(out[key], str) and "::" not in out[key]:
                out[key] = f"{out['cpp_type']}::{out[key].strip()}"
    # SQL-quote string-typed defaults here so field authors write the plain
    # value (e.g. :default: ACT/360, not :default: 'ACT/360') — SQL quoting
    # syntax is the template's concern, not the model's.
    elif out.get("type") in _SQL_STRING_TYPES:
        for key in ("default", "default_value"):
            if key in out and isinstance(out[key], str):
                out[key] = _sql_quote_default(out[key])
    description, detail = _description_and_detail(node)
    if description:
        out["description"] = description
    if detail:
        out["detail"] = detail
    if "generator" in node.src_blocks:
        out["generator_expr"] = node.src_blocks["generator"]
    return out


def _natural_key_node_to_dict(node: OrgNode) -> dict[str, Any]:
    d = _column_node_to_dict(node)
    # Natural keys use 'column' instead of 'name' in the JSON model.
    d["column"] = d.pop("name")
    return d


# Per-field flags in a unified ``* Columns`` section that route a field into
# ``primary_key``/``natural_keys`` instead of the plain ``columns`` list.
# Metadata, not schema — stripped from the rendered field dict.
_KEY_ROLE_FLAGS = ("primary_key", "natural_key")


def _split_columns_section(cols: OrgNode) -> tuple[
    list[dict[str, Any]], list[dict[str, Any]], list[dict[str, Any]]
]:
    """Split a unified ``* Columns`` section's children into three ordered
    buckets by their ``:primary_key:``/``:natural_key:`` flag: primary-key
    fields, natural-key fields, and plain fields.

    A field carries at most one of the two flags. Order within each bucket
    is declaration order — the sole mechanism for compound-key column
    order (no separate ordinal)."""
    primary_key_fields: list[dict[str, Any]] = []
    natural_key_fields: list[dict[str, Any]] = []
    plain_fields: list[dict[str, Any]] = []
    for child in cols.children:
        is_primary_key = _parse_typed(child.properties.get("primary_key", "false")) is True
        is_natural_key = _parse_typed(child.properties.get("natural_key", "false")) is True
        if is_primary_key and is_natural_key:
            raise ValueError(
                f"Column '{child.title}' cannot be both :primary_key: and "
                ":natural_key:"
            )
        if is_primary_key:
            d = _natural_key_node_to_dict(child)
            for flag in _KEY_ROLE_FLAGS:
                d.pop(flag, None)
            primary_key_fields.append(d)
        elif is_natural_key:
            d = _natural_key_node_to_dict(child)
            for flag in _KEY_ROLE_FLAGS:
                d.pop(flag, None)
            natural_key_fields.append(d)
        else:
            d = _column_node_to_dict(child)
            for flag in _KEY_ROLE_FLAGS:
                d.pop(flag, None)
            plain_fields.append(d)
    return primary_key_fields, natural_key_fields, plain_fields


def _primary_key_dict(fields: list[dict[str, Any]]) -> dict[str, Any]:
    """Project a non-empty, ordered list of primary-key fields into the
    canonical ``primary_key`` dict: back-compat single-column scalars
    (mirroring the first field, for every existing consumer that only
    knows about a single-column key) plus the full ordered ``columns``
    list (the new shape a compound-key-aware consumer reads)."""
    out = dict(fields[0])
    out["columns"] = fields
    return out


def _soft_fk_validation_node_to_dict(node: OrgNode) -> dict[str, Any]:
    """Convert a soft FK validation heading into a template-ready dict.

    The heading title becomes ``column``; the PROPERTIES drawer supplies
    ``table``, ``error_message``, and optional boolean flags
    ``nullable``, ``use_no_tenant``, ``use_system_tenant``. ``target_column``
    -- the only property name the sql_schema_domain_entity_create template
    actually reads for this -- defaults to ``id`` (every UUID-keyed
    entity's PK); set it explicitly when the referenced table's key is
    not ``id`` (e.g. a code-keyed lookup entity like ``calendar``, whose
    PK column is ``code``). There is deliberately no ``referenced_column``
    alias: an earlier version of this function produced one, calendar_rule
    and calendar_exception's models were written against it instead of
    the real ``target_column`` key, and the template silently ignored it
    -- their soft-FK checks against ``calendars.code`` never worked
    despite looking overridden. Keep this a single, unambiguous key.
    """
    out: dict[str, Any] = {"column": node.title}
    for k, v in node.properties.items():
        out[k.lower()] = _parse_typed(v)
    # Target table's PK column defaults to "id" -- the shape every
    # existing soft-FK target uses. Override via :target_column: for a
    # target with a differently-named PK (e.g. ores_assets_images_tbl's
    # image_id).
    out.setdefault("target_column", "id")
    return out


# --------------------------------------------------------------------------
# Soft-FK parent resolution (eventing-integration-test seeding)
#
# An eventing integration test writes a child row whose mandatory soft FK
# references another entity; the child's insert trigger rejects a synthetic
# key that matches no active parent row, so the test must write an active
# parent first. generate_from_model (core.py) resolves each FK's :table: to
# the parent entity's modeling org via this scan, then loads the parent's
# raw model to learn its generator facet, audit-group status and its own
# mandatory-FK needs (e.g. portfolio's mandatory party_id, which the
# template seeds before the portfolio seed).

@lru_cache(maxsize=None)
def _entity_org_by_table(projects_dir: Path) -> dict[str, dict[str, Any]]:
    """Map every SQL ``:tablename:`` to its entity's modeling org.

    Raw-text scan (no org parse), cached per process: only the SQL Flags
    drawer's ``:tablename:`` and the frontmatter ``#+entity_singular:`` are
    read from each file under ``projects_dir/*/modeling/``. Called from
    generate_from_model's FK enrichment, which runs once per rendered
    unit, so the whole-tree scan must stay cheap.
    """
    out: dict[str, dict[str, Any]] = {}
    for org in sorted(projects_dir.glob("*/modeling/*.org")):
        text = org.read_text(encoding="utf-8", errors="replace")
        # Only entity models count as FK parents: documentation orgs (e.g.
        # the meta-model's knowledge doc) mention :tablename: in prose and
        # in examples -- a doc's example would shadow the real entity's
        # mapping and silently disable seeding for it. The frontmatter
        # entity_singular is the entity marker; the tablename must be a
        # plain lowercase identifier (the =:tablename:= org-emphasis form
        # in prose is not).
        sm = re.search(r"^#\+entity_singular:\s*(\S+)", text, re.M)
        if not sm:
            continue
        m = re.search(r":tablename:\s+([a-z][a-z0-9_]*_tbl)", text)
        if not m:
            continue
        out[m.group(1)] = {"org": org, "entity_singular": sm.group(1)}
    return out


def _table_display(node: OrgNode) -> list[dict[str, str]]:
    if not node.tables:
        return []
    rows: list[dict[str, str]] = []
    for row in node.tables[0]:
        rows.append({"column": row.get("column", ""), "header": row.get("header", "")})
    return rows


def _detail_fields(node: OrgNode) -> list[dict[str, Any]]:
    if not node.tables:
        return []
    out: list[dict[str, Any]] = []
    for row in node.tables[0]:
        entry: dict[str, Any] = {}
        for k, v in row.items():
            # A blank cell means "use the default", not "explicitly empty
            # string" -- drop it rather than keying it in with '', so
            # core.py's f.setdefault(...) calls still fire for this field.
            # Table columns are per-*entity* (every row shares the same
            # header), so a column relevant to only one field (e.g.
            # spin_min override) still works fine left blank on every
            # other row.
            if v == "":
                continue
            entry[k] = _parse_typed(v)
        out.append(entry)
    return out


def _qt_columns(node: OrgNode) -> list[dict[str, Any]]:
    """Convert the Qt 'Columns (Qt model)' table into the dict list shape."""
    if not node.tables:
        return []
    out: list[dict[str, Any]] = []
    for row in node.tables[0]:
        entry: dict[str, Any] = {}
        for k, v in row.items():
            if k == "type":
                # Replace the readability column with the type-flag the JSON model uses.
                low = v.lower()
                if low == "string":
                    entry["is_string"] = True
                elif low == "int":
                    entry["is_int"] = True
                elif low in ("bool", "boolean"):
                    entry["is_bool"] = True
                elif low == "double":
                    entry["is_double"] = True
                elif low == "timestamp":
                    entry["is_timestamp"] = True
                elif low == "uuid":
                    entry["is_uuid"] = True
                elif low == "date":
                    entry["is_date"] = True
                continue
            entry[k] = _parse_typed(v)
        out.append(entry)
    return out


def _qt_icon_columns(node: OrgNode) -> list[dict[str, Any]]:
    """Convert the Qt 'Icon columns (Qt model)' table into the dict list shape.

    Each row is one Qt::DecorationRole column: `column` (the Column enum
    value), `accessor` (a function taking ImageCache& then 1-2 row fields,
    e.g. currency_flag_icon), `field1` (always), `field2` (optional — a
    second row field for a composited two-field icon, e.g. a currency pair).
    `has_field2` is computed so the template can conditionally emit the
    second argument without any templating logic beyond section presence.

    `is_pair` marks a *visually* wide (roughly 2:1) composited icon that
    needs the view's iconSize widened past the single-flag default —
    defaults to true when field2 is set (the two-field case, e.g.
    currency_pair's own base_currency/quote_currency), but must be set
    explicitly for a single-field accessor that still produces a pair icon
    internally (e.g. currency_flag_icon_from_pair_code splitting one
    "BASE/QUOTE" field) — has_field2 alone can't detect that case.
    """
    if not node.tables:
        return []
    out: list[dict[str, Any]] = []
    for row in node.tables[0]:
        entry: dict[str, Any] = {}
        for k, v in row.items():
            entry[k] = _parse_typed(v)
        entry["has_field2"] = bool(entry.get("field2"))
        if "is_pair" not in entry:
            entry["is_pair"] = entry["has_field2"]
        out.append(entry)
    return out


def _qt_setting_gated_actions(node: OrgNode) -> list[dict[str, Any]]:
    """Convert the Qt 'Setting-gated actions' table into the dict list shape.

    Each row names one existing QAction* member (`action`, without the
    trailing underscore) whose visibility is gated by a boolean system
    setting (`setting`). Wired via a single shared
    SettingGatedActionController per mdi_window/detail_dialog rather
    than duplicating the subscribe/notify/query mechanism per action.
    """
    if not node.tables:
        return []
    return [
        {k: _parse_typed(v) for k, v in row.items()} for row in node.tables[0]
    ]


def _qt_related_entity_shortcuts(node: OrgNode) -> list[dict[str, Any]]:
    """Convert the Qt 'Related entity shortcuts' table into the dict list shape.

    Each row is a toolbar shortcut to a related entity's own list window —
    e.g. currency's Rounding Type / Monetary Nature / Market Tier combos each
    reference a small lookup entity worth a one-click detour to. `signal`
    names the emitted/relayed Qt signal (`show{signal}Requested`); `icon`
    is an Icon:: enum value; `tooltip`/`label` are the toolbar action's
    tooltip and button text. Wiring the signal to the target entity's own
    controller happens in the plugin's composition root (e.g.
    RefdataPlugin), not here — cross-controller wiring is inherently
    plugin-level, same as every other controller-to-controller signal."""
    if not node.tables:
        return []
    return [
        {k: _parse_typed(v) for k, v in row.items()} for row in node.tables[0]
    ]


def _custom_methods(node: OrgNode) -> list[dict[str, Any]]:
    """Each custom method is a sub-heading with its own ID, prose body
    explaining intent, and named src blocks for declaration/implementation."""
    out: list[dict[str, Any]] = []
    for c in node.children:
        method: dict[str, Any] = {"name": c.title}
        if c.org_id:
            method["id"] = c.org_id
        method["description"] = _strip_body(c)
        method.update(c.src_blocks)
        out.append(method)
    return out


def _collect_implementations(root: OrgNode) -> dict[str, list[str]]:
    """Walk the tree and return ``{kind_uuid: [block_code, ...]}`` for every
    babel block carrying an ``:implements <UUID>`` header argument."""
    out: dict[str, list[str]] = {}

    def walk(node: OrgNode) -> None:
        for block in node.src_blocks_list:
            if block.implements:
                out.setdefault(block.implements, []).append(block.code)
        for child in node.children:
            walk(child)

    walk(root)
    return out


def org_document_to_model(doc: OrgDocument) -> dict[str, Any]:
    """Convert a parsed OrgDocument into the canonical model dict.

    The output mirrors the JSON ``_domain_entity.json`` shape so the rest
    of codegen can consume it unchanged.
    """
    _ensure_profile_binding(doc)
    de: dict[str, Any] = {}

    # Frontmatter contains entity-wide string keys.
    # NB: #+description describes the *document* (the codegen model), not the
    # modelled thing. The modelled thing's description is the prose body
    # between the frontmatter and the first heading (captured below).
    fm = doc.frontmatter
    for k in (
        "brief", "entity_singular", "entity_plural", "entity_title",
    ):
        if k in fm:
            de[k] = fm[k]

    # Boolean + string scalars carried in the frontmatter of unified entity
    # org files (these keys come from the table pathway during Step 5 migration).
    for k in ("has_tenant_id", "image_id", "has_artefact_insert_fn"):
        if k in fm:
            de[k] = _parse_typed(fm[k])
    if "coding_scheme" in fm:
        de["coding_scheme"] = fm["coding_scheme"]  # raw; boolean flags computed at render time

    # Prose body before the first heading describes the modelled entity.
    pre_heading_body = _strip_body(doc.root)
    if pre_heading_body:
        de["description"] = pre_heading_body

    # File-level properties at the top of the file (if any) contribute too.
    if doc.root.org_id:
        de["entity_org_id"] = doc.root.org_id

    # Top-level sections.
    flags = _section(doc.root, "Flags")
    if flags:
        for k, v in flags.properties.items():
            de[k.lower()] = _parse_typed(v)
        # Custom: service_find_by_code is represented as a column reference
        # in the org file; reshape into the {"column": ...} dict the JSON
        # model uses.
        if "service_find_by_code_column" in de:
            col = de.pop("service_find_by_code_column")
            de["service_find_by_code"] = {"column": col}

    cols = _section(doc.root, "Columns")
    if cols:
        pk_fields, nk_fields, plain_fields = _split_columns_section(cols)
        if pk_fields:
            de["primary_key"] = _primary_key_dict(pk_fields)
        if nk_fields:
            de["natural_keys"] = nk_fields
        de["columns"] = plain_fields

    sql = _section(doc.root, "SQL")
    if sql:
        de["sql"] = {k.lower(): _parse_typed(v) for k, v in sql.properties.items()}

    repo = _section(doc.root, "Repository")
    if repo:
        de["repository"] = {k.lower(): _parse_typed(v) for k, v in repo.properties.items()}

    # Foreign keys: a domain-level concept (references to another entity's
    # row) shared by the SQL facet (existence-check trigger) and C++ facets
    # (repository/service/protocol/handler list-by-FK queries, opted into
    # per column via :list_by:). Top-level on domain_entity, not nested
    # under any one facet's namespace.
    fk_section = _section(doc.root, "Foreign keys")
    if fk_section and fk_section.children:
        fks = [_soft_fk_validation_node_to_dict(c) for c in fk_section.children]
        for fk in fks:
            # :bump_parent_version: true asks the child's insert trigger and
            # delete rule to call the parent's generated touch function
            # (see the "Temporal composite entity versioning" architecture
            # doc). Derive the function name from the FK's target table
            # rather than requiring it spelled out per FK.
            if fk.get("bump_parent_version"):
                fk["touch_function"] = re.sub(r"_tbl$", "_touch_version_fn", fk["table"])
        de["foreign_keys"] = fks

    # SQL section: SQL-specific flags + structured sub-sections.
    sql_section = _section(doc.root, "SQL")
    if sql_section:
        sql_flags = _section(sql_section, "Flags")
        if sql_flags:
            de["sql"] = {
                k.lower(): _parse_typed(v) for k, v in sql_flags.properties.items()
            }
        checks_section = _section(sql_section, "Checks")
        if checks_section and checks_section.tables:
            rows = _parse_org_table_rows(checks_section)
            expressions = [r["expression"] for r in rows if r.get("expression")]
            if expressions:
                de.setdefault("sql", {})["extra_checks"] = expressions
        delete_sets_section = _section(sql_section, "Delete sets")
        if delete_sets_section and delete_sets_section.tables:
            rows = _parse_org_table_rows(delete_sets_section)
            sets = [r["expression"] for r in rows if r.get("expression")]
            if sets:
                de.setdefault("sql", {})["extra_delete_sets"] = sets
        bitemporal_nk_section = _section(sql_section, "Bitemporal natural keys")
        if bitemporal_nk_section and bitemporal_nk_section.tables:
            rows = _parse_org_table_rows(bitemporal_nk_section)
            de.setdefault("sql", {})["bitemporal_natural_keys"] = [
                {"column": r["column"], "is_nullable": _parse_typed(r.get("nullable", "false"))}
                for r in rows if r.get("column")
            ]
        indexes_section = _section(sql_section, "Indexes")
        if indexes_section and indexes_section.tables:
            rows = _parse_org_table_rows(indexes_section)
            de["indexes"] = [
                {
                    "name": r["name"],
                    "columns": r.get("columns", ""),
                    "unique": _parse_typed(r.get("unique", "false")),
                    "current_only": _parse_typed(r.get("current_only", "false")),
                    "where_extra": r.get("where_extra", "") or None,
                }
                for r in rows if r.get("name")
            ]

    # Artefact indexes: extra indexes on the artefact/staging table
    # (sql_schema_domain_entity_artefact_create.mustache), carried over from
    # the lookup_entity convention this section originates from. A top-level
    # heading (not nested under SQL), one ``** <name>`` child per index with
    # a ``:columns:`` property (raw column-list text, verbatim into the
    # index definition).
    artefact_section = _section(doc.root, "Artefact indexes")
    if artefact_section:
        artefact_indexes: list[dict[str, Any]] = []
        for node in artefact_section.children:
            entry = {"name": node.title}
            for k, v in node.properties.items():
                entry[k.lower()] = v  # keep columns string verbatim
            artefact_indexes.append(entry)
        de["artefact_indexes"] = artefact_indexes

    # Used by the C++ repository facet to conditionally include the
    # datetime header only when at least one FK opts into the as-of
    # window query (:list_by_as_of: true). Computed after the SQL
    # section above so it survives that block's de["sql"] reassignment.
    if any(fk.get("list_by_as_of") for fk in de.get("foreign_keys", [])):
        de.setdefault("sql", {})["has_list_by_as_of"] = True

    # Optional sections carried over from the table pathway; present in unified
    # entity org files after Step 5 content migration.
    vfn_section = _section(doc.root, "Validation function")
    if vfn_section:
        vfn: dict[str, Any] = {}
        for k, v in vfn_section.properties.items():
            key = k.lower()
            if key in ("default", "default_value"):
                vfn[key] = v  # raw string; Mustache 0-falsy guard
            else:
                vfn[key] = _parse_typed(v)
        de["validation_fn"] = vfn

    insert_section = _section(doc.root, "Insert trigger")
    if insert_section:
        validations_section = _section(insert_section, "Validations")
        if validations_section:
            rows = _parse_org_table_rows(validations_section)
            nullable_by_column = {
                c["name"]: c.get("nullable", False) for c in de.get("columns", [])
            }
            for row in rows:
                row["nullable"] = nullable_by_column.get(row.get("column"), False)
            de["insert_trigger"] = {"validations": rows}

    check_section = _section(doc.root, "Check constraints")
    if check_section:
        constraints: list[dict[str, Any]] = []
        for node in check_section.children:
            entry: dict[str, Any] = {}
            for k, v in node.properties.items():
                entry[k.lower()] = v  # keep expression verbatim
            constraints.append(entry)
        de["check_constraints"] = constraints

    idx_section = _section(doc.root, "Indexes")
    if idx_section:
        idxs: list[dict[str, Any]] = []
        for node in idx_section.children:
            idx_entry: dict[str, Any] = {"name": node.title}
            for k, v in node.properties.items():
                idx_entry[k.lower()] = _parse_typed(v)
            idxs.append(idx_entry)
        de["indexes"] = idxs

    # C++ section: everything C++ codegen needs.
    cpp_section = _section(doc.root, "C++")
    if cpp_section:
        # Flags: lift directly onto domain_entity (these are top-level
        # in the JSON model).
        cpp_flags = _section(cpp_section, "Flags")
        if cpp_flags:
            for k, v in cpp_flags.properties.items():
                de[k.lower()] = _parse_typed(v)
            if "service_find_by_code_column" in de:
                col = de.pop("service_find_by_code_column")
                de["service_find_by_code"] = {"column": col}
                # A child entity whose natural key is composite (parent
                # foreign key + code column, e.g. party_contact_information's
                # (party_id, contact_type)) needs the parent column threaded
                # through too, or a bare find_X_by_code(code) would be
                # ambiguous across every parent row sharing that code.
                if "service_find_by_code_parent_column" in de:
                    de["service_find_by_code"]["parent_column"] = de.pop(
                        "service_find_by_code_parent_column")

        # Repository naming conventions.
        repo = _section(cpp_section, "Repository")
        if repo:
            de["repository"] = {
                k.lower(): _parse_typed(v) for k, v in repo.properties.items()
            }

        # cpp.includes + cpp.iterator_var + cpp.table_display
        cpp_out: dict[str, Any] = {}
        dom = _section(cpp_section, "Domain includes")
        ent = _section(cpp_section, "Entity includes")
        if dom or ent:
            cpp_out["includes"] = {
                "domain": _includes_from_named_block(dom) if dom else [],
                "entity": _includes_from_named_block(ent) if ent else [],
            }
        conv = _section(cpp_section, "Conventions")
        if conv:
            for k, v in conv.properties.items():
                cpp_out[k.lower()] = _parse_typed(v)
        td = _section(cpp_section, "Table display")
        if td:
            cpp_out["table_display"] = _table_display(td)
        if cpp_out:
            de["cpp"] = cpp_out

        # Qt UI bindings.
        qt = _section(cpp_section, "Qt")
        if qt:
            de["qt"] = _parse_qt_drawer(
                qt, _profile_namespace_defaults(de.get("profile"), "qt")
            )

        # Custom repository methods (the literate fragment mechanism).
        cm = _section(cpp_section, "Custom repository methods")
        if cm:
            de["custom_repository_methods"] = _custom_methods(cm)

    # Implementations by kind UUID — consumed by the post-render
    # ``<<paste:UUID>>`` substitution pass in the codegen driver.
    impls = _collect_implementations(doc.root)
    if impls:
        de["implementations"] = impls

    # Profile binding: a :profile: property on the root Flags drawer
    # (already lifted into de['profile'] by the generic Flags loop above)
    # supplies feature defaults from the named profile's own Assignments
    # table. Applied last so every explicit value parsed above wins.
    _apply_profile(de)

    return {"domain_entity": de}


def _parse_qt_drawer(
    qt: OrgNode, profile_defaults: dict[str, Any] | None = None
) -> dict[str, Any]:
    """Parse a ``** Qt`` drawer (properties + Detail fields / Columns (Qt
    model) / Icon columns / Setting-gated actions / Related entity
    shortcuts sub-sections) into the ``qt`` sub-dict the ``ores.cpp.qt``
    mustache templates consume. Shared by :func:`org_document_to_model`
    (domain_entity) and :func:`load_org_junction_model` (junction) — the
    drawer shape and every derived flag below is identical for both.

    ``profile_defaults`` (a bound entity's profile's qt-namespace
    Assignments, if any) is seeded in *before* any derivation below runs --
    e.g. ``has_toolbar``'s derivation from ``has_version_navigation`` must
    see the profile-supplied value, not its absence. An explicit drawer
    property still wins over it either way."""
    qt_out: dict[str, Any] = {}
    for k, v in qt.properties.items():
        qt_out[k.lower()] = _parse_typed(v)
    if profile_defaults:
        for k, v in profile_defaults.items():
            qt_out.setdefault(k, v)
    df = _section(qt, "Detail fields")
    if df:
        qt_out["detail_fields"] = _detail_fields(df)
        # static_combo fields declare their fixed option set as a
        # comma-separated :combo_values: string (e.g. "Active,
        # Inactive,Closed") — parsed here into the {label, value}
        # dicts the template iterates. label == value; if a field
        # ever needs them to differ, extend this to accept
        # "label:value" pairs.
        for f in qt_out["detail_fields"]:
            raw = f.get('combo_values')
            if f.get('type') == 'static_combo' and isinstance(raw, str) and raw:
                f['combo_values'] = [
                    {'label': v.strip(), 'value': v.strip()}
                    for v in raw.split(',') if v.strip()
                ]
        # combo_widget_customs/has_combo_badge_source are computed in
        # core.py, not here: they depend on combo_widget_class /
        # badge_key values that core.py's is_flagged_combo /
        # is_static_combo handling defaults in, which runs after
        # this module.
    qc = _section(qt, "Columns (Qt model)")
    if qc:
        qt_out["columns"] = _qt_columns(qc)
        # Preview columns for the generic import dialog (has_csv_xml_io):
        # excludes audit/system fields that are meaningless before a
        # record is imported (its own version/modified_by/recorded_at
        # belong to the never-yet-saved row, not the source file).
        #
        # Opt-in, not opt-out: a column only appears in the import
        # preview if its row in "Columns (Qt model)" sets
        # :import_preview: true explicitly. An exclude-list keyed on
        # generic properties (is_timestamp, audit field names) was
        # tried first and silently over-included fields the entity
        # author never intended to preview (caught in PR #1445
        # review — currency's hand-migrated dialog only shows 5 of
        # its 14 columns, not the ~11 an opt-out list would keep).
        qt_out["import_preview_columns"] = [
            c for c in qt_out["columns"] if c.get("import_preview")
        ]
    ic = _section(qt, "Icon columns (Qt model)")
    if ic:
        qt_out["icon_columns"] = _qt_icon_columns(ic)
        qt_out["has_icon_columns"] = bool(qt_out["icon_columns"])
        # Any pair (roughly 2:1) composited icon needs the view's
        # iconSize widened past Qt's default square box — see
        # currency_pair_icon_size() in FlagIconHelper.hpp — or it
        # renders squished.
        qt_out["has_pair_icon_column"] = any(
            entry.get("is_pair") for entry in qt_out["icon_columns"]
        )
    sga = _section(qt, "Setting-gated actions")
    if sga:
        qt_out["setting_gated_actions"] = _qt_setting_gated_actions(sga)
        qt_out["has_setting_gated_actions"] = bool(qt_out["setting_gated_actions"])
    # Every entity gets a generate_synthetic_<entity> generator (ores.cpp.generator
    # facet) — a detail dialog opts into a "Generate" toolbar button that fills
    # its fields from it by naming the QAction member "generateAction" in the
    # Setting-gated actions table above (member declaration + visibility gating
    # both come from that table already; this only decides whether the click
    # handler and its generator call get generated).
    qt_out["has_generate_action"] = any(
        a.get("action") == "generateAction" for a in qt_out.get("setting_gated_actions", [])
    )
    # A detail dialog needs a QToolBar iff it hosts version-nav
    # actions, the Generate action (both add QAction rows to it),
    # or the author explicitly asked for one (e.g. to host a
    # hand-written paste-block action with no dedicated knob of
    # its own, like calendar's "Regenerate up to <year>") --
    # an explicit :has_toolbar: true in the drawer must survive
    # this derivation, not be silently overwritten by it.
    qt_out["has_toolbar"] = bool(
        qt_out.get("has_toolbar")
        or qt_out.get("has_version_navigation")
        or qt_out["has_generate_action"]
    )
    res = _section(qt, "Related entity shortcuts")
    if res:
        qt_out["related_entity_shortcuts"] = _qt_related_entity_shortcuts(res)
        qt_out["has_related_entity_shortcuts"] = bool(qt_out["related_entity_shortcuts"])
    # has_flag_icon is derived, not a separately-authored property:
    # an entity has the single-column image_id-keyed flag mechanism
    # iff it declared which column shows it. Any manually-set
    # :has_flag_icon: in the .org file is ignored/overwritten here —
    # it was always redundant with :flag_icon_column: being present.
    qt_out["has_flag_icon"] = bool(qt_out.get("flag_icon_column"))
    # has_key_flag_icon gates ONLY the detail dialog's inline
    # leading-icon-on-a-line-edit display (keyFlagField()/
    # keyFlagIcon(), via set_line_edit_flag_icon() —
    # FlagIconHelper.hpp) — a purely derived, read-only decoration
    # requiring no image_id field of its own. Deliberately split
    # from has_flag_icon (the entity's own uploadable flag image:
    # entityImageId(), initFlagButton(), image_id save-back) since
    # an entity can want one without the other — e.g.
    # currency_pair_convention shows a flag derived from its
    # pair_code text field but owns no image_id at all. Defaults
    # key_flag_field/key_flag_accessor from flag_inline_widget/
    # flag_accessor when has_flag_icon is set and neither was given
    # explicitly, so existing has_flag_icon entities (Country,
    # Currency) are unaffected.
    if not qt_out.get("key_flag_field") and qt_out.get("flag_inline_widget"):
        qt_out["key_flag_field"] = qt_out["flag_inline_widget"]
    if not qt_out.get("key_flag_accessor") and qt_out.get("flag_accessor"):
        qt_out["key_flag_accessor"] = qt_out["flag_accessor"]
    qt_out["has_key_flag_icon"] = bool(qt_out.get("key_flag_field"))
    # Any list view showing a flag at all (own image_id-backed
    # column, or a derived icon_columns entry) must set an explicit
    # iconSize rather than rely on Qt's implicit per-style default —
    # see single_flag_icon_size()/currency_pair_icon_size() in
    # FlagIconHelper.hpp: two views relying on the implicit default
    # aren't guaranteed to render the same flag at the same size.
    qt_out["has_any_flag_icon"] = (
        qt_out["has_flag_icon"] or qt_out.get("has_icon_columns", False)
    )
    # Whether an ImageCache reference needs threading through the
    # controller/window/detail-dialog layers at all — true for either
    # icon mechanism. Kept distinct from has_flag_icon (which also
    # still gates the single-column iconColumn()/flagDecoration()
    # code in the client model itself) so that a model using only
    # the newer multi-column mechanism still gets ImageCache wired
    # through everywhere it's needed.
    # Whether any dynamic-combo detail field decorates its items with
    # flag icons (e.g. a currency combo) via FlagIconHelper —
    # gates the include and the ImageCache wiring below.
    qt_out["has_combo_flag_source"] = any(
        f.get("flag_source") for f in qt_out.get("detail_fields", [])
    )
    # Whether any dynamic-combo detail field uses the
    # populateDynamicCombo<Entity> helper (fetch/sort/tooltip/
    # placeholder/restore-selection, piloted on currency's
    # rounding_type/monetary_nature/market_tier combos) — gates
    # the DynamicComboSetup.hpp/LookupFetcher.hpp includes.
    qt_out["has_dynamic_combo_helper_fields"] = any(
        f.get("combo_domain_type") for f in qt_out.get("detail_fields", [])
    )
    qt_out["needs_image_cache"] = (
        qt_out["has_flag_icon"]
        or qt_out["has_key_flag_icon"]
        or qt_out.get("has_icon_columns", False)
        or qt_out["has_combo_flag_source"]
    )
    return qt_out


# --------------------------------------------------------------------------
# Validator


REQUIRED_FLAGS = ("schema", "product", "component", "subcomponent")
REQUIRED_COLUMN_PROPS = ("type",)


def validate_model(model: dict[str, Any]) -> list[str]:
    """Return a list of human-readable validation errors (empty == valid)."""
    errors: list[str] = []
    if "domain_entity" not in model:
        errors.append("Missing top-level 'domain_entity' key")
        return errors
    de = model["domain_entity"]

    for k in REQUIRED_FLAGS:
        if k not in de:
            errors.append(f"Missing required flag: {k}")

    if "primary_key" not in de:
        errors.append(
            "Missing primary key: no field in 'Columns' is flagged "
            ":primary_key: true"
        )
    elif "column" not in de["primary_key"]:
        errors.append("Primary key missing required property: column")

    for col in de.get("columns", []):
        for required in REQUIRED_COLUMN_PROPS:
            if required not in col:
                errors.append(
                    f"Column '{col.get('name','?')}' missing required property: {required}"
                )

    # Non-nullable is_enum columns/natural keys/primary-key fields get no
    # C++ member initializer unless :default_value: is set specifically
    # (cpp_domain_type_class.hpp.mustache only emits `= {{default_value}}`
    # from that exact property -- there is no {{#default}} fallback, so
    # :default: alone does NOT satisfy this despite the two keys being used
    # interchangeably elsewhere in this file for SQL-side vs C++-side
    # defaults) -- a plain `{{{cpp_type}}} {{name}};` leaves a scoped enum
    # member uninitialized on default construction, which is undefined
    # behaviour the moment anything reads it (e.g. rfl::enum_to_string() in
    # a repository mapper). A :name generator: block only feeds the
    # synthetic-data generator, not the struct itself, so it does not
    # substitute for a real default. This exact defect shipped once already
    # (PR #1412, task_fix_market_series_generator_uninitialized_enums) via a
    # hand-written generator-only fix that a later regeneration silently
    # dropped; requiring :default_value: up front closes the whole class of
    # bug rather than one instance of it.
    _enum_check_fields = (
        de.get("columns", [])
        + de.get("natural_keys", [])
        + de.get("primary_key", {}).get("columns", [])
    )
    for col in _enum_check_fields:
        if (
            col.get("is_enum")
            and not col.get("nullable")
            and "default_value" not in col
        ):
            errors.append(
                f"Enum column '{col.get('name') or col.get('column')}' has no "
                ":default_value: -- a non-nullable is_enum column with no default "
                "gets no C++ member initializer and is left uninitialized on "
                "default construction. Add :default_value: <enumerator> (see "
                "ores.marketdata.feed_binding.org's asset_class for the pattern)."
            )

    return errors


# --------------------------------------------------------------------------
# Public entry point


def load_org_model(path: Path | str) -> dict[str, Any]:
    """Load an org-mode entity model into the canonical dict structure.

    Raises ``ValueError`` if the loaded model fails validation, so the
    author sees a structural error up front instead of a cryptic
    template-render failure later."""
    text = Path(path).read_text(encoding="utf-8")
    doc = parse_org(text)
    model = org_document_to_model(doc)
    errors = validate_model(model)
    if errors:
        raise ValueError(
            f"Validation errors in {path}:\n  " + "\n  ".join(errors)
        )
    return model


def _fk_side_from_section(node: OrgNode) -> dict[str, Any]:
    """Read a junction ``* Left`` / ``* Right`` sub-section into a dict
    matching the JSON-side shape (column, type, cpp_type, ..., plus
    description / detail / generator_expr from the body)."""
    out: dict[str, Any] = {}
    for k, v in node.properties.items():
        out[k.lower()] = _parse_typed(v)
    description, detail = _description_and_detail(node)
    if description:
        out["description"] = description
    if detail:
        out["detail"] = detail
    if "generator" in node.src_blocks:
        out["generator_expr"] = node.src_blocks["generator"]
    return out


def load_org_junction_model(path: Path | str) -> dict[str, Any]:
    """Load an org-mode junction model into the ``{junction: {...}}`` dict
    shape that ``sql_schema_junction_create.mustache`` consumes."""
    text = Path(path).read_text(encoding="utf-8")
    doc = parse_org(text)
    _ensure_profile_binding(doc)
    fm = doc.frontmatter

    j: dict[str, Any] = {}
    for key in ("product", "schema", "component", "name", "name_singular",
                "name_title", "name_singular_words", "brief"):
        if key in fm:
            j[key] = fm[key]
    if "has_tenant_id" in fm:
        j["has_tenant_id"] = _parse_typed(fm["has_tenant_id"])
    # Top-level * Flags section -- the single canonical binding point for
    # :profile: (and any other root-level flags), mirroring
    # org_document_to_model's domain_entity handling.
    flags = _section(doc.root, "Flags")
    if flags:
        for k, v in flags.properties.items():
            j[k.lower()] = _parse_typed(v)

    body = _strip_body(doc.root)
    if body:
        j["description"] = body

    left = _section(doc.root, "Left")
    if left:
        j["left"] = _fk_side_from_section(left)
    right = _section(doc.root, "Right")
    if right:
        j["right"] = _fk_side_from_section(right)

    cols_section = _section(doc.root, "Columns")
    columns: list[dict[str, Any]] = []
    if cols_section:
        for node in cols_section.children:
            columns.append(_column_node_to_dict(node))
    j["columns"] = columns

    # Soft-FK validations, parsed identically to a domain_entity's
    # (see _soft_fk_validation_node_to_dict): a `* Foreign keys` section
    # with one `** <column>` child per referenced table. Absent (the case
    # for every other junction model today), the key is omitted and the
    # sql_schema_junction_create template emits no validation block.
    fk_section = _section(doc.root, "Foreign keys")
    if fk_section and fk_section.children:
        j["foreign_keys"] = [
            _soft_fk_validation_node_to_dict(c) for c in fk_section.children
        ]

    sql_section = _section(doc.root, "SQL")
    if sql_section:
        sql_flags = _section(sql_section, "Flags")
        if sql_flags:
            j["sql"] = {
                k.lower(): _parse_typed(v) for k, v in sql_flags.properties.items()
            }

    repo = _section(doc.root, "Repository")
    if repo:
        j["repository"] = {
            k.lower(): _parse_typed(v) for k, v in repo.properties.items()
        }

    cpp_section = _section(doc.root, "C++")
    cpp_out: dict[str, Any] = {}
    if cpp_section:
        # Flags: lift directly onto junction (these are top-level in the
        # JSON model), matching load_org_model()'s domain_entity Flags
        # handling -- e.g. :subcomponent: for resolve_output_path()'s
        # component_dir/component_include/... derivation.
        flags = _section(cpp_section, "Flags")
        if flags:
            for k, v in flags.properties.items():
                j[k.lower()] = _parse_typed(v)
        dom = _section(cpp_section, "Domain includes")
        ent = _section(cpp_section, "Entity includes")
        if dom or ent:
            cpp_out["includes"] = {
                "domain": _includes_from_named_block(dom) if dom else [],
                "entity": _includes_from_named_block(ent) if ent else [],
            }
        conv = _section(cpp_section, "Conventions")
        if conv:
            for k, v in conv.properties.items():
                cpp_out[k.lower()] = _parse_typed(v)
        td = _section(cpp_section, "Table display")
        if td:
            cpp_out["table_display"] = _table_display(td)

        # Qt UI bindings, parsed identically to a domain_entity's ** Qt
        # drawer (see _parse_qt_drawer) -- but a junction's own fields
        # (name_singular/name/... , repository.name_short/...) use
        # different key names than domain_entity's (entity_singular/
        # entity_plural/..., repository.entity_plural_short/...), so the
        # ores.cpp.qt templates (written once, against the domain_entity
        # shape only) need those keys aliased onto j directly. Only done
        # when a Qt drawer is actually present -- a junction with no Qt
        # facet stays exactly as before.
        qt_section = _section(cpp_section, "Qt")
        if qt_section:
            j["qt"] = _parse_qt_drawer(
                qt_section, _profile_namespace_defaults(j.get("profile"), "qt")
            )
            name_singular = j.get("name_singular", "unknown")
            words = name_singular.split("_")
            j["entity_singular"] = name_singular
            j["entity_plural"] = j.get("name", name_singular + "s")
            j["entity_title"] = j.get("name_title")
            j["entity_singular_words"] = j.get("name_singular_words") or (
                words[-1] if words else name_singular)
            j["entity_snake"] = name_singular
            j["entity_upper"] = name_singular.upper()
            j["entity_pascal"] = "".join(w.capitalize() for w in words)
            j["entity_pascal_short"] = (
                words[-1].capitalize() if words else name_singular.capitalize())
            plural_words = j["entity_plural"].split("_")
            j["entity_pascal_short_plural"] = (
                plural_words[-1].capitalize() if plural_words
                else j["entity_plural"].capitalize())
            j["entity_plural_words"] = plural_words[-1] if plural_words else j["entity_plural"]
            # repository.entity_plural_short is what client_model_impl's
            # generated get-response field access uses (result->{{...}}) --
            # aliased from the junction Repository drawer's own
            # name_short/name_singular_short naming.
            repo = j.get("repository") or {}
            if repo:
                # Merge, not replace -- repo also carries name_short/
                # name_singular_short/name_words/order_column, which
                # core.py's repository-field hoist (below) copies onto
                # the junction's top level for the non-Qt repository/
                # service/nats-handler templates to consume directly
                # (e.g. order_by("{{order_column}}"_c)). Replacing the
                # dict wholesale would silently corrupt those facets the
                # next time they're regenerated.
                repo.update({
                    "entity_singular_short": repo.get("name_singular_short", name_singular),
                    "entity_plural_short": repo.get("name_short", j["entity_plural"]),
                    "entity_singular_words": repo.get(
                        "name_singular_words", j["entity_singular_words"]),
                    "entity_plural_words": repo.get(
                        "name_words", j["entity_plural_words"]),
                })
    if cpp_out:
        j["cpp"] = cpp_out

    # Implementations by kind UUID -- consumed by the post-render
    # ``<<paste:UUID>>`` substitution pass in the codegen driver, mirroring
    # domain_entity's equivalent collection in org_document_to_model().
    impls = _collect_implementations(doc.root)
    if impls:
        j["implementations"] = impls

    # Profile binding, mirroring org_document_to_model()'s domain_entity
    # handling: a #+profile: frontmatter line resolves against the named
    # profile's own Assignments table as feature defaults.
    _apply_profile(j)

    return {"junction": j}


def load_org_field_group_model(path: Path | str) -> dict[str, Any]:
    """Load an org-mode field-group model into the JSON-equivalent dict.

    Produces a ``{"field_group": {...}}`` dict matching the shape
    consumed by ``cpp_field_group.hpp.mustache``: frontmatter
    scalars (component, component_include, entity_singular, brief,
    description, ...), a ``cpp`` sub-dict with ``namespace`` +
    ``includes`` (parsed from the named ``includes`` babel block under
    ``* Includes``), and a ``fields`` list (parsed from ``** <name>``
    sub-headings under ``* Fields``)."""
    text = Path(path).read_text(encoding="utf-8")
    doc = parse_org(text)
    fm = doc.frontmatter

    fg: dict[str, Any] = {}
    for key in ("product", "component", "subcomponent",
                "entity_singular", "brief"):
        if key in fm:
            fg[key] = fm[key]

    body = _strip_body(doc.root)
    if body:
        fg["description"] = body

    cpp: dict[str, Any] = {}
    if "namespace" in fm:
        cpp["namespace"] = fm["namespace"]
    inc = _section(doc.root, "Includes")
    if inc:
        cpp["includes"] = _includes_from_named_block(inc)
    if cpp:
        fg["cpp"] = cpp

    fields_section = _section(doc.root, "Fields")
    fields: list[dict[str, Any]] = []
    if fields_section:
        for node in fields_section.children:
            entry: dict[str, Any] = {"name": node.title}
            for k, v in node.properties.items():
                key = k.lower()
                # Mustache treats numeric 0 as falsy, so {{#default_value}}
                # would skip a "0" initializer. Keep default_value as the
                # raw string (the template emits it verbatim via {{{...}}}).
                if key == "default_value":
                    entry[key] = v
                else:
                    entry[key] = _parse_typed(v)
            description = _strip_body(node)
            if description:
                entry["description"] = description
            fields.append(entry)
    fg["fields"] = fields

    return {"field_group": fg}


def _parse_org_table_rows(node: OrgNode) -> list[dict[str, str]]:
    """Find the first org table on ``node`` or any descendant and return
    its rows. ``parse_org`` pre-parses tables into list-of-dict form, so
    nothing extra is needed here."""
    if node.tables:
        return list(node.tables[0])
    for child in node.children:
        nested = _parse_org_table_rows(child)
        if nested:
            return nested
    return []


def _sql_column_from(src: dict[str, Any], name_key: str) -> dict[str, Any]:
    """Project a column- or natural-key dict onto the minimal shape the SQL
    schema template consumes: ``name``, ``type``, ``nullable``, ``default``.

    The natural-key/column split is a C++/domain concern; for the physical
    schema both are just data columns, so this is where the two converge onto
    one field-naming convention (``name``). ``default`` is forced to a string
    because Mustache treats a numeric ``0`` as falsy and would silently drop a
    ``default 0`` clause."""
    col: dict[str, Any] = {"name": src[name_key], "type": src["type"]}
    if "nullable" in src:
        col["nullable"] = src["nullable"]
    if "default" in src:
        col["default"] = str(src["default"])
    return col


def domain_entity_to_table_context(de: dict[str, Any]) -> dict[str, Any]:
    """Project a unified ``domain_entity`` model onto the ``{table: {...}}``
    render context consumed by ``sql_schema_create.mustache``.

    This is the single canonical entity→SQL projection: the entity pathway and
    the (legacy) table pathway both feed the *same* schema template, so an
    entity regenerates byte-identically to its retired ``*_table.org``. Derived
    boolean flags (``has_coding_scheme``, validation-function scope, check
    constraints, last-item markers) are *not* computed here — they are applied
    uniformly by ``normalise_sql_table_context`` in the generator, shared with
    the table pathway."""
    t: dict[str, Any] = {}
    for key in ("product", "schema", "component",
                "entity_singular", "entity_plural", "description"):
        if key in de:
            t[key] = de[key]
    t["coding_scheme"] = de.get("coding_scheme", "none")
    t["has_tenant_id"] = bool(de.get("has_tenant_id", True))
    if "image_id" in de:
        t["image_id"] = de["image_id"]

    pk = dict(de.get("primary_key", {}))
    # The physical schema needs to know whether the key is text (drives the
    # non-empty CHECK and quoting). Entity models express it as the column
    # type; derive the flag the schema template expects.
    if "is_text" not in pk and "type" in pk:
        pk["is_text"] = pk["type"] == "text"
    t["primary_key"] = pk

    # Physical column order: secondary natural keys first, then plain columns —
    # mirroring how the table pathway laid them out under a single * Columns.
    t["columns"] = (
        [_sql_column_from(nk, "column") for nk in de.get("natural_keys", [])]
        + [_sql_column_from(c, "name") for c in de.get("columns", [])]
    )

    if "validation_fn" in de:
        t["validation_fn"] = dict(de["validation_fn"])
    if "insert_trigger" in de:
        t["insert_trigger"] = de["insert_trigger"]
    t["check_constraints"] = de.get("check_constraints", [])
    t["indexes"] = de.get("indexes", [])

    return {"table": t}


# Scalars that ``load_org_lookup_entity_model`` lifts from frontmatter as
# raw strings (preserve every entity_* / component_* string verbatim).
_LOOKUP_ENTITY_STR_SCALARS = (
    "product", "schema", "component", "subcomponent",
    "entity_singular", "entity_singular_upper", "entity_singular_short",
    "entity_singular_words",
    "entity_plural", "entity_plural_short",
    "entity_plural_words", "entity_plural_words_cap",
    "entity_title", "entity_title_lower",
)
# Scalars that should round-trip as booleans (Mustache treats truthy/falsy).
_LOOKUP_ENTITY_BOOL_SCALARS = (
    "has_tenant_id", "system_tenant_validation", "has_display_order",
    "has_coding_scheme", "has_image_id", "has_artefact_insert_fn",
)


def load_org_lookup_entity_model(path: Path | str) -> dict[str, Any]:
    """Load an org-mode lookup-entity model into the ``{entity: {...}}``
    dict shape that ``sql_schema_table_create.mustache`` consumes via the
    ``--address ores.sql.schema`` model_types=["schema"] route.

    Lookup entities share the bi-temporal DDL shape with table models
    but route through codegen's "schema" model_type (JSON root key
    ``entity``). The org file preserves all C++/Qt/protocol scalar
    metadata (``entity_singular_upper``, ``entity_title``,
    ``component_include``, ...) in the frontmatter so future profiles
    don't need the JSON re-introduced."""
    text = Path(path).read_text(encoding="utf-8")
    doc = parse_org(text)
    fm = doc.frontmatter

    e: dict[str, Any] = {}
    for key in _LOOKUP_ENTITY_STR_SCALARS:
        if key in fm:
            e[key] = fm[key]
    for key in _LOOKUP_ENTITY_BOOL_SCALARS:
        if key in fm:
            e[key] = _parse_typed(fm[key])
    e.setdefault("subcomponent", "api")

    body = _strip_body(doc.root)
    if body:
        e["description"] = body

    # Lookup entities declare their single-column key via a dedicated
    # ``* Primary key`` heading (:column:/:type:/:is_text: on the heading
    # itself), not a ``** <name>`` sub-heading with :primary_key: true
    # under ``* Columns`` (the domain_entity convention _split_columns_section
    # handles). Every lookup_entity model in the repo uses this heading;
    # without reading it, entity.primary_key.column renders empty in
    # sql_schema_table_create.mustache and silently corrupts the DDL.
    pk_section = _section(doc.root, "Primary key")
    if pk_section:
        pk: dict[str, Any] = {"column": pk_section.properties.get("column", "")}
        if "type" in pk_section.properties:
            pk["type"] = pk_section.properties["type"]
        if "is_text" in pk_section.properties:
            pk["is_text"] = _parse_typed(pk_section.properties["is_text"])
        e["primary_key"] = pk

    cols_section = _section(doc.root, "Columns")
    if cols_section:
        pk_fields, nk_fields, plain_fields = _split_columns_section(cols_section)
        if pk_fields and "primary_key" not in e:
            e["primary_key"] = _primary_key_dict(pk_fields)
        if nk_fields:
            e["natural_keys"] = nk_fields
        e["columns"] = plain_fields
    else:
        e["columns"] = []

    validations_section = _section(doc.root, "Validations")
    if validations_section:
        e["validations"] = _parse_org_table_rows(validations_section)

    indexes_section = _section(doc.root, "Indexes")
    if indexes_section:
        indexes: list[dict[str, Any]] = []
        for node in indexes_section.children:
            entry = {"name": node.title}
            for k, v in node.properties.items():
                entry[k.lower()] = _parse_typed(v)
            indexes.append(entry)
        e["indexes"] = indexes
    else:
        e["indexes"] = []

    artefact_section = _section(doc.root, "Artefact indexes")
    if artefact_section:
        artefact_indexes: list[dict[str, Any]] = []
        for node in artefact_section.children:
            entry = {"name": node.title}
            for k, v in node.properties.items():
                entry[k.lower()] = v  # keep columns string verbatim
            artefact_indexes.append(entry)
        e["artefact_indexes"] = artefact_indexes

    return {"entity": e}


_SERVICE_REGISTRY_SCALARS = (
    "psql_var", "env_key", "iam_role", "description", "role", "email",
)


def _service_registry_prefix_bullets(node: OrgNode, section_title: str) -> list[dict[str, str]]:
    """Read bullets under a ``** <section_title>`` sub-heading and
    rewrap each as ``{"prefix": <bullet>}`` to match the JSON shape."""
    sub = _section(node, section_title)
    if not sub:
        return []
    out: list[dict[str, str]] = []
    for group in sub.bullet_lists:
        for line in group:
            out.append({"prefix": line.strip()})
    return out


def _service_registry_select_tables(node: OrgNode) -> list[dict[str, str]]:
    """Read bullets under ``** Select tables`` as ``- k1=v1, k2=v2``
    pairs and reconstruct each entry as a dict. Mirrors the converter's
    ``_select_table_bullet``: comma-joined ``key=value`` pairs round-trip
    back into the original dict shape so a future select_tables entry
    won't be flattened to a single-key ``{"prefix": ...}``."""
    sub = _section(node, "Select tables")
    if not sub:
        return []
    out: list[dict[str, str]] = []
    for group in sub.bullet_lists:
        for line in group:
            item: dict[str, str] = {}
            for part in line.split(","):
                if "=" in part:
                    k, v = part.split("=", 1)
                    item[k.strip()] = v.strip()
            if item:
                out.append(item)
    return out


def _service_registry_extra_args(node: OrgNode) -> list[str]:
    """Read bullets under ``** Extra args`` as a flat list of genuinely
    per-service CLI flags, one bullet per flag."""
    sub = _section(node, "Extra args")
    if not sub:
        return []
    out: list[str] = []
    for group in sub.bullet_lists:
        out.extend(line.strip() for line in group)
    return out


def load_org_service_registry_model(path: Path | str) -> dict[str, Any]:
    """Load the org-mode service-registry model into the
    ``{service_registry: {services: [...]}}`` dict.

    Each top-level ``* <service name>`` heading is one fleet process,
    keyed by its full binary name (e.g. ``ores.iam.service``), with two
    independent optional aspects:

    - *DB access* (only for services with their own NATS-domain-service
      role): the ``:psql_var:``, ``:env_key:``, ``:iam_role:``,
      optional ``:role:``, ``:email:`` scalars, plus ``** DML prefixes``
      / ``** Select tables`` / ``** Select prefixes`` /
      ``** Execute prefixes`` sub-headings.
    - *Deployment* (every entry): ``:replicas:`` (int), ``:enabled:``
      (bool), optional ``:depends_on:`` (comma-separated service
      names), plus an optional ``** Extra args`` sub-heading carrying
      genuinely per-service CLI flags as one bullet per flag. Consumed
      by ``compass systemd generate`` to render one concrete unit per
      (service, environment); deliberately platform-agnostic so a
      future Windows-service or macOS-launchd generator can consume
      the same model.

    ``:description:`` is shared by both aspects (a human-readable
    label/blurb, not aspect-specific)."""
    text = Path(path).read_text(encoding="utf-8")
    doc = parse_org(text)

    services: list[dict[str, Any]] = []
    for node in doc.root.children:
        # Defensive: skip top-level headings that aren't service entries
        # (e.g. a future "Notes" or "References" section in the doc).
        # Every real entry carries :replicas: (the deployment aspect is
        # mandatory; the DB-access aspect, keyed off :psql_var:, is not).
        if "replicas" not in node.properties:
            continue
        # Match the JSON's stable key order (name → scalars in
        # _SERVICE_REGISTRY_SCALARS order → lists).
        ordered: dict[str, Any] = {"name": node.title}
        for k in _SERVICE_REGISTRY_SCALARS:
            if k in node.properties:
                ordered[k] = node.properties[k]
        ordered["dml_prefixes"] = _service_registry_prefix_bullets(node, "DML prefixes")
        ordered["select_tables"] = _service_registry_select_tables(node)
        ordered["select_prefixes"] = _service_registry_prefix_bullets(node, "Select prefixes")
        ordered["execute_prefixes"] = _service_registry_prefix_bullets(node, "Execute prefixes")
        ordered["replicas"] = int(node.properties["replicas"])
        ordered["enabled"] = node.properties.get("enabled", "true").strip().lower() == "true"
        ordered["depends_on"] = [
            d.strip() for d in node.properties.get("depends_on", "").split(",") if d.strip()
        ]
        ordered["extra_args"] = _service_registry_extra_args(node)
        services.append(ordered)

    return {"service_registry": {"services": services}}


def load_org_dataset_model(path: Path | str) -> dict[str, Any]:
    """Load a promoted ``dataset_overview.org`` into the ``{dataset: {...}}``
    dict that drives data-scope (populate/seed) generation.

    The dataset model carries no payload of its own: it declares the
    dataset's ``name`` (the on-disk output directory under
    ``projects/ores.sql/populate/``) and ``prefix`` (the output-filename
    stem), and — via its file-level ``:PROPERTIES:`` drawer — opts the
    default-off ``ores.sql.populate`` facet in. The actual payloads are the
    sibling JSON files referenced by each archetype's ``#+data_source:``;
    ``_generate_single`` feeds those through the legacy per-file enrichment.

    ``prefix`` defaults to ``name`` when omitted, mirroring
    ``resolve_output_path``'s ``dataset`` branch."""
    text = Path(path).read_text(encoding="utf-8")
    doc = parse_org(text)
    fm = doc.frontmatter

    name = fm.get("name", "unknown")
    d: dict[str, Any] = {
        "name": name,
        "prefix": fm.get("prefix", name),
    }
    return {"dataset": d}


def load_org_component_model(path: Path | str) -> dict[str, Any]:
    """Load an org-mode component model into the
    ``{component: {name, full_name, brief, description}}`` dict
    consumed by the ``component`` profile (CMake scaffold + C++ stubs).

    The three short scalars sit in frontmatter; the free-form
    description is the document body."""
    text = Path(path).read_text(encoding="utf-8")
    doc = parse_org(text)
    fm = doc.frontmatter

    c: dict[str, Any] = {}
    for k in ("name", "full_name", "brief"):
        if k in fm:
            c[k] = fm[k]
    body = _strip_body(doc.root)
    if body:
        c["description"] = body

    return {"component": c}


# Fields whose lines are emitted by the header templates' guard slots
# (has_quote_type/has_point/has_vol) rather than the verbatim field loop.
_ORESMD_GENERATED_FIELDS = ("quote_type", "point", "vol")

# Hand-crafted parse-time case mapping: entity fields and ccy are upper-
# cased, tenor/point lower-cased, everything else passes through raw.
_ORESMD_UPPER_FIELDS = {"pair", "ccy", "ticker", "reference_entity",
                        "commodity_code", "index_code", "factor_pair"}
_ORESMD_LOWER_FIELDS = {"tenor", "point"}


def load_org_oresmd_quote_type_model(path: Path | str) -> dict[str, Any]:
    """Load an oresmd quote-type org model into a dict.

    When the file is named ``model.org`` (the manifest), scans the directory
    for all sibling ``*_quote_type.org`` files, loads each one, and returns
    them as a combined ``{"oresmd_quote_types": [...]}`` dict — this is the
    batch entry point that drives codegen for all asset classes at once.

    For individual spec files, returns ``{"oresmd_quote_type": {...}}`` with
    the single asset class's data.
    """
    p = Path(path)

    # --- Batch manifest (model.org) ---
    if p.name == "model.org":
        # The manifest's own "* Spec files" table declares the generation
        # order (e.g. ir first, matching the hand-crafted enum file) --
        # NOT alphabetical glob order. Spec files that exist on disk but
        # are not listed are appended afterwards, still deterministic.
        text = p.read_text(encoding="utf-8")
        doc = parse_org(text)
        specs: list[dict[str, Any]] = []
        seen: set[str] = set()
        table = _section(doc.root, "Spec files")
        if table:
            for row in _parse_org_table_rows(table):
                fname = row.get("file")
                if not fname or fname in seen:
                    continue
                spec = _load_single_oresmd_spec(p.parent / fname)
                if spec:
                    # variant_order drives the header templates' struct/variant
                    # ordering (fx-first), which differs from the enum
                    # generation order above; parse_order drives the
                    # parse_*()/resolve_*() definition order.
                    vo = row.get("variant_order")
                    if vo:
                        spec["variant_order"] = int(vo)
                    po = row.get("parse_order")
                    if po:
                        spec["parse_order"] = int(po)
                    specs.append(spec)
                    seen.add(fname)
        for sibling in sorted(p.parent.glob("*_quote_type.org")):
            if sibling.name in seen:
                continue
            spec = _load_single_oresmd_spec(sibling)
            if spec:
                specs.append(spec)
        return {"oresmd_quote_types": specs}

    # --- Single spec file ---
    spec = _load_single_oresmd_spec(p)
    if spec:
        return {"oresmd_quote_type": spec}
    return {}


def _load_single_oresmd_spec(path: Path) -> dict[str, Any] | None:
    """Load a single oresmd quote-type spec org file."""
    text = path.read_text(encoding="utf-8")
    doc  = parse_org(text)
    fm   = doc.frontmatter

    asset_class = fm.get("asset_class", "")
    if not asset_class:
        return None

    result: dict[str, Any] = {}
    result["asset_class"] = asset_class
    result["authority"]   = fm.get("authority", "")
    result["component"]   = fm.get("component", "ores.marketdata")

    # --- Parser template directives ---
    # validate: how the parser rejects an asset class's disallowed query
    # keys -- function (own validate_<ac>() with explicit reject calls, and
    # any type-gated checks), delegate_function (own validate_<ac>() that
    # delegates to validate_no_ir_only_keys()), inline_delegate (inline
    # validate_no_ir_only_keys() call inside parse_<ac>()), or inline
    # (explicit reject calls inside parse_<ac>(), with the rejects emitted
    # from the Reject keys table). quote_type_checked: whether parse_<ac>()
    # enforces "quote only meaningful when type=quote" (false for credit,
    # whose hand-crafted parser has no such check).
    result["validate"] = fm.get("validate", "")
    result["quote_type_checked"] = fm.get("quote_type_checked", "true") != "false"

    # --- Quote types table ---
    qt_section = _section(doc.root, "Quote types")
    qts: list[dict[str, Any]] = []
    if qt_section:
        for row in _parse_org_table_rows(qt_section):
            qts.append({k: v for k, v in row.items()})
    result["quote_types"] = qts

    # --- Enum brief: the class enum's doc comment, verbatim (line breaks
    # are significant -- the generated file must match the hand-crafted
    # text exactly; clang-format does not reflow comment prose). Each
    # continuation line is given the doc-comment " * " prefix here so the
    # template can emit the brief inline after "@brief ".
    brief_section = _section(doc.root, "Enum brief")
    if brief_section:
        brief = _strip_body(brief_section)
        if brief:
            result["enum_brief"] = "\n * ".join(brief.splitlines())

    # --- Enum footer: optional comment lines rendered inside the enum
    # body, after the last enumerator (e.g. credit's "rating descoped"
    # note). Indented to the enum-body depth by the loader so the spec
    # section can hold plain text.
    footer_section = _section(doc.root, "Enum footer")
    if footer_section:
        footer = _strip_body(footer_section)
        if footer:
            result["enum_footer"] = "\n".join(
                "    " + line for line in footer.splitlines()
            )

    # --- Fields table ---
    # Each row is enriched for the templates: kind (string vs enum) and the
    # unqualified enum type name; the generated rows (quote_type/point/vol)
    # are marked and mirrored as has_* guards so the header templates can
    # skip them in the verbatim loop and emit them in their guard slots;
    # parse_transform captures the hand-crafted case mapping (entity fields
    # and ccy are upper-cased, tenor/point lower-cased, enums untyped); the
    # first mandatory string field is the URI entity segment.
    fields_section = _section(doc.root, "Fields")
    fields: list[dict[str, Any]] = []
    if fields_section:
        for row in _parse_org_table_rows(fields_section):
            field: dict[str, Any] = {k: v for k, v in row.items()}
            name = field.get("name", "")
            field["kind"] = "string" if "string" in field.get("cpp_type", "") else "enum"
            # The requirement structs wrap every field in std::optional --
            # including the mandatory ones the identifiers leave plain -- so
            # the requirement template renders the inner type and re-wraps;
            # mandatory cells (std::string, instrument_type) pass through
            # unchanged.
            field["cpp_type_inner"] = re.sub(r"^std::optional<", "", field["cpp_type"]).removesuffix(">")
            if name in _ORESMD_GENERATED_FIELDS:
                field["generated"] = True
                result[f"has_{name}"] = True
            if name == "quote_type":
                result["quote_type_cpp"] = field["cpp_type"]
                result["quote_type_cpp_inner"] = field["cpp_type_inner"]
            if field["kind"] == "enum":
                # std::optional<domain::ir_quote_type> -> ir_quote_type
                enum_type = re.sub(r"^std::optional<", "", field["cpp_type"])
                enum_type = enum_type.replace("domain::", "").removesuffix(">")
                field["enum_type"] = enum_type
            if name in _ORESMD_UPPER_FIELDS:
                field["parse_transform"] = "upper"
            elif name in _ORESMD_LOWER_FIELDS:
                field["parse_transform"] = "lower"
            else:
                field["parse_transform"] = "none"
            # The resolver template picks a fill strategy per field:
            # mandatory string (pair/ccy/...) -> pick_mandatory_string,
            # defaulted (type) -> pick, everything optional -> pick_optional.
            field["is_mandatory_string"] = (
                field.get("kind") == "string"
                and field.get("mandatory") == "yes"
                and not field.get("default")
            )
            # vol is derived from `point` during parsing, never resolved.
            if name == "vol":
                field["resolver_skip"] = True
            fields.append(field)
        for field in fields:
            if field["kind"] == "string" and field.get("mandatory") == "yes":
                field["is_entity_field"] = True
                result["entity_field"] = field["name"]
                break
        for field in fields:
            if (field.get("name") == "ccy" and field.get("mandatory") == "yes"
                    and not field.get("is_entity_field")):
                result["requires_ccy"] = True
    result["fields"] = fields

    # --- Per-struct doc comments: identifier/requirement briefs, in the
    # same " * " continuation scheme as the enum brief (the header
    # templates render them inside /** ... */ blocks; line breaks are
    # significant -- clang-format does not reflow comment prose). Blank
    # lines become bare " *" lines; the requirement template needs the
    # multiline flag because the hand-crafted file uses the single-line
    # "/** @brief ... */" form for every requirement except fx's. ---
    for key, title in (("identifier_brief", "Identifier brief"),
                       ("requirement_brief", "Requirement brief")):
        brief_section = _section(doc.root, title)
        if brief_section:
            brief = _strip_body(brief_section, per_line=True)
            if brief:
                lines = brief.splitlines()
                continuation = [lines[0]]
                continuation.extend(
                    " *" if not line.strip() else f" * {line}"
                    for line in lines[1:]
                )
                result[key] = "\n".join(continuation)
                if key == "requirement_brief" and len(lines) > 1:
                    result["requirement_brief_multiline"] = True

    # --- Index family (ir only): the shared benchmark-family enum values.
    # Emitted as dicts so core's _mark_last_item() (dict-only) can flag the
    # final value and the template can omit its trailing comma. ---
    if_section = _section(doc.root, "Index family")
    if if_section:
        result["index_family"] = [
            {"value": v}
            for v in (r.get("value", "") for r in _parse_org_table_rows(if_section))
            if v
        ]

    # --- Reject keys table ---
    reject_section = _section(doc.root, "Reject keys")
    reject_keys: list[str] = []
    if reject_section:
        for row in _parse_org_table_rows(reject_section):
            if "key" in row:
                reject_keys.append(row["key"])
    result["reject_keys"] = reject_keys

    # --- Type-gated keys (ir only): query keys that are only meaningful
    # when type=quote; the parser's validate_<ac>() emits one check per key
    # ("'<key>' is only meaningful when type=quote"). ---
    tgs_section = _section(doc.root, "Type-gated keys")
    if tgs_section:
        result["type_gated_keys"] = [
            r.get("key", "") for r in _parse_org_table_rows(tgs_section) if r.get("key")
        ]

    # --- URI order: the to_uri() serialization order of the query keys.
    # Defaults to [ccy] + [type] + the remaining query-key fields in table
    # order; the ir spec overrides with an explicit "* URI order" section
    # because its `type` key serializes after `role` in the hand-crafted
    # to_uri branch, which no field-table ordering can express. Each entry
    # resolves the emission form: type (always appended), ccy (mandatory,
    # lowercased), or the owning field's kind (enum -> append_enum_if,
    # string -> append_if). ---
    uri_section = _section(doc.root, "URI order")
    if uri_section:
        uri_keys = [
            r.get("key", "") for r in _parse_org_table_rows(uri_section) if r.get("key")
        ]
    else:
        uri_keys = []
        if result.get("requires_ccy"):
            uri_keys.append("ccy")
        uri_keys.append("type")
        uri_keys.extend(
            f["query_key"] for f in fields
            if f.get("query_key") and f["query_key"] not in ("ccy", "type")
        )
    uri_order: list[dict[str, Any]] = []
    for k in uri_keys:
        if k == "type":
            entry: dict[str, Any] = {"key": k, "kind": "type"}
        elif k == "ccy":
            entry = {"key": k, "kind": "ccy"}
        else:
            f = next((x for x in fields if x.get("query_key") == k), None)
            if not f:
                continue
            entry = {"key": k, "kind": f["kind"], "name": f["name"]}
        # Boolean kind marker so the template can branch on the emission
        # form ({{#is_type}}, {{#is_enum}}, ...) -- mustache sections test
        # for key presence, and the kind value ("type", "enum", ...) is
        # not itself a key of the entry.
        entry[f"is_{entry['kind']}"] = True
        uri_order.append(entry)
    result["uri_order"] = uri_order

    # --- Test cases ---
    tests: dict[str, list[dict[str, str]]] = {}
    tc_section = _section(doc.root, "Test cases")
    if tc_section:
        for child in tc_section.children:
            # Subsection titles become list keys, e.g. "Projections" ->
            # "projections", "Round-trip (extended)" -> "round_trip_extended";
            # core.py then names the per-class lists from these keys.
            kind = re.sub(r"[^a-z0-9]+", "_", child.title.lower()).strip("_")
            cases: list[dict[str, str]] = []
            for row in _parse_org_table_rows(child):
                row = {k: v for k, v in row.items()}
                # Projection rows with expected=nullopt are negative tests
                # (the projection returns no key); the templates branch on
                # this flag to emit REQUIRE_FALSE instead of the equality
                # check.
                if row.get("expected") == "nullopt":
                    row["nullopt"] = True
                cases.append(row)
            tests[kind] = cases
    result["test_cases"] = tests

    return result


def load_org_component_overview_model(path: Path | str) -> dict[str, Any]:
    """Load a ``component_overview.org`` into the
    ``{component: {name, full_name, brief, description}}`` dict
    consumed by the ``component`` profile.

    Differs from ``load_org_component_model``: all four scalars sit
    in frontmatter (``#+name:``, ``#+full_name:``, ``#+brief:``,
    ``#+description:``). The literate body of the overview (Diagram,
    Summary, Inputs, ...) is for humans and Emacs navigation — codegen
    keys off the frontmatter ``#+description:`` as the JSON
    ``description`` equivalent.

    Replaces the standalone ``*_component.org`` shape with a single
    source of truth per component."""
    text = Path(path).read_text(encoding="utf-8")
    doc = parse_org(text)
    fm = doc.frontmatter

    c: dict[str, Any] = {}
    for k in ("name", "full_name", "brief", "description"):
        if k in fm:
            c[k] = fm[k]
    # Component kind (flat | api | core | service | composite) selects the
    # scaffolding variant set via the graph's kind discriminator; defaults
    # to "flat". "composite" is a pure aggregator with sub-components of
    # its own and no code -- no archetype in ores.cpp.component/
    # ores.cmake.component declares it, so a composite component
    # generates nothing from either facet by design.
    c["kind"] = fm.get("component_kind", "flat")
    return {"component": c}
