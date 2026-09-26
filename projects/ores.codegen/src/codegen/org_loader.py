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
import uuid
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
# Which of the entity dict's three namespaces (root / sql / presentation) a
# feature belongs to is fixed by the feature catalogue itself
# (projects/modeling/variability_features.org's grouping).

_PROFILES_DIR = Path(__file__).resolve().parents[3] / "modeling"

# feature name -> which sub-dict of the domain_entity a profile default for
# it is merged into. "" means the root of the domain_entity dict itself.
_FEATURE_NAMESPACE: dict[str, str] = {
    "has_tenant_id": "",
    "has_workspace_id": "",
    "has_parent_id": "",
    "read_only": "",
    "system_scope": "sql",
    "nullable_tenant_id": "sql",
    "no_audit_columns": "sql",
    "current_state": "sql",
    "extra_checks": "sql",
    "extra_delete_sets": "sql",
    "fk_copy_validations": "sql",
    "text_code_validations": "sql",
    "party_id_from_book_id": "sql",
    "party_id_from_session": "sql",
    "rls_tenant_isolation": "sql",
    "rls_party_isolation": "sql",
    "rls_system_tenant_visible": "sql",
    "has_pagination": "presentation",
    "has_uuid_primary_key": "presentation",
    "has_change_reason_cache": "presentation",
    "has_explorer_api": "presentation",
    "parent_entity_singular": "presentation",
    "has_csv_xml_io": "presentation",
    "has_export_macro": "presentation",
    "has_version_navigation": "presentation",
    "has_readonly_paginated_list": "presentation",
    "has_parent_scoped_list": "presentation",
    "parent_key_field": "presentation",
    "parent_key_param": "presentation",
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


@lru_cache(maxsize=None)
def _custom_type_names() -> frozenset[str]:
    """The type names the custom-type registry declares.

    A name here is a value type the tree owns -- a domain enum, a cron
    expression, a tenant uuid -- and not a model entity, which is what decides
    whether a TypeScript twin is an interface or a string.
    """
    return frozenset(name for name, _ in _load_custom_type_headers())


@lru_cache(maxsize=None)
def _load_custom_type_headers() -> tuple[tuple[str, str], ...]:
    """Parse projects/modeling/cpp_custom_types.org into (type, header) pairs.

    A column's ``:cpp_type:`` may name a type the domain-header template
    cannot derive an include for -- a domain enum, a value type such as
    ``cron_expression``. The registry binds each to the header that
    defines it so the model states the type once instead of stating the
    type and remembering the header separately.

    Rejects a registry that binds one type to two headers: a bare
    ``domain::`` name is component-relative rather than globally unique,
    so an ambiguity introduced later must fail rather than resolve to
    whichever row happens to come first.
    """
    path = _PROFILES_DIR / "cpp_custom_types.org"
    if not path.is_file():
        return ()
    doc = parse_org(path.read_text(encoding="utf-8"))
    section = _section(doc.root, "The registry")
    if not section:
        return ()
    out: dict[str, str] = {}
    for row in _parse_org_table_rows(section):
        name = row.get("Type", "").strip().strip("=")
        header = row.get("Header", "").strip().strip("=")
        if not name or not header:
            continue
        if name in out and out[name] != header:
            raise ValueError(
                f"cpp_custom_types.org binds '{name}' to both "
                f"{out[name]} and {header}; a type must name one header")
        out[name] = header
    return tuple(sorted(out.items()))


def _headers_for_types(cpp_types: list[str]) -> list[str]:
    """Registered headers the given column types need, in registry order.

    Matches a registered name as a whole type name inside each column's
    declared type, so ``std::optional<cron_expression>`` needs the same
    header as ``cron_expression`` -- the wrapper is not what needs
    including.

    Two boundary rules make that safe. A neighbouring identifier
    character rules the occurrence out, so ``asset_class_id`` is not a
    use of ``asset_class``. A preceding ``::`` rules it out too, so the
    bare ``domain::product_type`` does not match inside
    ``ores::trading::domain::product_type``: a name qualified by a
    different namespace is a different type, and matching it here would
    resolve it to another component's header without the registry's
    one-type-one-header check ever seeing the collision. Each spelling
    in use therefore earns its own row.

    Every column is searched, and every occurrence within it. An
    earlier column that merely contains a registered name must not hide
    a later column that actually declares it.
    """
    pattern_cache = [
        (header, re.compile(
            r"(?<![A-Za-z0-9_:])" + re.escape(name) + r"(?![A-Za-z0-9_])"))
        for name, header in _load_custom_type_headers()
    ]
    out: list[str] = []
    for header, pattern in pattern_cache:
        if header in out:
            continue
        if any(pattern.search(t) for t in cpp_types):
            out.append(header)
    return out


def _with_registered_headers(includes: dict, columns: list) -> None:
    """Add the headers the columns' registered custom types need.

    Appends rather than reorders, and only what is missing, so a model
    that already lists the header by hand is untouched -- which is what
    keeps regeneration byte-identical for every entity that predates the
    registry. Order does not matter in the output: codegen runs
    clang-format, and .clang-format sorts and merges include blocks.
    """
    types = [str(c.get("cpp_type", "")) for c in columns if c.get("cpp_type")]
    if not types:
        return
    domain = includes.setdefault("domain", [])
    for header in _headers_for_types(types):
        if header not in domain:
            domain.append(header)


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
    model follows. Every reader calls this so a misplaced :profile: fails
    loudly on all paths, not just the physical-space override pass.

    Both misplaced spellings are rejected. The drawer spelling lands in
    ``file_properties``; the ``#+profile:`` keyword spelling lands in
    ``frontmatter``, where nothing reads it -- every profile consumer
    resolves the key from the * Flags drawer. A keyword profile is
    therefore not a second binding point but a silent no-op, which is
    worse: the model renders with the profile's defaults missing and no
    diff, check or type can see it.

    The entity, junction, field-group and operation loaders call this, as
    does the physical-space override pass. A loader that resolves no
    profile has nothing to reject."""
    if "profile" in doc.file_properties:
        raise ValueError(
            ":profile: found in file-level :PROPERTIES: drawer — "
            "move it to the * Flags section's :PROPERTIES: drawer instead. "
            "Only * Flags is the canonical binding point for profiles.")
    if any(key.lower() == "profile" for key in doc.frontmatter):
        raise ValueError(
            "#+profile: found in the file frontmatter — "
            "move it to the * Flags section's :PROPERTIES: drawer instead. "
            "Only * Flags is the canonical binding point for profiles, and "
            "a frontmatter profile is silently ignored.")


def _reject_junction_only_flags(doc: "OrgDocument", kind: str) -> None:
    """Reject ``:client_read_only:`` outside a junction model.

    The flag splits a junction's repository write surface from the verbs a
    client reaches, and only :func:`load_org_junction_model` reads it. Every
    other model type that declares it renders as though it had not, which is
    the silent no-op the profile guard above rejects for ``:profile:``.

    ``:read_only:`` is not in this set: a domain_entity honours it too, for a
    table the application never writes.
    """
    cpp = _section(doc.root, "C++")
    drawers = [("* Flags", _section(doc.root, "Flags"))]
    if cpp:
        drawers.append(("* C++ ** Flags", _section(cpp, "Flags")))
    for where, section in drawers:
        if section and any(
                key.lower() == "client_read_only"
                for key in section.properties):
            raise ValueError(
                f":client_read_only: found in {where} of a {kind} model — "
                "only a junction separates a repository write surface from "
                "the verbs a client reaches. Use :read_only: to suppress "
                "every write, or model the table as a junction.")


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
    namespace (``""``/``"sql"``/``"presentation"``), as a plain dict -- used
    to seed a facet's raw properties *before* that facet derives any computed
    flag from them (e.g. the presentation drawer's ``has_toolbar`` from
    ``has_version_navigation``), so the derivation sees the profile's values
    rather than their absence."""
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
    Assignments as defaults into de (root / sql / presentation namespaces
    per feature), in list order. An already-explicit value at the entity level
    always wins -- a profile supplies defaults, it never overrides what the
    model author wrote. Two profiles bound together that disagree on the
    same feature raise rather than silently picking one -- see
    :func:`_profile_conflict_message`.

    This is the final safety-net pass over the whole ``de`` dict; namespaces
    whose facet derives computed flags from these features (currently
    ``presentation``) must also seed those defaults *before* that facet's own
    parsing via :func:`_profile_namespace_defaults`, since by the time this
    runs the derivation has already happened and setdefault here is too late
    to affect it."""
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


# Includes the repository entity-header template emits itself, plus the
# tokens legacy org "Entity includes" blocks add that no emitted member can
# require: Timestamp.hpp arrives via db_types.hpp (always emitted, and it
# includes Timestamp.hpp), and every uuid-typed column and PK/FK side
# renders as std::string, so boost uuid headers are never needed. <cstdint>
# appears in drift-rollout-era blocks as a stale transcription of pre-org
# headers; the canonical entity headers compile without it and the registry
# pins them byte-for-byte. Filtering these tokens keeps regeneration
# byte-identical; includes the template cannot derive -- e.g. the header of
# a domain-enum column type -- pass through verbatim.
_ENTITY_HEADER_STANDARD_INCLUDES = frozenset({
    "<string>",
    "<optional>",
    "<ostream>",
    '"ores.database/repository/db_types.hpp"',
    '"sqlgen/PrimaryKey.hpp"',
    '"sqlgen/Timestamp.hpp"',
    "<boost/uuid/uuid.hpp>",
    "<cstdint>",
})


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
        # A key column is read by renderers that carry their own type
        # projection: the entity declares the primary key as
        # sqlgen::PrimaryKey<std::string>, the natural-key block renders the
        # raw cpp_type, and the mapper's key branches convert by is_text,
        # is_uuid, is_int and is_timestamp. None of them carries
        # :is_value_type:, so a value-type key would reach the entity layer as
        # the domain class and the mapper would leave it unmapped -- silently,
        # because the value type is a legal column everywhere else. Refuse the
        # shape rather than generate it wrongly; plain columns carry the
        # projection (see test_value_type_column.py).
        if (is_primary_key or is_natural_key) and (
            _parse_typed(child.properties.get("is_value_type", "false")) is True
        ):
            role = "primary key" if is_primary_key else "natural key"
            raise ValueError(
                f"Column '{child.title}' is a {role} and opts in as a value "
                "type. The entity and mapper renderers carry the value-type "
                "projection for plain columns only, so a key column would be "
                "generated as the domain class and left unmapped. Model the key "
                "as the string the type wraps, or drop :is_value_type: from "
                "this column."
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
    # A named ``parent_seed`` source block under the FK heading is the
    # seeding snippet for a parent table with no modeling org (a
    # hand-authored table such as ores_iam_accounts_tbl): the eventing
    # integration test cannot call a generated synthetic generator for
    # it, so the org supplies the seed code verbatim. Emitted by
    # cpp_nats_integration_test.cpp.mustache in place of the auto
    # parent seeding, which only fires for org-resolved parents. The
    # sibling ``parent_seed_includes`` block names the headers the
    # snippet needs: the auto include emission derives its paths from
    # the parent's org metadata, which by definition does not exist
    # here.
    if "parent_seed" in node.src_blocks:
        out["parent_seed_snippet"] = node.src_blocks["parent_seed"]
    if "parent_seed_includes" in node.src_blocks:
        out["parent_seed_includes"] = node.src_blocks["parent_seed_includes"]
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


@lru_cache(maxsize=None)
def _collection_name_by_entity(projects_dir: Path) -> dict[str, str]:
    """Map every entity's ``#+entity_singular:`` to its ``:collection_name:``.

    Raw-text scan (no org parse), cached per process, the same shape and for
    the same reason as ``_entity_org_by_table`` above. A detail field's
    ``combo_domain_type`` names the domain type its options come from (e.g.
    ``refdata::domain::book_status``); the TypeScript lookup source needs the
    collection that type lists from, which only the target entity's own
    presentation drawer states.
    """
    out: dict[str, str] = {}
    for org in sorted(projects_dir.glob("*/modeling/*.org")):
        text = org.read_text(encoding="utf-8", errors="replace")
        sm = re.search(r"^#\+entity_singular:\s*(\S+)", text, re.M)
        if not sm:
            continue
        cm = re.search(r"^:collection_name:\s*(\S+)\s*$", text, re.M)
        if cm:
            out[sm.group(1)] = cm.group(1)
    return out


def collection_name_for_domain_type(projects_dir: Path,
                                    combo_domain_type: str) -> str | None:
    """The collection a ``refdata::domain::book_status`` combo lists from.

    Resolved one hop: the domain type's last ``::`` segment is the target
    entity's singular, and that entity's presentation drawer names its
    collection. Returns None when the hop does not resolve, and the caller
    omits the lookup rather than emitting a source it cannot name.
    """
    entity = (combo_domain_type or "").rsplit("::", 1)[-1]
    return _collection_name_by_entity(projects_dir).get(entity)


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


def _presentation_columns(node: OrgNode) -> list[dict[str, Any]]:
    """Convert the drawer's 'Columns' table into the dict list shape."""
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


def _presentation_icon_columns(node: OrgNode) -> list[dict[str, Any]]:
    """Convert the drawer's 'Icon columns' table into the dict list shape.

    `column` is the Column enum value of a column that renders an icon,
    and both projections read it to mark that column's style. The
    `accessor`, `field1` and `field2` cells record how the icon was built
    for the retired Qt view and have no reader now.

    `is_pair` marks a *visually* wide (roughly 2:1) composited icon. It
    defaults to true when `field2` is set, and a single-field accessor that
    still produces a pair icon internally must set it explicitly.
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


def _presentation_setting_gated_actions(node: OrgNode) -> list[dict[str, Any]]:
    """Convert the drawer's 'Setting-gated actions' table into the dict list shape.

    Each row names one QAction* member (`action`, without the trailing
    underscore) that the retired client's window classes declared, whose
    visibility is gated by a boolean system setting (`setting`). Wired
    via a single shared SettingGatedActionController per window rather
    than duplicating the subscribe/notify/query mechanism per action.
    """
    if not node.tables:
        return []
    return [
        {k: _parse_typed(v) for k, v in row.items()} for row in node.tables[0]
    ]


def _presentation_related_entity_shortcuts(node: OrgNode) -> list[dict[str, Any]]:
    """Convert the drawer's 'Related entity shortcuts' table into the dict list shape.

    Each row is a toolbar shortcut to a related entity's own list window —
    e.g. currency's Rounding Type / Monetary Nature / Market Tier combos each
    reference a small lookup entity worth a one-click detour to. `signal`
    named the emitted/relayed Qt signal (`show{signal}Requested`) of the
    retired client's toolbar action; `icon` is an Icon:: enum value;
    `tooltip`/`label` are that action's tooltip and button text. Wiring
    the signal to the target entity's own controller happened in the
    plugin's composition root (e.g. RefdataPlugin), not here —
    cross-controller wiring was inherently plugin-level."""
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


# The paste point the C++ protocol header renders a model's extra messages
# at, before it renders the ``* Messages`` section. A model that still feeds
# the paste point and also declares messages would render them twice there,
# while the TypeScript twin, which has no paste point, renders them once.
PROTOCOL_MESSAGES_PASTE_KIND = "2C4E8F1A-6B9D-4A3E-8F2C-7D1E5A9B3C6F"


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
    _reject_junction_only_flags(doc, "domain_entity")
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
        # party_id_from_book_id is a struct feature: the insert trigger
        # derives both party_id and portfolio_id from book_id, because the
        # book states both. The struct supplies the book table and the
        # message the existence check raises, which a Flags property
        # cannot carry.
        book_section = _section(sql_section, "Party id from book id")
        if book_section and book_section.properties:
            de.setdefault("sql", {})["party_id_from_book_id"] = {
                k.lower(): v for k, v in book_section.properties.items()
            }
        # An entity guarded by a state machine cannot be rewritten with the
        # activity that booked it: that activity names a transition which
        # starts the machine, and the row already has a state. The model
        # therefore names the activity an amendment carries, so a generated
        # round-trip test can amend the row it just wrote.
        transition_section = _section(sql_section, "Status transition")
        if transition_section and transition_section.properties:
            props = {k.lower(): v for k, v in transition_section.properties.items()}
            if props.get("amend_activity_code") and props.get("amend_activity_column"):
                de["amend_activity"] = {
                    "column": props["amend_activity_column"],
                    "code": props["amend_activity_code"],
                }
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
                # The repository entity-header template always emits its own
                # standard include set (string/optional/ostream/db_types/
                # PrimaryKey, with Timestamp.hpp arriving via db_types.hpp's
                # db_timestamp). Org "Entity includes" blocks predate that
                # fixed list and mostly repeat it, so surface only the tokens
                # the template cannot derive -- e.g. the header of a
                # domain-enum column type. Filtering keeps regeneration
                # byte-stable for orgs whose block duplicates the standard set.
                "entity": [
                    t for t in _includes_from_named_block(ent)
                    if t not in _ENTITY_HEADER_STANDARD_INCLUDES
                ] if ent else [],
            }
            _with_registered_headers(cpp_out["includes"], de.get("columns", []))
        conv = _section(cpp_section, "Conventions")
        if conv:
            for k, v in conv.properties.items():
                cpp_out[k.lower()] = _parse_typed(v)
        # Domain groups: the N-way generalisation of the
        # domain_identity_group/domain_audit_group pair. An entity whose
        # reflected struct must stay under the MSVC C1202 threshold splits
        # its columns across several field groups, and two slots cannot
        # express more than two. Each row names the member and the field
        # group model supplying its type; row order is member order, which
        # is also the wire order.
        dg = _section(cpp_section, "Domain groups")
        if dg and dg.tables:
            rows = _parse_org_table_rows(dg)
            de["domain_groups"] = [
                {"member": r["member"], "field_group": r["field_group"]}
                for r in rows if r.get("member") and r.get("field_group")
            ]
        td = _section(cpp_section, "Table display")
        if td:
            cpp_out["table_display"] = _table_display(td)
        # The table renderer names the row it is streaming, and a model that
        # leaves the drawer out or states it empty renders `const auto& : v`,
        # which does not compile. One default, stated here, is what
        # _prepare_table_display() already assumes for its own loop.
        if not str(cpp_out.get("iterator_var", "")).strip():
            cpp_out["iterator_var"] = "e"
        if cpp_out:
            de["cpp"] = cpp_out

        # Presentation bindings.
        drawer = _section(cpp_section, "Presentation")
        if drawer:
            de["presentation"] = _parse_presentation_drawer(
                drawer, _profile_namespace_defaults(de.get("profile"), "presentation")
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

    # Messages the entity declares beside its derived CRUD set. The C++
    # protocol header renders them at its paste point and the TypeScript
    # twin appends them to the derived list, so one section feeds both.
    declared = parse_declared_messages(doc.root)
    if declared:
        if PROTOCOL_MESSAGES_PASTE_KIND in (de.get("implementations") or {}):
            raise ValueError(
                "a * Messages section and an :implements "
                f"{PROTOCOL_MESSAGES_PASTE_KIND} block are both present — "
                "the C++ protocol header renders the paste point and then the "
                "declared messages, so the same structs would appear twice "
                "while the TypeScript twin renders them once. Move the paste "
                "block's messages into * Messages and delete the block.")
        de["declared_messages"] = declared

    return {"domain_entity": de}


def _parse_presentation_drawer(
    drawer: OrgNode, profile_defaults: dict[str, Any] | None = None
) -> dict[str, Any]:
    """Parse a ``** Presentation`` drawer (properties + Detail fields /
    Columns / Icon columns / Setting-gated actions / Related entity
    shortcuts sub-sections) into the ``presentation`` sub-dict a consuming
    facet renders from. Shared by :func:`org_document_to_model`
    (domain_entity) and :func:`load_org_junction_model` (junction) — the
    drawer shape and every derived flag below is identical for both.

    ``profile_defaults`` (a bound entity's profile's presentation-namespace
    Assignments, if any) is seeded in *before* any derivation below runs --
    e.g. ``has_toolbar``'s derivation from ``has_version_navigation`` must
    see the profile-supplied value, not its absence. An explicit drawer
    property still wins over it either way."""
    presentation_out: dict[str, Any] = {}
    for k, v in drawer.properties.items():
        presentation_out[k.lower()] = _parse_typed(v)
    if profile_defaults:
        for k, v in profile_defaults.items():
            presentation_out.setdefault(k, v)
    df = _section(drawer, "Detail fields")
    if df:
        presentation_out["detail_fields"] = _detail_fields(df)
        # static_combo fields declare their fixed option set as a
        # comma-separated :combo_values: string (e.g. "Active,
        # Inactive,Closed") — parsed here into the {label, value}
        # dicts the template iterates. label == value; if a field
        # ever needs them to differ, extend this to accept
        # "label:value" pairs.
        for f in presentation_out["detail_fields"]:
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
    qc = _section(drawer, "Columns")
    if qc:
        presentation_out["columns"] = _presentation_columns(qc)
        # Preview columns for the generic import dialog (has_csv_xml_io):
        # excludes audit/system fields that are meaningless before a
        # record is imported (its own version/modified_by/recorded_at
        # belong to the never-yet-saved row, not the source file).
        #
        # Opt-in, not opt-out: a column only appears in the import
        # preview if its row in "Columns" sets
        # :import_preview: true explicitly. An exclude-list keyed on
        # generic properties (is_timestamp, audit field names) was
        # tried first and silently over-included fields the entity
        # author never intended to preview (caught in PR #1445
        # review — currency's hand-migrated dialog only shows 5 of
        # its 14 columns, not the ~11 an opt-out list would keep).
        presentation_out["import_preview_columns"] = [
            c for c in presentation_out["columns"] if c.get("import_preview")
        ]
    ic = _section(drawer, "Icon columns")
    if ic:
        presentation_out["icon_columns"] = _presentation_icon_columns(ic)
        presentation_out["has_icon_columns"] = bool(presentation_out["icon_columns"])
        # Any pair (roughly 2:1) composited icon needed the retired
        # view's iconSize widened past Qt's default square box — see
        # currency_pair_icon_size() in its FlagIconHelper.hpp — or it
        # rendered squished.
        presentation_out["has_pair_icon_column"] = any(
            entry.get("is_pair") for entry in presentation_out["icon_columns"]
        )
    sga = _section(drawer, "Setting-gated actions")
    if sga:
        presentation_out["setting_gated_actions"] = _presentation_setting_gated_actions(sga)
        presentation_out["has_setting_gated_actions"] = bool(presentation_out["setting_gated_actions"])
    # Every entity gets a generate_synthetic_<entity> generator (ores.cpp.generator
    # facet) — the retired detail dialog opted into a "Generate" toolbar button
    # filling its fields from it by naming the QAction member "generateAction" in
    # the Setting-gated actions table above (member declaration + visibility
    # gating both came from that table already; this only decided whether the
    # click handler and its generator call got generated).
    presentation_out["has_generate_action"] = any(
        a.get("action") == "generateAction" for a in presentation_out.get("setting_gated_actions", [])
    )
    # A detail dialog needed a QToolBar iff it hosted version-nav
    # actions, the Generate action (both added QAction rows to it),
    # or the author explicitly asked for one (e.g. to host a
    # hand-written paste-block action with no dedicated knob of
    # its own, like calendar's "Regenerate up to <year>") --
    # an explicit :has_toolbar: true in the drawer must survive
    # this derivation, not be silently overwritten by it.
    presentation_out["has_toolbar"] = bool(
        presentation_out.get("has_toolbar")
        or presentation_out.get("has_version_navigation")
        or presentation_out["has_generate_action"]
    )
    res = _section(drawer, "Related entity shortcuts")
    if res:
        presentation_out["related_entity_shortcuts"] = _presentation_related_entity_shortcuts(res)
        presentation_out["has_related_entity_shortcuts"] = bool(presentation_out["related_entity_shortcuts"])
    # has_flag_icon is derived, not a separately-authored property:
    # an entity has the single-column image_id-keyed flag mechanism
    # iff it declared which column shows it. Any manually-set
    # :has_flag_icon: in the .org file is ignored/overwritten here —
    # it was always redundant with :flag_icon_column: being present.
    presentation_out["has_flag_icon"] = bool(presentation_out.get("flag_icon_column"))
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
    if not presentation_out.get("key_flag_field") and presentation_out.get("flag_inline_widget"):
        presentation_out["key_flag_field"] = presentation_out["flag_inline_widget"]
    if not presentation_out.get("key_flag_accessor") and presentation_out.get("flag_accessor"):
        presentation_out["key_flag_accessor"] = presentation_out["flag_accessor"]
    presentation_out["has_key_flag_icon"] = bool(presentation_out.get("key_flag_field"))
    # Any list view showing a flag at all (own image_id-backed
    # column, or a derived icon_columns entry) must set an explicit
    # iconSize rather than rely on Qt's implicit per-style default —
    # see single_flag_icon_size()/currency_pair_icon_size() in the
    # retired client's FlagIconHelper.hpp: two views relying on the
    # implicit default weren't guaranteed to render the same flag at
    # the same size.
    presentation_out["has_any_flag_icon"] = (
        presentation_out["has_flag_icon"] or presentation_out.get("has_icon_columns", False)
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
    presentation_out["has_combo_flag_source"] = any(
        f.get("flag_source") for f in presentation_out.get("detail_fields", [])
    )
    # Whether any dynamic-combo detail field uses the
    # populateDynamicCombo<Entity> helper (fetch/sort/tooltip/
    # placeholder/restore-selection, piloted on currency's
    # rounding_type/monetary_nature/market_tier combos) — gates
    # the DynamicComboSetup.hpp/LookupFetcher.hpp includes.
    presentation_out["has_dynamic_combo_helper_fields"] = any(
        f.get("combo_domain_type") for f in presentation_out.get("detail_fields", [])
    )
    presentation_out["needs_image_cache"] = (
        presentation_out["has_flag_icon"]
        or presentation_out["has_key_flag_icon"]
        or presentation_out.get("has_icon_columns", False)
        or presentation_out["has_combo_flag_source"]
    )
    return presentation_out


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
        # A model lives under its component's include root when the component
        # has no sub-components, and says so rather than naming a directory
        # that does not exist.
        if k == "subcomponent" and de.get("no_subcomponent"):
            continue
        if k not in de:
            errors.append(f"Missing required flag: {k}")

    # A key is required of a record that is stored, not of one that is only
    # carried. The two are told apart by the model saying which it is, so an
    # entity that forgets a key still fails, and an entity that has none is not
    # asked to invent one.
    if de.get("no_primary_key"):
        if "primary_key" in de:
            errors.append(
                "States :no_primary_key: true and also flags a primary key")
    elif "primary_key" not in de:
        errors.append(
            "Missing primary key: no field in 'Columns' is flagged "
            ":primary_key: true -- or state :no_primary_key: true if the "
            "record carries no key"
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
    _resolve_domain_group_fields(model, Path(path))
    errors = validate_model(model)
    if errors:
        raise ValueError(
            f"Validation errors in {path}:\n  " + "\n  ".join(errors)
        )
    return model


def _resolve_domain_group_fields(model: dict[str, Any], path: Path) -> None:
    """Attach each domain group's field types to the entity.

    A grouped entity reaches its columns through a group, so its own
    ``columns`` list says nothing about them. Anything keyed on a
    column's C++ type -- the table-display converter's choice between
    streaming a value, wrapping an optional, or stringifying a uuid --
    would otherwise see no type at all and emit code that does not
    compile. Read each group's field group model, which sits beside this
    one, and record the member-qualified name and type of every field.
    """
    de = (model or {}).get("domain_entity") or {}
    groups = de.get("domain_groups") or []
    if not groups:
        return
    # Three ways a grouped model can lose a field without saying so. The
    # struct is the member list alone, so anything the groups do not cover
    # is simply absent from it, and the two older mechanisms have no say
    # once Domain groups is present.
    for older in ("domain_identity_group", "domain_audit_group"):
        if de.get(older):
            raise ValueError(
                f"{path}: declares Domain groups and {older}. Domain groups "
                f"supersedes the identity/audit pair; state the group as a "
                f"row of the table instead."
            )
    if ((de.get("cpp") or {}).get("includes") or {}).get("domain"):
        raise ValueError(
            f"{path}: declares Domain groups and a Domain includes block. A "
            f"grouped struct reaches every field through a member, so the "
            f"group headers are its only domain includes and the block would "
            f"be dropped. Remove it."
        )
    resolved: list[dict[str, str]] = []
    for group in groups:
        fg_path = path.with_name(f"{group['field_group']}_field_group.org")
        if not fg_path.exists():
            raise ValueError(
                f"{path}: domain group '{group['member']}' names field group "
                f"'{group['field_group']}', but {fg_path.name} does not exist"
            )
        fg = load_org_field_group_model(fg_path).get("field_group", {})
        for field in fg.get("fields", []) or []:
            resolved.append({
                "name": f"{group['member']}.{field['name']}",
                "cpp_type": field.get("cpp_type", ""),
            })
    de["domain_group_fields"] = resolved
    covered = {f["name"].split(".", 1)[1] for f in resolved}
    declared = {c["name"] for c in de.get("columns", []) or []}
    pk = de.get("primary_key") or {}
    if pk.get("column"):
        declared.add(pk["column"])
    for col in pk.get("columns", []) or []:
        # A compound key's entries spell the column as "column"; a single
        # key's own dict spells it as "name".
        name = col.get("name") or col.get("column")
        if name:
            declared.add(name)
    missing = sorted(declared - covered)
    if missing:
        raise ValueError(
            f"{path}: column(s) {', '.join(missing)} belong to no domain "
            f"group. A grouped struct is its member list, so a column no "
            f"group carries is absent from it."
        )


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
    # A junction stores its key columns in the entity's own types, which are
    # not the domain's: a uuid and a date are both text in the table. The
    # templates branch on these flags to state the conversion once, so a
    # query compares the column's type and a caller passes the domain's.
    cpp_type = str(out.get("cpp_type", ""))
    out["is_uuid"] = cpp_type == "boost::uuids::uuid"
    out["is_date"] = cpp_type == "std::chrono::year_month_day"
    out["is_timestamp"] = cpp_type == "std::chrono::system_clock::time_point"
    out["query_needs_str"] = out["is_uuid"] or out["is_date"] or out["is_timestamp"]
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

    if not j.get("name_singular"):
        raise ValueError(
            f"{path}: a junction must declare :name_singular:. The singular "
            f"names the generated header, the C++ types and the protocol "
            f"messages, and no rule derives it from the plural name safely."
        )

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

    # Validations through a named function, parsed identically to a
    # domain_entity's (see load_org_model): an `* Insert trigger` section
    # whose `** Validations` table names the function each column is
    # checked by. A junction's sides are always mandatory, so nothing here
    # is nullable and the template renders the unconditional form.
    insert_section = _section(doc.root, "Insert trigger")
    if insert_section:
        validations_section = _section(insert_section, "Validations")
        if validations_section:
            j["insert_trigger"] = {
                "validations": _parse_org_table_rows(validations_section)
            }

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
    # Flags: lift directly onto junction (these are top-level in the
    # JSON model), matching load_org_model()'s domain_entity Flags
    # handling -- e.g. :subcomponent: for resolve_output_path()'s
    # component_dir/component_include/... derivation.
    flags = _section(cpp_section, "Flags") if cpp_section else None
    if flags:
        for k, v in flags.properties.items():
            j[k.lower()] = _parse_typed(v)
    # Two independent switches decide the write surface. ``read_only`` is the
    # repository's: it suppresses write and remove, for a table provisioned
    # outside the application. ``client_read_only`` leaves the repository
    # writable for a server-side producer and suppresses only the verbs a
    # client can reach, which the wire templates and the derived TypeScript
    # list branch on through the derived flag computed at the end of this
    # function. A junction with no C++ drawer declares neither flag, so its
    # wire writes stay on, and a template that reads a missing key would treat
    # the absence as off and silently drop the write surface.
    if cpp_section:
        dom = _section(cpp_section, "Domain includes")
        ent = _section(cpp_section, "Entity includes")
        if dom or ent:
            cpp_out["includes"] = {
                "domain": _includes_from_named_block(dom) if dom else [],
                # Junction entity headers render the same fixed include set
                # and stringify both FK sides, so filter org tokens with the
                # same standard set as domain entities (see
                # _ENTITY_HEADER_STANDARD_INCLUDES). Without the filter a
                # junction org repeating the set re-emits the covered tokens
                # (e.g. sqlgen/Timestamp.hpp) on regeneration.
                "entity": [
                    t for t in _includes_from_named_block(ent)
                    if t not in _ENTITY_HEADER_STANDARD_INCLUDES
                ] if ent else [],
            }
            _with_registered_headers(cpp_out["includes"], columns)
        conv = _section(cpp_section, "Conventions")
        if conv:
            for k, v in conv.properties.items():
                cpp_out[k.lower()] = _parse_typed(v)
        td = _section(cpp_section, "Table display")
        if td:
            cpp_out["table_display"] = _table_display(td)

        # Presentation bindings, parsed identically to a domain_entity's
        # ** Presentation drawer (see _parse_presentation_drawer) -- but a
        # junction's own fields
        # (name_singular/name/... , repository.name_short/...) use
        # different key names than domain_entity's (entity_singular/
        # entity_plural/..., repository.entity_plural_short/...), so a
        # consuming facet written against the domain_entity shape needs
        # those keys aliased onto j directly. Only done when a presentation
        # drawer is actually present -- a junction without one stays
        # exactly as before.
        drawer = _section(cpp_section, "Presentation")
        if drawer:
            j["presentation"] = _parse_presentation_drawer(
                drawer, _profile_namespace_defaults(j.get("profile"), "presentation")
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
                # the junction's top level for the repository/
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
    # handling: the * Flags drawer's :profile: resolves against the named
    # profile's own Assignments table as feature defaults.
    _apply_profile(j)

    # Derived here rather than beside the flags it reads: a profile can supply
    # read_only, so the derivation has to follow _apply_profile. Computed
    # earlier, a profile-bound junction advertises write messages for
    # repository methods the repository template has already dropped, because
    # that template reads read_only directly and sees the profile's value.
    j["wire_write_enabled"] = not (
        j.get("read_only") or j.get("client_read_only"))

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
    _ensure_profile_binding(doc)
    _reject_junction_only_flags(doc, "field group")
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


def _indent_block(text: str, spaces: int) -> str:
    """Pad every non-blank line of ``text`` by ``spaces``.

    Mustache cannot indent a multi-line value, so a nested comment is
    pre-indented here rather than in the template. Blank lines stay
    blank — padding them would leave trailing whitespace in the output.
    """
    pad = " " * spaces
    return "\n".join(pad + line if line.strip() else line
                     for line in text.splitlines())


_TS_SCALARS = {
    "std::string": "string",
    "bool": "boolean",
    "int": "number",
    # The fixed-width family and the floating types. A missing entry is not
    # harmless: an unqualified name falls through to the PascalCase fallback
    # and renders a type that does not exist, which is how ``double`` came to
    # emit ``Double`` on the roughly fifty members that carry one.
    "float": "number",
    "double": "number",
    "std::int8_t": "number",
    "std::int16_t": "number",
    "std::int32_t": "number",
    "std::int64_t": "number",
    "std::uint8_t": "number",
    "std::uint16_t": "number",
    "std::uint32_t": "number",
    "std::uint64_t": "number",
    # A byte offset within a rendered value. It is a count, so it crosses as
    # the same JavaScript number every other integer does; only the width
    # differs, and JSON carries no width.
    "std::size_t": "number",
    # Both cross the wire as a string, per their rfl reflectors in
    # ores.utility/rfl/reflectors.hpp.
    "boost::uuids::uuid": "string",
    # tenant_id's reflector writes std::string too (reflectors.hpp), and a
    # message-shaped model carries the type the domain struct declares.
    "utility::uuid::tenant_id": "string",
    "std::chrono::year_month_day": "string",
    # An address crosses the wire as its textual form, per the same
    # reflectors, so a message that carries one states a string. It is listed
    # here as well as in the domain-only set below because a write record can
    # carry one: a login records the address it came from.
    "boost::asio::ip::address": "string",
    # A time point crosses the wire as its ISO 8601 text, per the same
    # reflectors. An event states when it occurred, so a protocol that could
    # not project one would have no event.
    "std::chrono::system_clock::time_point": "string",
}

# A domain member's fully qualified C++ name, e.g.
# ``ores::iam::domain::tenant_type``. The entity is the interface the
# ``ores.ts.domain`` facet emits for that entity, so the two projections
# name one type. ``ores::utility::`` is excluded: its domain types --
# ``hierarchy_node`` is the one a protocol carries -- are hand-written
# utility types with no entity model behind them, so there is no facet
# output to reference. Those go through ``_TS_UTILITY_DOMAIN_TYPES``
# instead.
_TS_DOMAIN_TYPE_RE = re.compile(
    r"^ores::(?!utility::)[A-Za-z_][A-Za-z0-9_]*::domain::"
    r"([A-Za-z_][A-Za-z0-9_]*)$"
)

# Hand-written domain types that cross the wire, mapped to the TypeScript
# interface and the module in the wire-protocol package that declares it. They
# have no entity model, so there is no ``ores.ts.domain`` facet to emit them and
# no per-component domain module to import from; the generated protocol imports
# the shared module the same way it imports an entity interface. An unlisted
# type of this kind still has no projection, which is what keeps the gap loud.
#
# Most are ``ores::utility::*`` -- the shared protocol records. The diff engine's
# payloads are the other kind: a component with an engine but no entity models,
# whose types still travel in a history response.
_TS_UTILITY_DOMAIN_TYPES = {
    "ores::utility::domain::hierarchy_node": ("HierarchyNode", "utility/hierarchy"),
    "ores::utility::domain::result": ("Result", "utility/protocol"),
    "ores::utility::domain::precondition": ("Precondition", "utility/protocol"),
    "ores::utility::domain::change_intent": ("ChangeIntent", "utility/protocol"),
    "ores::utility::domain::order": ("Order", "utility/protocol"),
    "ores::utility::domain::scope": ("Scope", "utility/protocol"),
    "ores::diff::domain::field_value": ("FieldValue", "diff/protocol"),
    "ores::diff::domain::diff_span": ("DiffSpan", "diff/protocol"),
    "ores::diff::domain::diff_entry": ("DiffEntry", "diff/protocol"),
    "ores::diff::domain::diff_result": ("DiffResult", "diff/protocol"),
}

# The same qualified name inside a larger C++ type, e.g.
# ``std::vector<ores::iam::domain::account_party>`` or
# ``std::optional<ores::iam::domain::role>``. An operation field of one of
# these types renders the entity's interface, so the module must import it;
# see ``ts_domain_imports``. ``ores::utility::`` is excluded for the same
# reason as above.
_TS_DOMAIN_REF_RE = re.compile(
    r"ores::(?!utility::)[A-Za-z_][A-Za-z0-9_]*::domain::"
    r"([A-Za-z_][A-Za-z0-9_]*)"
)


def _to_pascal_case(name: str) -> str:
    """The interface name a snake_case message name renders to."""
    return "".join(part.capitalize() for part in name.split("_"))


def _ts_type(cpp_type: str) -> str | None:
    """Project a C++ member type onto its TypeScript counterpart.

    A member whose type is ``ores::<component>::domain::<entity>`` takes
    the PascalCase interface ``ores.ts.domain`` emits for that entity, and
    a timestamp takes ``string`` (the JSON shape rfl::json writes).

    Returns ``None`` for a type with no projection yet. Callers decide
    whether that absence is fatal; see ``load_org_operation_model`` and
    ``_reject_silent_ts_gap``.

    An unqualified name is a message defined in the same protocol, so it
    takes the interface name the template will emit for it.
    """
    if cpp_type.startswith("std::optional<") and cpp_type.endswith(">"):
        # rfl::json writes an unset optional as null, so the field is
        # nullable rather than absent.
        inner = _ts_type(cpp_type[len("std::optional<"):-1])
        return f"{inner} | null" if inner else None
    if cpp_type.startswith("std::vector<") and cpp_type.endswith(">"):
        inner = _ts_type(cpp_type[len("std::vector<"):-1])
        return f"{inner}[]" if inner else None
    if cpp_type in _TS_SCALARS:
        return _TS_SCALARS[cpp_type]
    if cpp_type == "std::chrono::system_clock::time_point":
        return "string"
    # A registered custom value type -- a domain enum, a cron expression, a
    # tenant uuid -- is not a model entity: the tree emits no interface for it,
    # so a name invented from its C++ spelling would reference a type that does
    # not exist, and the entity that names it would import a module nothing
    # writes. Each crosses the wire as the string its reflector writes, and the
    # registry is the one declaration of which names these are.
    if cpp_type in _custom_type_names():
        return "string"
    domain = _TS_DOMAIN_TYPE_RE.match(cpp_type)
    if domain:
        return _to_pascal_case(domain.group(1))
    # An unqualified ``domain::<name>`` is an enum the model declares beside the
    # entity rather than a type another component owns, and it crosses the wire
    # as the enumerator's own name: rfl reflects an enum class to a string, and
    # the store holds the same text (the SQL such a model writes compares the
    # column to 'system', not to a number). The TypeScript twin is therefore
    # ``string``, which is what every other enumerated column in the tree
    # projects to.
    if cpp_type.startswith("domain::"):
        return "string"
    utility = _TS_UTILITY_DOMAIN_TYPES.get(cpp_type)
    if utility:
        return utility[0]
    if "::" not in cpp_type:
        return _to_pascal_case(cpp_type)
    return None


# Domain member types the protocol projection deliberately refuses -- they
# are not operation-message types -- but the domain interface itself can
# express. Each crosses the wire as a string, per its rfl reflector in
# ores.utility/rfl/reflectors.hpp:
#
#   boost::uuids::uuid          ReflType std::string
#   std::chrono::year_month_day ReflType std::string (ISO 8601 date)
#   boost::asio::ip::address    ReflType std::string (IPv4 or IPv6)
#
# boost::asio::ip::tcp is not listed: it is an HTTP infrastructure type, not
# a domain struct member, so it has no wire shape to project.
_TS_DOMAIN_STRING_TYPES = frozenset({
    "boost::uuids::uuid",
    "std::chrono::year_month_day",
    "boost::asio::ip::address",
})


def _ts_domain_type(cpp_type: str) -> str | None:
    """Project a domain struct member's C++ type onto TypeScript.

    The domain interface mirrors the object the C++ domain class declares,
    so it is the member type (``{{{cpp_type}}}`` verbatim in
    ``cpp_domain_type_class.hpp.mustache``) that decides the interface
    field: an explicit ``std::optional<T>`` becomes ``T | null`` because
    rfl::json writes null for it, a uuid, a date and an IP address become
    ``string``, and everything else is the protocol projection's answer.
    Returns ``None`` when no projection exists;
    ``_reject_silent_entity_domain_ts_gap`` refuses the model rather than
    let the template emit a member with an empty type.
    """
    cpp_type = (cpp_type or "").strip()
    if cpp_type.startswith("std::optional<") and cpp_type.endswith(">"):
        inner = _ts_domain_type(cpp_type[len("std::optional<"):-1])
        return f"{inner} | null" if inner else None
    if cpp_type in _TS_DOMAIN_STRING_TYPES:
        return "string"
    return _ts_type(cpp_type)


def _ts_field(name: str, cpp_type: str, comment: str = "",
              default: str = "") -> dict[str, Any]:
    """One derived protocol field, with its TypeScript type when one exists.

    ``comment`` is always set: Mustache resolves a name it cannot find by
    walking up the context stack, so an absent ``comment`` would inherit
    the enclosing message's. ``default`` is the C++ initialiser the
    specification states for a field whose value has a defined starting
    point, and is absent rather than empty when there is none, so the
    renderer writes a bare member.
    """
    field: dict[str, Any] = {"name": name, "cpp_type": cpp_type,
                             "comment": comment}
    if default:
        field["default"] = default
    mapped = _ts_type(cpp_type)
    if mapped:
        field["ts_type"] = mapped
    return field


def _ts_message(
    name: str,
    *,
    response_type: str | None = None,
    subject: str | None = None,
    fields: list[dict[str, Any]] | None = None,
    verb: str | None = None,
) -> dict[str, Any]:
    """One derived protocol message: the shape both twins render from.

    ``verb`` states what an operation asks for when its name does not, which
    is any operation on a sub-resource: an entity whose plural ends in
    ``_versions`` names its own list the way the versions sub-resource names
    its own, so the name alone cannot tell the two apart.
    """
    message: dict[str, Any] = {
        "name": name,
        "name_pascal": _to_pascal_case(name),
        "comment": "",
        "fields": fields or [],
        # Derived here rather than declared by a model's own * Messages
        # section. The service and handler templates render the derived
        # operations, because their names and subjects come from this
        # derivation; a declared message's handler is written by hand beside
        # the operation model that states it.
        "derived": True,
    }
    if verb:
        message["verb"] = verb
    if response_type:
        message["response_type"] = response_type
    if subject:
        message["subject"] = subject
    return message


def ts_domain_imports(messages: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """The domain interfaces an operation's fields render, one entry each.

    A field whose C++ type names ``ores::<component>::domain::<entity>`` --
    on its own or inside a container -- renders that entity's interface, so
    the module must import it or the name is undefined. Returns
    ``{"entity": ..., "entity_pascal": ...}`` in entity-name order, so the
    template emits one import per entity regardless of how many messages
    reference it.
    """
    entities: set[str] = set()
    registered = _custom_type_names()
    for message in messages:
        for field in message.get("fields") or []:
            for match in _TS_DOMAIN_REF_RE.finditer(field.get("cpp_type") or ""):
                # A registered custom value type renders as a string, so there
                # is no interface to import. See _ts_type.
                if match.group(0) in registered:
                    continue
                entities.add(match.group(1))
    # A type whose TypeScript is hand-written is imported from its shared
    # module by ts_utility_imports(); claiming it here too would emit a second
    # import from a per-component domain module that does not exist.
    hand_written = {qualified.rsplit("::", 1)[-1]
                    for qualified in _TS_UTILITY_DOMAIN_TYPES}
    return [
        {"entity": entity, "entity_pascal": _to_pascal_case(entity)}
        for entity in sorted(entities - hand_written)
    ]


def apply_ts_domain_alias(
        entity: dict[str, Any],
        messages: list[dict[str, Any]]) -> str:
    """Alias the domain interface when a shared utility interface owns its name.

    An entity and a shared utility type can carry the same TypeScript name, as
    the envelope's ``Result`` and an entity named ``result`` do. The protocol
    imports both, and two imports of one identifier from two modules is a
    duplicate identifier. The domain import takes a suffixed alias and the
    fields that name the entity's own type follow it, so the two stay apart.
    Every other entity keeps the plain import and stays byte-identical.

    Returns the alias, or an empty string when the name is free.
    """
    singular = entity.get("entity_singular", "")
    component = entity.get("component", "")
    pascal = _to_pascal_case(singular)
    taken = {item["name_pascal"] for item in ts_utility_imports(messages)}
    if pascal not in taken:
        return ""
    alias = f"{pascal}Entity"
    own = f"ores::{component}::domain::{singular}"
    for message in messages:
        for field in message.get("fields") or []:
            if own in (field.get("cpp_type") or "") and field.get("ts_type"):
                field["ts_type"] = re.sub(
                    rf"\b{re.escape(pascal)}\b", alias, field["ts_type"])
    return alias


def ts_utility_imports(messages: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """The shared utility interfaces a protocol's fields render, one each.

    A field whose C++ type names a registered
    ``ores::utility::domain::<type>`` -- on its own or inside a container --
    renders that type's hand-written interface, so the module must import it
    from the wire-protocol package's shared ``utility/`` directory. Returns
    ``{"name_pascal": ..., "module": ...}`` in interface-name order.
    """
    found: dict[str, str] = {}
    for message in messages:
        for field in message.get("fields") or []:
            cpp_type = field.get("cpp_type") or ""
            for qualified, (pascal, module) in _TS_UTILITY_DOMAIN_TYPES.items():
                # A word boundary, not a substring test: a registered name
                # that prefixes another (``hierarchy_node`` against
                # ``hierarchy_node_view``) must not import an interface the
                # field never renders.
                if re.search(re.escape(qualified) + r"(?![A-Za-z0-9_])", cpp_type):
                    found[pascal] = module
    return [
        {"name_pascal": pascal, "module": found[pascal]}
        for pascal in sorted(found)
    ]


# The specification's vocabulary. A subject has exactly four segments and its
# last is one of these verbs; a scoped read adds a relation to the verb rather
# than a segment, so it stays four. An event's last segment is an action
# instead, drawn from its own closed set, because an event reports what
# happened and no caller asked for it.
SPEC_VERBS = (
    "get", "get_many", "list", "put", "put_many", "delete", "delete_many")
SPEC_EVENT_ACTIONS = ("created", "updated", "deleted")
SPEC_VERSIONS_VERBS = ("list", "get")


def entity_event_prefix(component: str, plural: str) -> str:
    """``iam.v1.tenants_events`` -- the collection one entity's events share.

    The prefix names the events collection, and the action is the last
    segment, so one payload is addressed by three subjects.
    """
    return f"{component}.v1.{plural}_events"


def entity_events(component: str, plural: str) -> list[dict[str, Any]]:
    """The subjects one entity's events are published on, one per action."""
    return [{"action": action, "subject": event_subject(component, plural, action)}
            for action in SPEC_EVENT_ACTIONS]


def request_subject(component: str, plural: str, verb: str) -> str:
    """``iam.v1.tenants.get`` -- one subject per resource and verb.

    ``verb`` is one of ``SPEC_VERBS``, or ``list_by_<relation>`` for a read
    scoped to a related entity. The relation is part of the verb, not a fifth
    segment, which is what keeps the grammar at four.
    """
    if verb not in SPEC_VERBS and not verb.startswith("list_by_"):
        raise ValueError(
            f"{verb!r} is not a verb of this protocol; "
            f"expected one of {SPEC_VERBS} or list_by_<relation>")
    return f"{component}.v1.{plural}.{verb}"


def event_subject(component: str, plural: str, action: str) -> str:
    """``iam.v1.tenants_events.created`` -- an announcement, not an operation.

    The resource segment names the events collection rather than the resource,
    in the ``{resource}_{collection}`` form a sub-resource uses, so the subject
    still has four segments and still sits inside its component's namespace.
    """
    if action not in SPEC_EVENT_ACTIONS:
        raise ValueError(
            f"{action!r} is not an event action; "
            f"expected one of {SPEC_EVENT_ACTIONS}")
    return f"{component}.v1.{plural}_events.{action}"


def versions_subject(component: str, plural: str, verb: str) -> str:
    """``iam.v1.tenants_versions.list`` -- an entity's versions, read-only.

    Versions are written by the database and never by a caller, so only the
    read verbs exist for the collection.
    """
    if verb not in SPEC_VERSIONS_VERBS:
        raise ValueError(
            f"{verb!r} is not a read; a versions collection is read-only, "
            f"so expected one of {SPEC_VERSIONS_VERBS}")
    return f"{component}.v1.{plural}_versions.{verb}"


# Fields the service derives and a client therefore never sends. The
# specification lists them once, and a request that carries one is invalid
# rather than silently overwritten, so a write record must not offer them.
SERVER_OWNED_FIELDS = frozenset({
    "tenant_id", "party_id", "version", "modified_by", "performed_by",
    "recorded_at", "valid_from", "valid_to",
})

# Why a change is being made is user-owned, but it travels beside the write
# record as change intent rather than inside it, so neither of these is a
# write-record field either.
CHANGE_INTENT_FIELDS = frozenset({
    "change_reason_code", "change_commentary",
})


def _column_name(column: dict[str, Any]) -> str:
    """A column's name, whichever of its three spellings the model carries.

    A ``* Columns`` entry is named ``name`` by the org drawer, while a primary
    key column, a foreign key and the repository's key aliases are named
    ``column``, and a presentation column table is named ``field``. Reading
    only one of them emits a member with no name at all, which is a wire shape
    nothing can address.
    """
    return (column.get("column") or column.get("field")
            or column.get("name") or "")


def write_record_fields(
    columns: list[dict[str, Any]],
    key_columns: frozenset[str] | set[str] = frozenset(),
) -> list[dict[str, Any]]:
    """The fields a client may send, in the order the model declares them.

    A write carries the user-owned fields and nothing else: tenancy and
    provenance come from the authenticated context, the version and the
    validity window from the database, and the change intent from beside the
    record. What is left is what a caller actually decides, which for a create
    includes the key.

    A key column is never stripped, even when its name is also a server-owned
    one. ``party_id`` is the acting party on most entities, but on a junction
    that links parties it is half the key, and which party a link names is the
    caller's to state; stripping it would emit a write that cannot say what it
    writes.

    Each field is shaped for the renderer, not handed back as the raw column:
    the message templates read ``name`` and ``cpp_type``, and a column dict
    passed through untouched would render an empty member.
    """
    fields: list[dict[str, Any]] = []
    for column in columns:
        name = _column_name(column)
        if name not in key_columns and (
                name in SERVER_OWNED_FIELDS or name in CHANGE_INTENT_FIELDS):
            continue
        field = _ts_field(name, column.get("cpp_type") or "std::string")
        # Where the domain type holds the member. A composed entity reaches
        # its fields through a group member, so the record's own member name
        # and the domain's access path are not the same string; the service
        # that builds a domain object from a record reads this.
        field["domain_member"] = (column.get("group_prefix") or "") + name
        fields.append(field)
    return fields


def _key_cpp_type(column: dict[str, Any]) -> str:
    """A key column's own type, not a string it happens to be printable as.

    The type is read from the column where the model states it, and falls back
    to the uuid flag for a column dict built by hand, as the tests build one.
    """
    return (column.get("cpp_type")
            or ("boost::uuids::uuid" if column.get("is_uuid") else "std::string"))


def write_record_columns(entity: dict[str, Any]) -> list[dict[str, Any]]:
    """Every column a write record may name, in the order the model declares them.

    The loader partitions an entity's fields: the primary key is its own list,
    a unique business identifier is a natural key, and the rest are columns.
    A write record has to carry all three, because a create states its key --
    which is the primary key for a surrogate-keyed entity and the natural key
    for a lookup one -- and a record built from ``columns`` alone would leave a
    create unable to say what it creates.
    """
    primary_key = entity.get("primary_key") or {}
    declared = (list(primary_key.get("columns") or [])
                + list(entity.get("natural_keys") or [])
                + list(entity.get("columns") or []))
    by_name: dict[str, dict[str, Any]] = {}
    for column in declared:
        by_name.setdefault(_column_name(column), column)
    return list(by_name.values())


def write_record_for(entity: dict[str, Any]) -> list[dict[str, Any]]:
    """The write record's fields for one entity, derived once for every reader.

    The protocol twin renders these as the record's members and the service
    builds a domain object from them, so a field the record carries and a
    field the service sets cannot disagree.
    """
    key_columns = frozenset(
        _column_name(column)
        for column in (entity.get("primary_key") or {}).get("columns") or [])
    return write_record_fields(write_record_columns(entity), key_columns)


def declared_key_field(entity: dict[str, Any]) -> str:
    """The field the model declares as the entity's key on the wire.

    The specification states that which key identifies an entity on the wire is
    a declaration of the model, made once, and that it is the same key that
    appears in the resource's operations and in the HTTP projection's path
    parameter, so a caller never translates between an address and a request.
    Reading it here is what makes the operation and the path agree.

    The declaration is the model's own ``key_field``. It used to sit in the
    presentation drawer alone; it is read from the model root first, so a model
    can drop a drawer it no longer needs without losing its key.

    Empty when the model declares no key, which is a model whose rows are
    addressed by the storage key. Its storage key is then the only key it has.
    """
    return (entity.get("key_field")
            or (entity.get("presentation") or {}).get("key_field") or "")


def declared_key_column(entity: dict[str, Any]) -> dict[str, Any] | None:
    """The declared key's own column dict, from wherever the model states it.

    A declared key may be the primary key, a natural key or a plain column, and
    the three are separate lists. Which one holds it does not matter to a
    caller; the type it is written in does, so the search covers all three.
    """
    name = declared_key_field(entity)
    if not name:
        return None
    primary_key = entity.get("primary_key") or {}
    for column in (list(primary_key.get("columns") or [])
                   + list(entity.get("natural_keys") or [])
                   + list(entity.get("columns") or [])):
        if _column_name(column) == name:
            return column
    return None


def key_is_primary(entity: dict[str, Any]) -> bool:
    """Whether the declared key is the storage key as well.

    When the two agree nothing needs translating and the generated reads
    already address the row. When they differ the model holds two keys -- a
    natural one callers use and a surrogate the store keeps for foreign-key
    stability -- so a read by the declared key has to exist for the address a
    caller holds to resolve to a row.
    """
    name = declared_key_field(entity)
    if not name:
        return True
    primary_key = entity.get("primary_key") or {}
    columns = [_column_name(column)
               for column in primary_key.get("columns") or []]
    return columns == [name]


def key_finders(entity: dict[str, Any]) -> list[dict[str, Any]]:
    """Every read by something other than the storage key, as method suffixes.

    Two things ask for one. ``:service_find_by_code_column:`` is the older
    opt-in and always names its method ``read_latest_by_code``, whatever column
    it reads -- the name is load-bearing, because hand-written callers
    elsewhere spell it, so it is left exactly as it was. The
    declared key asks for one whenever it is not the storage key, and names its
    method after the column it reads.

    When both name the same column the two are one method and it is stated
    once, which is what keeps an entity that already opted in byte-identical.
    """
    finders: list[dict[str, Any]] = []
    legacy = entity.get("service_find_by_code") or {}
    if legacy.get("column"):
        finder: dict[str, Any] = {"column": legacy["column"], "suffix": "code"}
        if legacy.get("parent_column"):
            finder["parent_column"] = legacy["parent_column"]
        finders.append(finder)
    declared = declared_key_field(entity)
    if declared and not key_is_primary(entity):
        if not any(f["column"] == declared for f in finders):
            # A model may already state this read itself, as a paste block, and
            # emitting a second declaration of the same method is a redefinition
            # rather than an addition. The model's own is kept: it is the one a
            # human wrote, and it may read more than the column's value.
            stated = "\n".join(
                code
                for codes in (entity.get("implementations") or {}).values()
                for code in codes)
            if f"read_latest_by_{declared}" not in stated:
                finders.append({"column": declared, "suffix": declared})
    return finders


def key_resolvers(entity: dict[str, Any]) -> list[dict[str, Any]]:
    """Reads that turn a declared key into a storage key, ignoring the window.

    History is addressed by the key the model declares and has to stay readable
    after a delete, which for a temporal entity closes the transaction-time
    window instead of removing the version rows. A latest read cannot resolve a
    closed row, so resolution needs its own read that ignores the window and
    takes the newest match.

    Deliberately independent of ``key_finders``. That list drops a finder the
    model states itself, because emitting a second declaration of the same
    method would be a redefinition. There is no such clash here: a model that
    hand-writes ``read_latest_by_username`` has said nothing about
    ``read_any_by_username``, and the resolver still has to exist for the
    service to call. Keeping the two lists apart is what stops a hand-written
    finder from silently leaving history unreadable.
    """
    declared = declared_key_field(entity)
    if declared and not key_is_primary(entity):
        return [{"column": declared, "suffix": declared}]
    return []


def key_record_fields(entity: dict[str, Any]) -> list[dict[str, Any]]:
    """The typed key that addresses one entity -- the key the model declares.

    The specification is explicit that a key carries each column with the
    column's own type, and that a composite key is never flattened to one
    string and never partially sent, because a partial key addresses a row
    that need not exist.

    It is equally explicit that which key identifies an entity on the wire is a
    declaration of the model, made once, and that the same key appears in the
    resource's operations and in the HTTP path. That declaration is the
    presentation drawer's ``key_field``, which is the field the path segment
    already carries. Building the key record from the storage key instead is
    what gave ``role``, ``tenant`` and ``permission`` two identities -- one the
    path used and one the request did.

    A model that declares no key has no other, so its storage key is the key.
    """
    declared = declared_key_column(entity)
    if declared is not None:
        return [_ts_field(declared_key_field(entity), _key_cpp_type(declared))]
    primary_key = entity.get("primary_key") or {}
    return [_ts_field(_column_name(column), _key_cpp_type(column))
            for column in primary_key.get("columns") or []]


# The shared records every entity's messages are built from. They live in
# ``ores.utility::domain`` because they are component-independent: a generated
# ``*.api`` protocol header may not depend on the service layer, so an entity's
# own result type could not be shared this way.
_RESULT = "ores::utility::domain::result"
_PRECONDITION = "ores::utility::domain::precondition"
_INTENT = "ores::utility::domain::change_intent"
_ORDER = "ores::utility::domain::order"
_SCOPE = "ores::utility::domain::scope"

# The operations that change state, and the auxiliary records only they carry.
# A model whose surface has no client-facing writes derives the reads alone, and
# a write record nothing refers to is dead surface rather than a message.
_WRITE_OPERATION_PREFIXES = ("put_", "put_many_", "delete_", "delete_many_")
_WRITE_ONLY_RECORD_SUFFIXES = ("_write", "_change", "_removal")

# The announcement record. An entity owns one because an event registrar is
# emitted for it, which publishes the record on three action subjects. No event
# registrar is emitted for a junction, so a junction's announcement is a record
# no code path reaches and no subject carries.
_ANNOUNCEMENT_RECORD_SUFFIXES = ("_event",)


def _column_cpp_type(entity: dict[str, Any], name: str) -> str:
    """One named column's own C++ type, or a string when the model is silent.

    The loader partitions an entity's fields, so a relation stated by a scoped
    read is often a natural key rather than a plain column -- a foreign key
    that identifies the child within its parent is exactly that. Searching
    ``columns`` alone would type such a relation ``std::string`` while the
    write record types the same column from its own dict, and the two would
    disagree about one column.
    """
    for column in write_record_columns(entity):
        if _column_name(column) == name:
            return column.get("cpp_type") or "std::string"
    return "std::string"


def _parent_id_field(entity: dict[str, Any]) -> str:
    """The column pointing at an entity's parent, when it declares one."""
    if not entity.get("has_parent_id"):
        return ""
    presentation = entity.get("presentation") or {}
    return (entity.get("parent_id_field")
            or presentation.get("parent_id_field")
            or presentation.get("parent_key_field") or "")


def _relation_columns(entity: dict[str, Any]) -> list[str]:
    """The columns a read may be scoped by, in the order the model declares them.

    A scoped read addresses one related entity, so every foreign key the model
    already reads by is a relation. The parent relation is one of these and not
    a case of its own: reading a node's children and reading its subtree are one
    verb, and ``scope`` says which.
    """
    names = [extra["filter_column"]
             for extra in entity.get("extra_list_requests") or []
             if extra.get("filter_column")]
    parent = _parent_id_field(entity)
    if parent and parent not in names:
        names.append(parent)
    return names


def filter_record_fields(entity: dict[str, Any]) -> list[dict[str, Any]]:
    """An entity's filter record: one optional member per filterable column.

    The specification makes filtering a record rather than a query language, so
    every member is optional and carries the field's own type. Which fields are
    filterable is the model's own statement: a column is filterable when the
    model already reads by it, which is the list filter column and every foreign
    key a scoped read is declared for. An empty result means the resource
    supports no filtering, and then it has no filter record at all.
    """
    names = ([entity["list_filter_column"]]
             if entity.get("list_filter_column") else []) + _relation_columns(entity)
    return [_ts_field(name,
                      f"std::optional<{_column_cpp_type(entity, name)}>")
            for name in dict.fromkeys(names)]


def versions_filter_fields() -> list[dict[str, Any]]:
    """The version axis as a filter, rather than a second way to select.

    An exact version and the two bounds, each optional and each typed, so that
    reading the entity as it stood at version 7 and reading the last month of
    changes need no operation of their own.
    """
    return [
        _ts_field("version", "std::optional<std::uint32_t>"),
        _ts_field("from_version", "std::optional<std::uint32_t>"),
        _ts_field("to_version", "std::optional<std::uint32_t>"),
    ]


def paged_list_messages(
    name: str,
    subject: str,
    leading: list[dict[str, Any]],
    filter_field: dict[str, Any] | None,
    collection: str,
    collection_type: str,
    verb: str | None = None,
) -> list[dict[str, Any]]:
    """The request and response pair every paged list shares.

    ``name`` is the message stem, ``leading`` the fields that say what the list
    covers -- nothing for an unscoped list, the relation for a scoped one, the
    key for a versions list. Offset, limit and total are unconditional in the
    specification, so they are stated here once instead of per entity, and the
    filter comes last, after the page it narrows. ``verb`` is passed through
    for a list whose name does not state which sub-resource it reads.
    """
    fields = list(leading) + [
        _ts_field("offset", "std::uint32_t", default="0"),
        _ts_field("limit", "std::uint32_t", default="100"),
        _ts_field("order", _ORDER),
    ]
    if filter_field:
        fields.append(filter_field)
    return [
        _ts_message(f"{name}_request", response_type=f"{name}_response",
                    subject=subject, fields=fields, verb=verb),
        _ts_message(f"{name}_response", fields=[
            _ts_field("result", _RESULT),
            _ts_field(collection, f"std::vector<{collection_type}>"),
            _ts_field("total", "std::uint64_t")]),
    ]


@lru_cache(maxsize=None)
def _entity_singulars_in(model_dir: str) -> frozenset[str]:
    """The entity singulars declared in one modeling directory, read once."""
    singulars: set[str] = set()
    for path in sorted(Path(model_dir).glob("*.org")):
        doc = parse_org(path.read_text(encoding="utf-8"))
        if doc.frontmatter.get("type") != "ores.codegen.entity":
            continue
        singular = (doc.frontmatter.get("entity_singular") or "").strip()
        if singular:
            singulars.add(singular)
    return frozenset(singulars)


def sibling_entity_singulars(model_path: Any) -> frozenset[str]:
    """The entity singulars modelled beside ``model_path``, its siblings.

    A model's version facet is named after the entity's singular, so an
    entity and an entity named ``<that singular>_version`` in the same
    component derive the same type, service and handler names. The facet
    reads this set so it can step aside; see ``entity_protocol_messages``.
    """
    if not model_path:
        return frozenset()
    return _entity_singulars_in(str(Path(model_path).resolve().parent))


def response_payload_member(entity: dict[str, Any]) -> str:
    """The member a response carries its payload in, named after the entity.

    The envelope states its outcome in a member named ``result``, so an entity
    of that name gives its payload a suffixed member rather than sharing the
    one the envelope already uses. The protocol header, the service body and
    the TypeScript twin all read this, so the three cannot disagree.
    """
    singular = entity.get("entity_singular", "")
    return "result_value" if singular == "result" else singular


def entity_protocol_messages(
        entity: dict[str, Any],
        sibling_singulars: frozenset[str] | None = None) -> list[dict[str, Any]]:
    """Derive an entity's canonical message list, as the specification states it.

    The list is the one model both protocol twins render: the C++
    ``cpp_protocol.hpp.mustache`` and the TypeScript ``ts_protocol.ts.mustache``
    each walk ``{{#messages}}``, so a message added here appears in both or in
    neither. An entry with a ``subject`` is an operation a caller addresses; an
    entry without one is an auxiliary record -- a key, a write record, a change,
    a removal, a lookup, a version key, a filter -- which the renderer writes as
    a plain struct.

    Must be called on the enriched entity, after ``core.generate_from_model``
    has hoisted the repository's ``entity_plural_short`` and derived
    ``extra_list_requests``, ``primary_key.columns`` and the messaging flags.
    Messages the model declares itself are appended, so both twins carry them
    from one section.

    A ``current_state`` entity derives no versions sub-resource: it has no
    valid_from/valid_to axis, so it has no version to read.

    Two names are chosen to avoid a collision rather than by derivation
    alone. The versions sub-resource is named after the singular, so a
    sibling entity named ``<singular>_version`` owns every one of its
    names; the facet then takes the plural as its stem and the sibling
    keeps what its own model states. The response envelope states its
    outcome in a member named ``result``, so an entity of that name gives
    its payload a suffixed member instead. ``sibling_singulars`` is the
    set from ``sibling_entity_singulars``; without it the singular is used
    as before.
    """
    component = entity.get("component", "")
    singular = entity.get("entity_singular", "")
    plural = entity.get("entity_plural", singular + "s")
    plural_short = entity.get("entity_plural_short") or plural
    domain_type = f"ores::{component}::domain::{singular}"
    key = f"{singular}_key"
    facet_stem = (
        plural if f"{singular}_version" in (sibling_singulars or ()) else singular)
    payload_member = response_payload_member(entity)
    key_columns = frozenset(
        _column_name(column)
        for column in (entity.get("primary_key") or {}).get("columns") or [])

    filter_fields = filter_record_fields(entity)

    # The auxiliary records come first, because a C++ message names the records
    # it carries and a type is declared before it is used. None of them has a
    # subject: a record is a shape, an operation is something a caller sends.
    messages = [
        _ts_message(key, fields=key_record_fields(entity)),
        _ts_message(f"{singular}_write", fields=write_record_fields(
            write_record_columns(entity), key_columns)),
        _ts_message(f"{singular}_change", fields=[
            _ts_field("write", f"{singular}_write"),
            _ts_field("precondition", _PRECONDITION)]),
        _ts_message(f"{singular}_removal", fields=[
            _ts_field("key", key),
            _ts_field("precondition", _PRECONDITION,
                      default="ores::utility::domain::removal_precondition")]),
        _ts_message(f"{singular}_lookup", fields=[
            _ts_field("key", key),
            _ts_field(singular, f"std::optional<{domain_type}>")]),
    ]
    if filter_fields:
        messages.append(_ts_message(f"{plural}_filter", fields=filter_fields))
    # The announcement. One payload carries what happened to one row, and the
    # subject's last segment says which action it reports, so the payload is
    # stated once and the three subjects alongside it.
    messages.append(_ts_message(f"{singular}_event", fields=[
        _ts_field("event_id", "boost::uuids::uuid"),
        _ts_field("key", key),
        _ts_field("action", "std::string"),
        _ts_field("version", "std::uint32_t"),
        _ts_field("occurred_at", "std::chrono::system_clock::time_point"),
        _ts_field("correlation_id", "std::optional<std::string>"),
    ]))
    if entity.get("has_audit_columns"):
        # A versioned entity's version is addressed by the entity's own key plus
        # the version number, so the pair is a record of its own. An entity with
        # no version column has no version to address -- a current-state table
        # keeps no history at all, and a table the model marked
        # ``no_audit_columns`` keeps a validity window with no version in it.
        # The gate is the version column, not the current-state flag, because
        # the two are not the same claim.
        messages.append(_ts_message(f"{facet_stem}_version_key", fields=[
            _ts_field(singular, key),
            _ts_field("version", "std::uint32_t")]))
        messages.append(_ts_message(f"{facet_stem}_versions_filter",
                                    fields=versions_filter_fields()))

    list_filter = (_ts_field("filter", f"std::optional<{plural}_filter>")
                   if filter_fields else None)

    plain_list = paged_list_messages(
        f"list_{plural}", request_subject(component, plural, "list"),
        [], list_filter, plural_short, domain_type)
    if entity.get("has_as_of_lookup"):
        # A stated instant resolves the row's own validity window rather than
        # naming a version, so the list carries it beside the page it narrows.
        plain_list[0]["fields"].append(
            _ts_field("as_of", "std::optional<std::string>"))
    messages += plain_list

    messages += [
        _ts_message(f"get_{singular}_request",
                    response_type=f"get_{singular}_response",
                    subject=request_subject(component, plural, "get"),
                    fields=[_ts_field("key", key)]),
        _ts_message(f"get_{singular}_response", fields=[
            _ts_field("result", _RESULT),
            _ts_field(payload_member, f"std::optional<{domain_type}>")]),
        _ts_message(f"get_many_{plural}_request",
                    response_type=f"get_many_{plural}_response",
                    subject=request_subject(component, plural, "get_many"),
                    fields=[_ts_field("keys", f"std::vector<{key}>")]),
        _ts_message(f"get_many_{plural}_response", fields=[
            _ts_field("result", _RESULT),
            _ts_field("entries", f"std::vector<{singular}_lookup>")]),
        _ts_message(f"put_{singular}_request",
                    response_type=f"put_{singular}_response",
                    subject=request_subject(component, plural, "put"),
                    fields=[_ts_field("change", f"{singular}_change"),
                            _ts_field("intent", _INTENT)]),
        _ts_message(f"put_{singular}_response", fields=[
            _ts_field("result", _RESULT),
            _ts_field(payload_member, domain_type)]),
        _ts_message(f"put_many_{plural}_request",
                    response_type=f"put_many_{plural}_response",
                    subject=request_subject(component, plural, "put_many"),
                    fields=[_ts_field("changes",
                                      f"std::vector<{singular}_change>"),
                            _ts_field("intent", _INTENT)]),
        _ts_message(f"put_many_{plural}_response", fields=[
            _ts_field("result", _RESULT),
            _ts_field(plural_short, f"std::vector<{domain_type}>")]),
        _ts_message(f"delete_{singular}_request",
                    response_type=f"delete_{singular}_response",
                    subject=request_subject(component, plural, "delete"),
                    fields=[_ts_field("removal", f"{singular}_removal"),
                            _ts_field("intent", _INTENT)]),
        _ts_message(f"delete_{singular}_response",
                    fields=[_ts_field("result", _RESULT)]),
        _ts_message(f"delete_many_{plural}_request",
                    response_type=f"delete_many_{plural}_response",
                    subject=request_subject(component, plural, "delete_many"),
                    fields=[_ts_field("removals",
                                      f"std::vector<{singular}_removal>"),
                            _ts_field("intent", _INTENT)]),
        _ts_message(f"delete_many_{plural}_response",
                    fields=[_ts_field("result", _RESULT)]),
    ]

    # A scoped read is the plain list with the relation in its addressing, so it
    # shares the page, the order, the total and the response shape. The column
    # names both the subject suffix and the field, so the two cannot disagree.
    # ``scope`` is what makes one verb serve both readings: a node's children,
    # and everything beneath it.
    for relation in _relation_columns(entity):
        messages += paged_list_messages(
            f"list_by_{relation}_{plural}",
            request_subject(component, plural, f"list_by_{relation}"),
            [_ts_field(relation, _column_cpp_type(entity, relation)),
             _ts_field("scope", _SCOPE,
                       default="ores::utility::domain::scope::direct")],
            list_filter, plural_short, domain_type)

    if entity.get("has_audit_columns"):
        messages += paged_list_messages(
            f"list_{facet_stem}_versions",
            versions_subject(component, plural, "list"),
            [_ts_field("key", key)],
            _ts_field("filter", f"std::optional<{facet_stem}_versions_filter>"),
            "versions", domain_type, verb="list_versions")
        messages += [
            _ts_message(f"get_{facet_stem}_version_request",
                        response_type=f"get_{facet_stem}_version_response",
                        subject=versions_subject(component, plural, "get"),
                        verb="get_version",
                        fields=[_ts_field("key", f"{facet_stem}_version_key")]),
            _ts_message(f"get_{facet_stem}_version_response", fields=[
                _ts_field("result", _RESULT),
                _ts_field("version", domain_type)]),
        ]

    # Messages the model declares itself, beside the derived set, so both twins
    # come from this one section.
    messages += entity.get("declared_messages") or []

    # Whether a caller must have established a session first. Every derived
    # operation acts on a logged-in caller, so the derived set states true; a
    # message the model declares may state otherwise in its own drawer. The
    # value is spelled for the renderer: both protocol twins emit it into C++
    # and TypeScript, where a Python bool is not a literal.
    for message in messages:
        if message.get("subject"):
            message.setdefault("requires_session", "true")

    # A read-only entity derives its reads and no writes. The write verbs are
    # then operations the model states itself, which is what a row write cannot
    # express: an account's password is hashed, its lockout counted and its TOTP
    # secret minted, and none of those is a column a client may set.
    if entity.get("read_only") or entity.get("client_read_only"):
        messages = [
            message for message in messages
            if not message["name"].startswith(_WRITE_OPERATION_PREFIXES)
            and not message["name"].endswith(_WRITE_ONLY_RECORD_SUFFIXES)
        ]

    return messages


def junction_entity_shape(junction: dict[str, Any]) -> dict[str, Any]:
    """A junction projected onto the entity shape every render reads.

    A junction is an entity whose key spans the two sides it links, so the
    protocol derivation and the shell projection address it as one. The shape
    is built here once rather than by each of them, because a junction that
    gained a side would otherwise gain it in the protocol and not in the
    shell -- the same drift a raw subject literal causes, one level up.

    The key carries the whole pair and never half of it, which is what makes
    addressing one link unambiguous. A junction links rows and carries no
    ``valid_from``/``valid_to`` axis, so it has no versions sub-resource; the
    plural is the junction's own name, because a table that links accounts to
    parties is named for the links and not for one of them.

    The caller adds ``messages``, ``operations``, ``write_fields`` and
    ``shell`` on top: those are derivations of this shape, not members of it.
    """
    name = junction.get("name", "")
    sides = (junction.get("left") or {}, junction.get("right") or {})
    return {
        "component": junction.get("component", ""),
        "entity_singular": junction.get("name_singular", ""),
        "entity_plural": name,
        "entity_plural_short": name,
        "current_state": True,
        "primary_key": {
            "columns": [{"column": side.get("column", ""),
                         "cpp_type": side.get("cpp_type") or "std::string",
                         "is_uuid": side.get("type") == "uuid"}
                        for side in sides],
        },
        "columns": [{"column": side.get("column", ""),
                     "cpp_type": side.get("cpp_type") or "std::string"}
                    for side in sides]
        + [{"column": _column_name(column),
            "cpp_type": column.get("cpp_type") or "std::string"}
           for column in junction.get("columns") or []],
        "extra_list_requests": [
            {"filter_column": side["column"],
             "nats_suffix": f"list_by_{side['column']}"}
            for side in sides if side.get("list_by")],
    }


def junction_protocol_messages(junction: dict[str, Any]) -> list[dict[str, Any]]:
    """Derive a junction's message list: the entity set, keyed by both sides.

    A junction is an entity whose key spans the two sides it links, so it
    declares the same verbs as any other resource and addresses one link by
    both columns at once. Its key record carries the whole pair and never half
    of it, which is what makes addressing one link unambiguous.

    A by-side read is a scoped read of the same collection: each side the model
    opted into ``:list_by:=`` contributes ``list_by_<column>``, which is the
    plain list with that side in its addressing. The reply is a page of link
    rows, as any scoped read replies, so a caller resolves the codes it wants
    to display with ``get_many`` rather than through a payload type of its own.

    A junction with no client-facing write surface derives the reads only. The
    wire has no read-only flag -- authorisation is what refuses a write -- but a
    model that states the surface has no writes should not emit messages for
    operations that cannot happen.

    Derived by projecting the junction onto the entity shape and delegating, so
    a change to the entity protocol reaches junctions in the same commit rather
    than being mirrored here by hand.
    """
    entity = junction_entity_shape(junction)
    messages = entity_protocol_messages(entity)
    # A junction announces nothing, so it drops the entity derivation's
    # announcement record rather than declaring a wire type nothing serves.
    messages = [message for message in messages
                if not message["name"].endswith(_ANNOUNCEMENT_RECORD_SUFFIXES)]
    # ``wire_write_enabled`` folds the repository's read_only together with the
    # client-only switch; the fallback keeps a hand-built junction dict, as the
    # tests use, on the read_only rule.
    if junction.get(
            "wire_write_enabled",
            not (junction.get("read_only") or junction.get("client_read_only"))):
        return messages
    return [message for message in messages
            if not message["name"].startswith(_WRITE_OPERATION_PREFIXES)
            and not message["name"].endswith(_WRITE_ONLY_RECORD_SUFFIXES)]


# Which verb a derived request states, from the request's own name. A request
# that answers a sub-resource states its verb where it is built instead, because
# its name cannot carry it: an entity whose plural ends in "_versions" names its
# own list the way the versions sub-resource names its own.
_OPERATION_PREFIXES = (
    ("list_by_", "list_scoped"),
    ("put_many_", "put_many"),
    ("delete_many_", "delete_many"),
    ("get_many_", "get_many"),
    ("list_", "list"),
    ("get_", "get"),
    ("put_", "put"),
    ("delete_", "delete"),
)


def _operation_verb(name: str) -> str:
    """The verb one derived request states, or an empty string if none."""
    for prefix, verb in _OPERATION_PREFIXES:
        if name.startswith(prefix):
            return verb
    return ""


def protocol_operations(messages: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """The operations a resource addresses, in the order the messages state them.

    A message with a subject is an operation; one without is a record. The
    method name is the request's own name without its ``_request`` suffix, so
    one rename moves the subject, the service method and the handler method
    together rather than three names kept in step by hand.

    Declared messages are left out: an operation model states its whole
    protocol and its handler beside it, so the derived surface a generated
    handler serves stops at what the derivation owns.
    """
    operations: list[dict[str, Any]] = []
    for message in messages:
        if not message.get("derived") or not message.get("subject"):
            continue
        fields = message.get("fields") or []
        names = [field.get("name") for field in fields]
        leading_type = fields[0].get("cpp_type", "") if fields else ""
        verb = message.get("verb") or _operation_verb(message["name"])
        operations.append({
            "method": message["name"][:-len("_request")],
            "request": message["name"],
            "response": message.get("response_type", ""),
            "subject": message["subject"],
            "verb": verb,
            "fields": fields,
            # What the request carries, so a service body states the page, the
            # order and the filter it was handed rather than assuming them.
            "has_order": "order" in names,
            "has_filter": "filter" in names,
            "has_scope": "scope" in names,
            # The field a paged list is scoped by: the relation for a scoped
            # read, the key for a versions read, nothing for a plain list.
            "leading": names[0] if names else "",
            # The repository takes a relation as text whatever the column is,
            # so a service body that passes one states the conversion from the
            # column's own type.
            "leading_is_uuid": "boost::uuids::uuid" in leading_type,
            "leading_is_timestamp": "time_point" in leading_type,
            # A relation the column admits as null is optional on the wire, so
            # a scoped read has to say what it does when the request omits it.
            "leading_is_optional": "optional" in leading_type,
            # A write is an operation that changes state, and the permission
            # it needs is the one the resource already names for that kind of
            # change. A read needs authentication alone, so it names none.
            "is_write": verb in ("put", "put_many", "delete", "delete_many"),
            # The single write verb has one hook the other verbs do not: a
            # component may intercept its own save before authentication, for
            # an orchestration command that carries its context in headers
            # rather than in a token. ores.refdata's party is the one model
            # that does, through an implementation block.
            "is_put_one": verb == "put",
            "permission": ("delete" if verb in ("delete", "delete_many")
                           else "write" if verb in ("put", "put_many") else ""),
        })
    return operations


def operations_by_verb(
        operations: list[dict[str, Any]]) -> dict[str, list[dict[str, Any]]]:
    """The same operations grouped by verb, for a template with a body per verb.

    A handler method is uniform whatever the verb, because the service answers
    the request; a service method is not, because the storage call it makes
    depends on what the verb asks for. Grouping here keeps the branch in the
    template instead of in the C++.
    """
    grouped: dict[str, list[dict[str, Any]]] = {}
    for operation in operations:
        grouped.setdefault(operation["verb"], []).append(operation)
    return grouped


def parse_declared_messages(root: "OrgNode") -> list[dict[str, Any]]:
    """Read a ``* Messages`` section into the message shape both twins render.

    The grammar is one ``**`` child per message, ``:subject:`` and
    ``:response:`` in its drawer, an optional ``#+begin_src cpp :name
    comment`` doc comment, and one ``***`` child per field carrying
    ``:cpp_type:`` and an optional ``:default:``. An operation model uses
    the section for its whole protocol; an entity model uses it to declare
    messages beside its derived CRUD set, which the C++ header renders at
    its paste point and the TypeScript twin renders from the same list.
    """
    messages: list[dict[str, Any]] = []
    section = _section(root, "Messages")
    if not section:
        return messages
    for node in section.children:
        entry: dict[str, Any] = {
            "name": node.title,
            "name_pascal": _to_pascal_case(node.title),
        }
        props = {k.lower(): v for k, v in node.properties.items()}
        if "subject" in props:
            entry["subject"] = props["subject"]
        # The org names the response; the template emits the C++ alias, so
        # the model key takes the alias's own name.
        if "response" in props:
            entry["response_type"] = props["response"]
        # How a caller authenticates. An operation that establishes the session
        # cannot present one, so the model states it beside the subject rather
        # than leaving each client to guess from the name. The value is spelled
        # for the renderer because both protocol twins emit it as a literal.
        auth = str(props.get("auth", "")).strip().lower()
        if auth not in ("", "none"):
            raise ValueError(
                f"message {node.title} states :auth: {props['auth']!r}; the "
                "only value is 'none', which marks an operation a caller runs "
                "before it has a session"
            )
        if "subject" in props:
            entry["requires_session"] = "false" if auth == "none" else "true"
        # A command that destroys the environment it runs in cannot be replayed
        # unattended, and nothing about its shape says so: `reset-system` looks
        # like any other zero-argument operation. The model states it, so a
        # generated script carries the warning and a runner can refuse it
        # rather than discover it by running it.
        destructive = str(props.get("destructive", "")).strip().lower()
        if destructive:
            if destructive not in ("true", "yes", "1"):
                raise ValueError(
                    f"message {node.title} states :destructive: "
                    f"{props['destructive']!r}; the values that mean yes are "
                    "'true', 'yes' and '1'"
                )
            entry["destructive"] = True
        comment = node.src_blocks.get("comment")
        if comment:
            entry["comment"] = comment

        fields: list[dict[str, Any]] = []
        for field_node in node.children:
            field_entry: dict[str, Any] = {"name": field_node.title}
            field_props = {k.lower(): v for k, v in field_node.properties.items()}
            if "cpp_type" in field_props:
                field_entry["cpp_type"] = field_props["cpp_type"]
                mapped = _ts_type(field_props["cpp_type"])
                if mapped:
                    field_entry["ts_type"] = mapped
            if "default" in field_props:
                field_entry["default"] = field_props["default"]
            field_comment = field_node.src_blocks.get("comment")
            # Always set the key: Mustache resolves a name it cannot find by
            # walking up the context stack, so an absent ``comment`` would
            # inherit the enclosing message's.
            field_entry["comment"] = (
                _indent_block(field_comment, 4) if field_comment else ""
            )
            fields.append(field_entry)
        entry["fields"] = fields
        messages.append(entry)
    return messages


# The segment a derived subject leaves to its caller. A cross-component request
# is addressed to the component that OWNS the entity, which the model cannot
# know: one history declaration reaches iam.v1.history.get for an IAM entity and
# refdata.v1.history.get for a refdata one. So the model states the pattern and
# codegen emits the rule, rather than every caller composing the subject itself
# and no two of them being checked against each other.
_DERIVED_SUBJECT_HOLE = "{component}"


def derived_subject(messages: list[dict[str, Any]],
                    entity_singular: str) -> dict[str, Any]:
    """The subject rule a model states with a hole in it, if it states one.

    One function per model rather than one per message: a model that derived two
    different subjects from the same key would be stating two rules where its
    caller has one, so a second pattern is refused rather than emitted.

    The hole is the canonical component segment -- iam.v1.history.get is
    component iam, resource history, verb get -- so the pattern is the canonical
    subject grammar with the one segment the model cannot fill left open.
    """
    patterns = {
        message["subject"] for message in messages
        if _DERIVED_SUBJECT_HOLE in (message.get("subject") or "")
    }
    if not patterns:
        return {}
    if len(patterns) > 1:
        raise ValueError(
            f"{entity_singular}: more than one derived subject "
            f"({', '.join(sorted(patterns))}); a model states one rule, so that "
            "the subject is determined in one place")
    head, _, tail = patterns.pop().partition(_DERIVED_SUBJECT_HOLE)
    words = entity_singular.split("_")
    return {
        "derived_subject": True,
        "derived_subject_prefix": head,
        "derived_subject_suffix": tail,
        # Two arities of one rule. A client holds the dispatch key of the
        # resource it is asking about; a service subscribing knows only its own
        # component. Both are generated from the one pattern, so neither has to
        # spell the subject itself.
        "subject_function": f"{entity_singular}_subject_for",
        "subject_function_by_component": (
            f"{entity_singular}_subject_by_component"),
        # The TypeScript twin spells the same function the way its own language
        # does, so the two say the same thing rather than sharing a spelling.
        "subject_function_ts": (
            words[0] + "".join(word.capitalize() for word in words[1:])
            + "SubjectFor"),
    }


def load_org_operation_model(path: Path | str) -> dict[str, Any]:
    """Load an org-mode protocol-operation model.

    Produces an ``{"operation": {...}}`` dict rendered by
    ``cpp_protocol.hpp.mustache``'s ``{{#operation}}`` block: frontmatter
    scalars (component, subcomponent, entity_singular, brief), the
    namespace, the ``includes`` list from the named ``includes`` babel
    block under ``* Includes``, and a ``messages`` list from the ``**``
    headings under ``* Messages``.

    A message carries two optional keys, and they are independent:

    - ``subject`` — the NATS subject. Present on an operation, absent on
      a plain payload struct such as ``party_summary``.
    - ``response_type`` — the response the request pairs with. Absent on
      ``public_key_request``, which has a subject and no response.

    Pairing is many-to-one: ``switch_party_request`` reuses
    ``select_party_response``, so the response is named on the request
    rather than nested under a shared operation node.

    A ``comment`` babel block documents the heading it sits under, so it
    goes directly below that heading's ``:PROPERTIES:`` drawer. Message
    comments stay flush; field comments are indented by four spaces (see
    ``_indent_block``).

    ``:default:`` stays a raw string: Mustache treats the number 0 as
    falsy, so a typed ``0`` would silently drop the initializer.

    Each message also carries ``name_pascal`` and each field a
    ``ts_type``, both derived for the TypeScript archetype, which shares
    this model. A field whose type has no projection carries no
    ``ts_type`` key at all, and ``_reject_silent_ts_gap`` decides whether
    that absence is fatal.
    """
    text = Path(path).read_text(encoding="utf-8")
    doc = parse_org(text)
    _ensure_profile_binding(doc)
    _reject_junction_only_flags(doc, "operation")
    fm = doc.frontmatter

    op: dict[str, Any] = {}
    for key in ("product", "component", "subcomponent",
                "entity_singular", "namespace", "brief"):
        if key in fm:
            op[key] = fm[key]

    body = _strip_body(doc.root)
    if body:
        op["description"] = body

    inc = _section(doc.root, "Includes")
    if inc:
        op["includes"] = _includes_from_named_block(inc)

    messages = parse_declared_messages(doc.root)
    for message in messages:
        if _DERIVED_SUBJECT_HOLE in (message.get("subject") or ""):
            message["subject_is_derived"] = True
    op["messages"] = messages
    # The subject rule the model states with a hole in it, emitted once for the
    # whole model so the subscriber and every client read it from one place.
    op.update(derived_subject(messages, op.get("entity_singular", "")))
    op["domain_imports"] = ts_domain_imports(messages)
    op["utility_imports"] = ts_utility_imports(messages)

    # The shell's view of the same list: one command per addressable message,
    # so the REPL surface and the protocol are one declaration.
    op["shell_commands"] = shell_command_projection(messages)
    # The unit-local helpers are emitted only when a command needs them, so an
    # unused static function is not compiled into every unit.
    op["shell_has_bool"] = any(
        field["is_bool"]
        for command in op["shell_commands"]
        for field in command["positionals"] + command["flags"]
    )
    op["shell_has_list"] = any(
        field["is_list"]
        for command in op["shell_commands"]
        for field in command["positionals"] + command["flags"]
    )
    # The anonymous namespace holds those two helpers, so it is emitted only
    # when one of them is.
    op["shell_has_helpers"] = bool(op["shell_has_bool"] or op["shell_has_list"])
    # Mustache cannot ask a list for its length, so the count the unit's own
    # test asserts is derived here.
    op["shell_command_count"] = len(op["shell_commands"])

    _reject_silent_ts_gap(path, messages, doc.file_properties)
    _reject_silent_shell_gap(path, op["shell_commands"], doc.file_properties)

    return {"operation": op}


def _reject_silent_ts_gap(
    path: Path | str, messages: list[dict[str, Any]], file_properties: dict[str, str]
) -> None:
    """Reject an operation that cannot render TypeScript without saying so.

    A field whose C++ type has no TypeScript projection renders an
    interface with the field missing, so a UI reading it fails at run
    time rather than at codegen. The model must either map the type or
    switch the facet off in its own drawer, which is what the five IAM
    protocols referencing ``ores::iam::domain::`` types do until a
    TypeScript domain layer exists.
    """
    unmapped = sorted({
        field["cpp_type"]
        for message in messages
        for field in message["fields"]
        if "cpp_type" in field and "ts_type" not in field
    })
    if not unmapped:
        return
    if str(file_properties.get("ores.ts.protocol.enabled", "")).strip().lower() in (
        "nil", "false", "no", "0"
    ):
        return
    raise ValueError(
        f"{Path(path).name}: no TypeScript projection for {unmapped}; map the "
        "type in org_loader._ts_type, or set ':ores.ts.protocol.enabled: nil' "
        "in the file's :PROPERTIES: drawer to skip the TypeScript facet"
    )


# The C++ types a generated shell command fills from one command-line token.
# A type outside this set has no token form, so the model must map it here or
# switch the facet off rather than render a unit that cannot compile.
_SHELL_TOKEN_TYPES = frozenset({
    "std::string",
    "bool",
    "int",
    "std::int32_t",
    "std::int64_t",
    "std::uint16_t",
    "std::uint32_t",
    "std::uint64_t",
    "double",
    "boost::uuids::uuid",
    # A timestamp and an address have a text form but no lexical_cast, so the
    # generated unit's read_token states their conversions itself. This set is
    # what those helpers fill, or a model is refused for a type they can read.
    "std::chrono::system_clock::time_point",
    "boost::asio::ip::address",
})
# The one container form the shell can fill: a comma-separated token.
_SHELL_LIST_TYPE = "std::vector<std::string>"
# The trailing words a message name carries that are not part of the action.
_SHELL_NAME_NOISE = frozenset({"request", "command", "typed"})


def shell_command_name(message_name: str) -> str:
    """The REPL command a declared message becomes.

    The trailing words name the artefact rather than the action, so they are
    dropped in whatever order the model wrote them: ``save_account_request``
    becomes ``save-account``, and ``get_accounts_request_typed`` becomes the
    same command as ``get_accounts_request``.
    """
    words = message_name.split("_")
    # The last word stands even when it is noise: a message named after the
    # artefact alone would otherwise give the menu an empty command.
    while len(words) > 1 and words[-1] in _SHELL_NAME_NOISE:
        words.pop()
    return "-".join(words)


def _shell_field(field: dict[str, Any]) -> dict[str, Any]:
    """One declared request field, as the shell unit asks for it.

    A field the model gave a ``:default:`` is optional, so it arrives as a
    ``--<name>`` flag and the struct's own initialiser stands when the caller
    omits it. A field without one is a positional argument.
    """
    cpp = (field.get("cpp_type") or "std::string").strip()
    is_list = cpp == _SHELL_LIST_TYPE
    return {
        "name": field["name"],
        "cpp_type": cpp,
        "default": field.get("default"),
        "is_optional": field.get("default") is not None,
        "is_list": is_list,
        "is_string": cpp == "std::string",
        "is_bool": cpp == "bool",
        "is_number": cpp in ("int", "std::uint32_t", "std::uint64_t"),
        "needs_from_token": not is_list and cpp not in ("std::string", "bool"),
        "fillable": is_list or cpp in _SHELL_TOKEN_TYPES,
    }


def shell_command_projection(messages: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """The shell commands a declared protocol yields, one per addressable message.

    A message is addressable when it states a subject and a response: the
    subject is what the command sends to and the response is what it prints, so
    a payload struct has neither and a request with no response answers with
    nothing the shell could show.

    ``unsupported`` lists the fields a token cannot fill. The renderer emits the
    command regardless, so an unfillable field fails the build rather than
    disappearing from the surface; :func:`_reject_silent_shell_gap` reports it
    when the model has opted in.

    ``public`` marks an operation a caller runs before it has a session --
    logging in, signing up, reading the signing key. Such a command presents no
    token and refuses none, because the caller has none to present. It is read
    from the message's own ``requires_session``, which is the same fact the
    protocol carries, so the two cannot drift.
    """
    commands: list[dict[str, Any]] = []
    for message in messages:
        subject = message.get("subject")
        response = message.get("response_type")
        if not subject or not response:
            continue
        # A derived subject is addressed to the component that owns the entity,
        # which a command cannot know: it would have to be handed the component
        # as an argument, and the shell's subject is a constant it sends to. So
        # the message is not addressable as a command, and a model that wants a
        # command for one states a fixed subject.
        if message.get("subject_is_derived"):
            continue
        public = message.get("requires_session") == "false"
        fields = [_shell_field(field) for field in message.get("fields") or []]
        positionals = [field for field in fields if not field["is_optional"]]
        flags = [field for field in fields if field["is_optional"]]
        command = shell_command_name(message["name"])
        commands.append({
            "command": command,
            "identifier": command.replace("-", "_"),
            "request": message["name"],
            "response_type": response,
            "subject": subject,
            "public": public,
            # Whether replaying the command destroys the environment it runs
            # in. The model states it, because the shape does not: a reset and
            # a status read are both a bare command name.
            "is_destructive": bool(message.get("destructive")),
            "positionals": positionals,
            "flags": flags,
            "positional_count": len(positionals),
            "has_positionals": bool(positionals),
            "has_flags": bool(flags),
            "usage": shell_command_usage(command, positionals, flags),
            "invocation": " ".join(
                [command]
                + [_sentinel_for_field(field["name"], field["cpp_type"])
                   for field in positionals]),
            "unsupported": [field["name"] for field in fields
                            if not field["fillable"]],
        })
    return commands


def shell_command_usage(command: str, positionals: list[dict[str, Any]],
                        flags: list[dict[str, Any]]) -> str:
    """The one-line help a generated command registers."""
    parts = [command]
    parts.extend(f"<{field['name']}>" for field in positionals)
    parts.extend(f"[--{field['name']} <v>]" for field in flags)
    return " ".join(parts)


def _reject_silent_shell_gap(
    path: Path | str, commands: list[dict[str, Any]],
    file_properties: dict[str, str],
) -> None:
    """Reject an opted-in model whose tokens cannot fill a declared field.

    The facet is opt-in, so a model that has not asked for a shell unit is left
    alone: the same projection serves every operation model and most of them
    render no unit at all.
    """
    unsupported = sorted({
        f"{command['command']}.{name}"
        for command in commands
        for name in command["unsupported"]
    })
    if not unsupported:
        return
    enabled = str(
        file_properties.get("ores.cpp.shell-command.enabled", "")
    ).strip().lower()
    if enabled not in ("true", "yes", "1"):
        return
    raise ValueError(
        f"{Path(path).name}: no shell token form for {unsupported}; map the "
        "type in org_loader._SHELL_TOKEN_TYPES, or set "
        "':ores.cpp.shell-command.enabled: nil' in the file's :PROPERTIES: "
        "drawer to skip the shell facet"
    )


def shell_menu_name(model_type: str, model_data: dict[str, Any]) -> str:
    """The REPL submenu a model's generated shell unit registers.

    One rule with two readers: the output-path resolver places a document under
    the menu's directory and the renderer states the menu in the document's
    filetag, so a menu computed twice would file a document under one name and
    tag it with another.
    """
    if model_type == "operation":
        return (model_data.get("operation") or {}).get("entity_singular", "")
    if model_type == "junction":
        # A junction states its plural as ``name``: the table that links
        # accounts to parties is named for the links, not for either side.
        junction = model_data.get("junction") or {}
        return junction.get("entity_plural") or junction.get("name") or ""
    entity = model_data.get("domain_entity") or {}
    return entity.get("entity_plural") or entity.get("entity_singular") or ""


# The namespace generated document ids are minted in. A constant, not a fresh
# random id per run: org-roam links documents by id, so a regenerated recipe
# that changed its id would orphan every reference to it. Deriving the id from
# the document's own name instead keeps it stable across runs and unique across
# documents.
_RECIPE_ID_NAMESPACE = uuid.UUID("6F1D2C3A-7E4B-4C58-9A21-8D3F5B7C0E64")


def recipe_org_id(name: str) -> str:
    """The stable org-roam id for a generated document called ``name``."""
    return str(uuid.uuid5(_RECIPE_ID_NAMESPACE, name)).upper()


# What a shell verb asks of the store, in the words a recipe uses. Keyed by the
# verb the protocol states rather than by the command name, because two commands
# may serve one verb -- a put becomes add and set -- and what differs between
# them is the precondition, which is stated separately below.
_SHELL_VERB_SENTENCE = {
    "list": "Reads one page of the collection. Nothing addresses it but the "
            "caller's tenant, so it is the read that shows what exists.",
    "get": "Reads the one row its key addresses. A key states every "
           "identifying column, so a partial key is refused rather than "
           "answered with an arbitrary row.",
    "get_many": "Reads several rows in one request, one key group per row.",
    "put": "Writes a row.",
    "put_many": "Writes several rows in one request. The count states how "
                "many field groups follow.",
    "delete": "Removes a row.",
    "delete_many": "Removes several rows in one request.",
    "list_versions": "Reads the row's recorded history, newest first.",
    "get_version": "Reads one recorded version of the row.",
    "list_scoped": "Reads one page of the rows that share a relation value. "
                   "The relation is part of the address rather than a filter "
                   "applied after the read.",
}

# What separates two commands that serve the same verb. A create and a replace
# are one verb stating two different claims about the row's existence.
_SHELL_PRECONDITION_SENTENCE = {
    "must_not_exist": "The change states that the row must not exist, so a "
                      "create that collides with an existing row is refused "
                      "rather than replacing it.",
    "must_match_version": "The change states the version it expects, so it is "
                          "a compare-and-swap against the version the caller "
                          "last read.",
    "any": "The change states no precondition, so it replaces whatever row the "
           "key already addresses.",
}


def _shell_command_commentary(command: dict[str, Any]) -> str:
    """The literate paragraph one shell command carries in a recipe.

    Two projections reach this: a derived entity command, whose shape the verb
    and its precondition describe, and a declared operation, which states no
    verb because its meaning is the server's. They are told apart by the key
    only the derived one carries.
    """
    if not command.get("kind"):
        return _declared_command_commentary(command)
    kind = command["kind"]
    sentences = [_SHELL_VERB_SENTENCE.get(command.get("verb", ""), "").strip()]
    if kind in ("put", "put_many"):
        sentences.append(
            _SHELL_PRECONDITION_SENTENCE.get(command.get("precondition", ""), ""))
    if command.get("has_intent"):
        sentences.append(
            "The change carries a reason and a commentary, which the server "
            "records on it so the row's history says why it moved.")
    if command.get("allows_version"):
        sentences.append(
            "Passing --version makes the write conditional on the version the "
            "caller last read.")
    return " ".join(sentence for sentence in sentences if sentence)


def _declared_command_commentary(command: dict[str, Any]) -> str:
    """The paragraph a declared operation carries.

    A declared operation states no verb, because what it does is the handler's
    business rather than a relation the store can derive. So the paragraph
    states what the command is: the request it sends, the reply it prints, and
    whether a caller needs a session to run it.
    """
    sentences = [
        f"Sends ={command.get('request', '')}= and prints "
        f"={command.get('response_type', '')}=, so the shape is the protocol's "
        "and the meaning is the service's."
    ]
    names = [field["name"] for field in command.get("positionals") or []]
    if names:
        sentences.append(
            "It reads " + ", ".join(f"={name}=" for name in names)
            + ", in that order.")
    if command.get("public"):
        sentences.append(
            "A caller runs it before it has a session, so the command "
            "presents no token and refuses none.")
    else:
        sentences.append("The caller must have established a session first.")
    if command.get("is_destructive"):
        sentences.append(
            "This command destroys the system it runs against. Do not replay "
            "it against an environment you need.")
    return " ".join(sentences)


def shell_recipe_document(component: str, menu: str, singular: str,
                          plural: str, commands: list[dict[str, Any]],
                          is_operation: bool) -> dict[str, Any]:
    """The literate recipe document a model's shell surface renders as.

    One entity, one document. Each command becomes a section that states what
    it does, the shape it asks for, and the subject it is addressed at, and
    that exports its own script into the shell's library. Mustache cannot loop
    twice over one list with different keys, so the document is assembled here
    rather than in the template.
    """
    # A recipe is titled as the question it answers, so a generated one reads
    # like the hand-written recipes it is catalogued beside rather than like a
    # section heading that wandered into the list.
    title = f"How do I run the {menu} commands from the shell?"
    rendered: list[dict[str, Any]] = []
    for command in commands:
        name = command["command"]
        block = f"{menu}-{name}"
        rendered.append({
            "command": name,
            "heading": name,
            # The block's own name, the id a reader links to, and the script it
            # exports. All three are built from the menu and the command, so one
            # rename moves the section, the block and the library file together.
            "block": block,
            "id": recipe_org_id(f"{component}.{block}"),
            "script": f"{block}.ores",
            # The unit's own help and invocation are written for a caller
            # already inside the submenu. A script loads at the root menu, so
            # both state the whole path a reader types.
            "usage": f"{menu} {command['usage']}",
            "invocation": f"{menu} {command['invocation']}",
            "verb": command.get("verb", ""),
            "subject": command.get("subject", ""),
            "request": command.get("request", ""),
            "response_type": command.get("response_type", ""),
            "commentary": _shell_command_commentary(command),
            "is_destructive": bool(command.get("is_destructive")),
        })
    return {
        "id": recipe_org_id(f"{component}.{menu}"),
        "component": component,
        "menu": menu,
        "singular": singular,
        "plural": plural,
        "title": title,
        "description": (
            f"Every command the {menu} submenu answers, with the script each "
            "one exports into the shell's script library."),
        "intro": _shell_recipe_intro(menu, plural, rendered, is_operation),
        "is_operation": is_operation,
        "commands": rendered,
        "command_count": len(rendered),
    }


def _shell_recipe_intro(menu: str, plural: str, commands: list[dict[str, Any]],
                        is_operation: bool) -> str:
    """The document's opening prose, in the recipe's own voice."""
    what = ("declared operations" if is_operation
            else f"the {plural} resource")
    if len(commands) == 1:
        return (
            f"This file documents {what} at the shell. Its one command is "
            f"=ores-shell> {menu} {commands[0]['command']}=, and the section "
            "below exports it as a script into "
            "=projects/ores.shell/scripts/library/=, which the shell can load.")
    return (
        f"This file documents {what} at the shell, one section per command. "
        f"Every command is a subcommand of ={menu}=, and every section exports "
        "its own script into =projects/ores.shell/scripts/library/=. Loading a "
        "script runs that one command, so a failure names the command it came "
        "from.")


def entity_shell_commands(entity: dict[str, Any]) -> list[dict[str, Any]]:
    """The shell commands an entity's derived operation set yields."""
    return entity_shell_plan(entity)["commands"]


def entity_shell_plan(entity: dict[str, Any]) -> dict[str, Any]:
    """The entity's shell commands, and the facts the unit needs about them.

    A unit emits a helper only when some command needs it and a loop only when
    a batch verb is present. Mustache cannot ask a list whether any member has
    a property, so those answers are computed here rather than tested per
    command.
    """
    """The shell's view of an entity's derived operation set.

    One command per operation the entity derives, so the REPL covers exactly
    the verbs the entity answers and a verb the model gains appears without an
    edit here. A put becomes two commands, because a create and a replace are
    one verb that states two different claims.

    ``kind`` is the shape of the handler rather than the verb: six shapes serve
    the verbs, because a versions read is a paged read addressed by a key and a
    list-by-relation read is a paged read addressed by a relation. The template
    branches on the shape and builds each member path itself, so the projection
    carries names and types rather than C++.

    ``keys`` and ``writes`` are what a caller types. A key column and a write
    field are shaped alike, because one token fills either; what differs is
    where the template puts them -- under the key, under the removal, under the
    change, or inside the loop of a batch verb.

    Only an entity that derives its own protocol reaches here. One whose
    protocol an operation model owns has no derived request types to name, and
    its declared operations are what the shell renders instead.
    """
    component = entity.get("component", "")
    singular = entity.get("entity_singular", "")
    namespace = f"ores::{component}::messaging"

    supplies: dict[str, str] = {}
    for column in list(entity.get("columns") or []) + list(
            (entity.get("primary_key") or {}).get("columns") or []):
        name = column.get("name") or column.get("column")
        if not name:
            continue
        if column.get("is_minted"):
            supplies[name] = "minted"
        elif column.get("is_session_party"):
            supplies[name] = "session_party"
        else:
            supplies[name] = "user"

    def _input(name: str, cpp_type: str) -> dict[str, Any]:
        cpp = (cpp_type or "std::string").strip()
        supply = supplies.get(name, "user")
        return {
            "name": name,
            "cpp_type": cpp,
            "is_user": supply == "user",
            "is_minted": supply == "minted",
            "is_session_party": supply == "session_party",
            "is_string": cpp == "std::string",
            "is_bool": cpp == "bool",
            "is_list": cpp == _SHELL_LIST_TYPE,
            "needs_from_token": cpp not in ("std::string", "bool"),
            "fillable": cpp == _SHELL_LIST_TYPE or cpp in _SHELL_TOKEN_TYPES,
        }

    # The key a command types is the one the model declares, because that is
    # the key the request it fills carries. Reading the storage key here would
    # have a command build a member the request does not have.
    declared = declared_key_column(entity)
    if declared is not None:
        keys = [_input(_column_name(declared), _key_cpp_type(declared))]
    else:
        keys = [
            _input(column.get("column", ""), column.get("cpp_type", ""))
            for column in (entity.get("primary_key") or {}).get("columns") or []
        ]
    writes = [
        _input(field.get("name", ""), field.get("cpp_type", ""))
        for field in entity.get("write_fields") or []
    ]
    # A batch verb reads its positionals in groups, so each input states the
    # offset it occupies within its group.
    for index, item in enumerate(keys):
        item["index"] = index
    for index, item in enumerate(writes):
        item["index"] = index

    # verb -> (kind, command). A put is absent because it becomes two commands.
    shapes = {
        "list": ("paged", "list"),
        "get": ("key_read", "get"),
        "get_many": ("get_many", "get-many"),
        "put_many": ("put_many", "put-many"),
        "delete": ("delete", "delete"),
        "delete_many": ("delete_many", "delete-many"),
        "list_versions": ("versions", "versions"),
        "get_version": ("version_read", "version"),
    }

    def _used_inputs(kind: str, typed: list[dict[str, Any]]) -> list[dict[str, Any]]:
        if kind in ("put", "put_many"):
            return typed
        if kind in ("paged", "list_by"):
            return []
        return keys

    def _command(operation: dict[str, Any], kind: str, name: str,
                 precondition: str = "",
                 relation: dict[str, Any] | None = None,
                 id_from_caller: bool = False) -> dict[str, Any]:
        # A create mints the row's id, so the caller does not state it. A
        # replace addresses a row that already exists, and the id is how the
        # store finds it: a minted id reaches the trigger as a row no current
        # row matches, so the write is taken for a create and collides with
        # the natural key's unique index. The replace therefore takes the id
        # from the caller, which is what a read of the row returns.
        def _typed(field: dict[str, Any]) -> dict[str, Any]:
            item = _input(field.get("name", ""), field.get("cpp_type", ""))
            if id_from_caller and item.get("is_minted"):
                item["is_minted"] = False
                item["is_user"] = True
            return item

        typed = [_typed(field) for field in entity.get("write_fields") or []]
        command = {
            "command": name,
            "identifier": name.replace("-", "_"),
            "verb": operation.get("verb", ""),
            "kind": kind,
            "request": operation["request"],
            "response_type": operation["response"],
            "subject": operation["subject"],
            "keys": keys,
            "writes": typed,
            # A single put fills a session party itself, so the caller types
            # only the remaining fields. A put_many reads the whole write
            # record, because a batch of changes states its own ids. The two
            # arities differ, and a command whose stated count is the wider one
            # refuses the line its own help prints.
            "put_writes": [field for field in typed if field.get("is_user")],
            "key_arity": len(keys),
            "write_arity": len(typed),
            "put_arity": sum(1 for field in typed if field.get("is_user")),
            "relation": relation,
            "has_order": bool(operation.get("has_order")),
            "has_intent": operation.get("verb") in
                ("put", "put_many", "delete", "delete_many"),
            "precondition": precondition,
            "allows_version": kind in ("put", "delete") and precondition != "must_not_exist",
            # Only what the handler actually asks for: a paged read takes no
            # key, and a write takes the write record rather than the key,
            # because the key travels inside it.
            "unsupported": sorted(
                item["name"] for item in _used_inputs(kind, typed)
                if not item["fillable"]),
        }
        # The flags the handler declares. Exact rather than uniform, so a
        # command that takes no page cannot be handed one.
        value_flags: list[str] = []
        switches: list[str] = []
        if command["has_order"]:
            value_flags += ["offset", "limit", "order"]
            switches += ["desc"]
        if kind == "list_by":
            value_flags += ["scope"]
        if kind == "version_read" or command["allows_version"]:
            value_flags += ["version"]
        if kind == "put_many":
            value_flags += ["count"]
        command["flags"] = value_flags
        command["switches"] = switches
        # Mustache cannot compare a string, so the shape is also stated as a
        # flag per shape and the template selects a body by section name.
        for shape in ("paged", "list_by", "versions", "key_read", "version_read",
                      "put", "put_many", "delete", "get_many", "delete_many"):
            command[f"is_{shape}"] = kind == shape
        # How many positionals the command reads, and whether that count is
        # fixed. A batch verb reads a whole number of groups, so only a
        # remainder is an error.
        if kind in ("paged", "list_by"):
            command["positional_count"] = 1 if kind == "list_by" else 0
            command["exact_count"] = True
        elif kind in ("put", "put_many"):
            command["positional_count"] = len(writes)
            command["exact_count"] = kind == "put"
        elif kind == "delete_many":
            command["positional_count"] = len(keys)
            command["exact_count"] = False
        elif kind == "get_many":
            command["positional_count"] = len(keys)
            command["exact_count"] = False
        else:
            command["positional_count"] = len(keys)
            command["exact_count"] = True
        command["usage"] = _entity_shell_usage(command)
        command["invocation"] = _entity_shell_invocation(command)
        return command

    commands: list[dict[str, Any]] = []
    for operation in entity.get("operations") or []:
        verb = operation.get("verb", "")
        if verb == "put":
            commands.append(_command(operation, "put", "add", "must_not_exist"))
            commands.append(_command(operation, "put", "set", "any",
                                     id_from_caller=True))
            continue
        # A scoped read's verb names the shape and not the column it is scoped
        # by: the derivation states the relation in `leading`, so the command
        # and its subject are built from that rather than from the verb.
        relation = ""
        if verb == "list_scoped":
            relation = operation.get("leading", "")
        elif verb.startswith("list_by_"):
            relation = verb[len("list_by_"):]
        if relation:
            addressed_by = _input(
                relation, _column_cpp_type(entity, relation))
            command = _command(operation, "list_by",
                               f"by-{relation.replace('_', '-')}",
                               relation=addressed_by)
            command["unsupported"] = sorted(
                item["name"] for item in [addressed_by] if not item["fillable"])
            command["usage"] = _entity_shell_usage(command)
            command["invocation"] = _entity_shell_invocation(command)
            commands.append(command)
            continue
        if verb in shapes:
            kind, name = shapes[verb]
            commands.append(_command(operation, kind, name))

    def _any(predicate) -> bool:
        return any(predicate(item) for command in commands
                   for item in command["keys"] + command["writes"])

    kinds = {command["kind"] for command in commands}
    # A verb the projection has no shape for would be skipped in silence, and
    # an entity would answer fewer verbs from the shell than it derives with
    # nothing to say so. The plan names them instead.
    known = set(shapes) | {"put", "list_scoped"}
    uncovered = sorted({
        operation["verb"] for operation in entity.get("operations") or []
        if operation.get("verb") not in known
    })
    return {
        "commands": commands,
        "uncovered_verbs": uncovered,
        "command_count": len(commands),
        "has_order": any(command["has_order"] for command in commands),
        "has_list": _any(lambda item: item["is_list"]),
        "has_bool": _any(lambda item: item["is_bool"]),
        "has_minted": _any(lambda item: item["is_minted"]),
        "has_session_party": _any(lambda item: item["is_session_party"]),
        "has_batch": bool(kinds & {"get_many", "put_many", "delete_many"}),
        "has_version": bool(kinds & {"version_read", "put", "delete"}),
        "any_versioned": any(command["allows_version"] for command in commands),
        "has_helpers": bool(commands),
    }


# The value a generated script sends for a field. A generator cannot invent a
# real id, and a recipe that sent nothing would fail before it left the client:
# a uuid the shell cannot parse, or an arity the command refuses, proves only
# that the client is strict. So a generated script sends a well-formed value
# that addresses nothing. A service that answers "not found" has then proved
# the command is registered, the subject has a subscriber and the request
# decoded -- which is what the script exists to check.
_SENTINEL_VALUES = {
    "boost::uuids::uuid": "00000000-0000-0000-0000-000000000000",
    "std::string": "__none__",
    "bool": "false",
    "int": "0",
    "std::int32_t": "0",
    "std::int64_t": "0",
    "std::uint16_t": "0",
    "std::uint32_t": "0",
    "std::uint64_t": "0",
    "double": "0",
    "std::chrono::system_clock::time_point": "1970-01-01T00:00:00Z",
    "boost::asio::ip::address": "0.0.0.0",
    "std::vector<std::string>": "__none__",
}

# A write states an intent, and the reason code is an enum value on the wire
# rather than free text, so the script sends a code the schema seeds.
_SENTINEL_REASON = "system.new_record"
_SENTINEL_COMMENTARY = "generated_script"


def _sentinel_value(cpp_type: str) -> str:
    """A well-formed value of ``cpp_type`` that addresses no row."""
    return _SENTINEL_VALUES.get((cpp_type or "").strip(), "__none__")


def _sentinel_for_field(name: str, cpp_type: str) -> str:
    """A sentinel for one declared field, by name where the name decides.

    A write's reason code is an enum the schema seeds and its commentary is
    free text, so a generic string sentinel would be refused before the
    request reached the handler -- and a script that never reaches the handler
    checks nothing.
    """
    if name == "reason_code":
        return _SENTINEL_REASON
    if name == "commentary":
        return _SENTINEL_COMMENTARY
    return _sentinel_value(cpp_type)


def _entity_shell_invocation(command: dict[str, Any]) -> str:
    """The command line a generated script sends.

    Built beside :func:`_entity_shell_usage` from the same inputs, so the line
    a script runs is the shape the help states and the two cannot disagree. An
    optional flag is left out rather than filled, because a script that states
    only what a command requires is the shortest thing that reaches it.
    """
    kind = command["kind"]
    tokens = [command["command"]]
    if kind in ("put", "put_many"):
        if kind == "put_many":
            tokens += ["--count", "1"]
            typed = command["writes"]
        else:
            typed = command["put_writes"]
        tokens += [_sentinel_value(field["cpp_type"]) for field in typed]
    elif kind == "list_by":
        tokens.append(_sentinel_value(command["relation"]["cpp_type"]))
    elif kind != "paged":
        tokens += [_sentinel_value(key["cpp_type"]) for key in command["keys"]]
    if command["has_intent"]:
        tokens += [_SENTINEL_REASON, _SENTINEL_COMMENTARY]
    if kind == "version_read":
        tokens += ["--version", "1"]
    # A page of one keeps a generated script's output readable, and a read that
    # is not paged has no such flag to give.
    if command["has_order"]:
        tokens += ["--limit", "1"]
    return " ".join(tokens)


def _entity_shell_usage(command: dict[str, Any]) -> str:
    """The one-line help a generated entity command registers.

    What a command asks for follows from its shape: a paged read is addressed
    by nothing, a write by its write record, and everything else by the key.
    """
    kind = command["kind"]
    parts = [command["command"]]
    if kind in ("put", "put_many"):
        if kind == "put_many":
            parts.append("--count <n>")
            typed = command["writes"]
        else:
            typed = command["put_writes"]
        parts.extend(f"<{field['name']}>" for field in typed)
    elif kind == "list_by":
        parts.append(f"<{command['relation']['name']}>")
    elif kind != "paged":
        parts.extend(f"<{key['name']}>" for key in command["keys"])
    if command["has_intent"]:
        parts.append("<reason> <commentary>")
    if kind == "version_read":
        parts.append("--version <n>")
    elif command["allows_version"]:
        parts.append("[--version <n>]")
    if command["has_order"]:
        parts.extend(["[--offset <n>]", "[--limit <n>]",
                      "[--order <field>]", "[--desc]"])
    return " ".join(parts)


def junction_ts_fields(junction: dict[str, Any]) -> list[dict[str, Any]]:
    """The junction members that need a TypeScript projection, in the order

    The audit tail is hard-coded as strings by the template, so it is not
    listed here. Only the left and right columns and the junction's own
    columns carry a C++ type the projection may not reach. A member with no
    ``cpp_type`` states nothing and is skipped.
    """
    fields: list[dict[str, Any]] = []
    for key in ("left", "right"):
        side = junction.get(key) or {}
        cpp_type = (side.get("cpp_type") or "").strip()
        if not cpp_type:
            continue
        field: dict[str, Any] = {"name": side.get("column") or key,
                                 "cpp_type": cpp_type}
        if side.get("ts_type"):
            field["ts_type"] = side["ts_type"]
        fields.append(field)
    for column in junction.get("columns") or []:
        cpp_type = (column.get("cpp_type") or "").strip()
        if not cpp_type:
            continue
        field = {"name": column.get("name"), "cpp_type": cpp_type}
        if column.get("ts_type"):
            field["ts_type"] = column["ts_type"]
        fields.append(field)
    return fields


def _reject_silent_junction_ts_gap(
    path: Path | str, junction: dict[str, Any]
) -> None:
    """Reject a junction the TypeScript domain interface cannot state in full.

    A member whose C++ type has no TypeScript projection renders an
    interface with the member missing, so a UI reading it fails at run
    time rather than at codegen. The model must either map the type or
    switch the facet off in its own drawer -- the same rule
    ``_reject_silent_ts_gap`` applies to an operation's messages.
    """
    unmapped = sorted({
        field["cpp_type"] for field in junction_ts_fields(junction)
        if "ts_type" not in field
    })
    if not unmapped:
        return
    raise ValueError(
        f"{Path(path).name}: no TypeScript projection for {unmapped}; map the "
        "type in org_loader._ts_domain_type, or set ':ores.ts.domain.enabled: "
        "nil' in the file's :PROPERTIES: drawer to skip the TypeScript facet"
    )


def entity_domain_ts_fields(entity: dict[str, Any]) -> list[dict[str, Any]]:
    """The entity members ``domain_types.ts.mustache`` states with a
    ``{{ts_type}}``, in template order.

    Mirrors the template's ``{{^has_domain_groups}}`` body: the primary key
    and the natural keys unless the identity group carries them, then the
    model's own columns. A grouped entity's body is its ``domain_groups``
    members instead. Members the template does not state through
    ``{{ts_type}}`` -- the derived prelude, a ``sql_only`` column, an
    identity-group column -- are not listed: they cannot render an empty
    type. A member with no ``cpp_type`` states nothing and keeps the empty
    string, which the caller reports.
    """
    if entity.get("has_domain_groups"):
        return [
            {"name": group.get("member"),
             "cpp_type": group.get("type_qualified"),
             "ts_type": group.get("ts_type")}
            for group in entity.get("domain_groups") or []
        ]
    fields: list[dict[str, Any]] = []
    if not entity.get("has_identity_group"):
        for column in (entity.get("primary_key") or {}).get("columns") or []:
            fields.append(_entity_domain_ts_member(column, "column"))
        for column in entity.get("natural_keys") or []:
            fields.append(_entity_domain_ts_member(column, "column"))
    for column in entity.get("columns") or []:
        if column.get("is_identity_group_column") or column.get("sql_only"):
            continue
        fields.append(_entity_domain_ts_member(column, "name"))
    return fields


def _entity_domain_ts_member(
    field: dict[str, Any], name_key: str
) -> dict[str, Any]:
    """One entity-domain member: its name, its C++ type, and the projection
    already stored on the field dict, if any."""
    member: dict[str, Any] = {
        "name": field.get(name_key),
        "cpp_type": (field.get("cpp_type") or "").strip(),
    }
    if field.get("ts_type"):
        member["ts_type"] = field["ts_type"]
    return member


def _reject_silent_entity_domain_ts_gap(
    path: Path | str, entity: dict[str, Any]
) -> None:
    """Reject an entity whose TypeScript domain interface cannot state a
    member in full.

    A member whose C++ type has no TypeScript projection renders
    ``<member>: ;`` -- a module that does not compile -- so the entity must
    map the type in ``org_loader._ts_domain_type``. A type with no wire
    shape a TypeScript client can read is a reason to record in the model,
    not to switch the facet off: unlike ``_reject_silent_ts_gap`` this guard
    reads no ``:ores.*.enabled:`` property, because the project's direction
    is that no facet is switched off by hand.
    """
    unmapped = [
        member for member in entity_domain_ts_fields(entity)
        if "ts_type" not in member
    ]
    if not unmapped:
        return
    listed = ", ".join(
        f"{member['name']} ({member['cpp_type']})" if member["cpp_type"]
        else f"{member['name']} (no cpp_type)"
        for member in unmapped
    )
    raise ValueError(
        f"{Path(path).name}: no TypeScript projection for {listed}; map the "
        "type in org_loader._ts_domain_type, or record in the model why the "
        "member has no wire shape a TypeScript client can read -- the entity "
        "domain interface is not switched off by a model property"
    )


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
    ``entity``). The org file preserves all C++/protocol scalar
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
    "runtime", "entry_point",
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
      names), optional ``:runtime:`` (``native`` for a compiled binary,
      the default, or ``node`` for a TypeScript service, which also
      needs ``:entry_point:`` relative to its component directory), plus
      an optional ``** Extra args`` sub-heading carrying
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
    # Component kind (flat | api | core | service | composite | adapter)
    # selects the scaffolding variant set via the graph's kind discriminator;
    # defaults to "flat". A composite has sub-components and no code of its
    # own, so it generates none of the code-bearing archetypes; the one thing
    # it does own is the CMakeLists that adds its parts. An adapter is a part
    # of such a composite whose build files are hand-authored, so no
    # build-file archetype serves it and only the file lists are generated.
    c["kind"] = fm.get("component_kind", "flat")
    # Part order is declared rather than derived. Dependency order is not
    # alphabetical -- ores.shell's `api trading application modeling` is one
    # such -- so the model states the order and the template renders it.
    parts = fm.get("parts", "").split()
    c["parts"] = [{"part": name, "last": i == len(parts) - 1}
                  for i, name in enumerate(parts)]
    return {"component": c}
