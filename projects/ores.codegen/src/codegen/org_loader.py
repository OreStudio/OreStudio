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
from pathlib import Path
from typing import Any


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


_ORG_VERBATIM_RE = re.compile(r"=([^=\s][^=]*?)=")


def _strip_org_markup(text: str) -> str:
    """Strip org-mode inline markup that should not appear in codegen output.

    Currently strips ``=foo=`` verbatim markers, leaving the bare token.
    Other markup (``*bold*``, ``/italic/``, ``~code~``) could be added if
    we adopt them in models."""
    return _ORG_VERBATIM_RE.sub(r"\1", text)


def _strip_body(node: OrgNode) -> str:
    """Return the prose body of a node, trimmed of leading/trailing blank
    lines and with org-mode verbatim markup stripped."""
    lines = node.body_lines
    while lines and not lines[0].strip():
        lines = lines[1:]
    while lines and not lines[-1].strip():
        lines = lines[:-1]
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


def _soft_fk_validation_node_to_dict(node: OrgNode) -> dict[str, Any]:
    """Convert a soft FK validation heading into a template-ready dict.

    The heading title becomes ``column``; the PROPERTIES drawer supplies
    ``table``, ``error_message``, and optional boolean flags
    ``nullable``, ``use_no_tenant``, ``use_system_tenant``.
    """
    out: dict[str, Any] = {"column": node.title}
    for k, v in node.properties.items():
        out[k.lower()] = _parse_typed(v)
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
                elif low == "timestamp":
                    entry["is_timestamp"] = True
                continue
            entry[k] = _parse_typed(v)
        out.append(entry)
    return out


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
    for k in ("has_tenant_id", "image_id"):
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

    pk = _section(doc.root, "Primary key")
    if pk:
        out_pk: dict[str, Any] = {}
        for k, v in pk.properties.items():
            out_pk[k.lower()] = _parse_typed(v)
        description, detail = _description_and_detail(pk)
        if description:
            out_pk["description"] = description
        if detail:
            out_pk["detail"] = detail
        if "generator" in pk.src_blocks:
            out_pk["generator_expr"] = pk.src_blocks["generator"]
        de["primary_key"] = out_pk

    nks = _section(doc.root, "Natural keys")
    if nks:
        de["natural_keys"] = [_natural_key_node_to_dict(c) for c in nks.children]

    cols = _section(doc.root, "Columns")
    if cols:
        de["columns"] = [_column_node_to_dict(c) for c in cols.children]

    sql = _section(doc.root, "SQL")
    if sql:
        de["sql"] = {k.lower(): _parse_typed(v) for k, v in sql.properties.items()}

    repo = _section(doc.root, "Repository")
    if repo:
        de["repository"] = {k.lower(): _parse_typed(v) for k, v in repo.properties.items()}

    # SQL section: SQL-specific flags + structured sub-sections.
    sql_section = _section(doc.root, "SQL")
    if sql_section:
        sql_flags = _section(sql_section, "Flags")
        if sql_flags:
            de["sql"] = {
                k.lower(): _parse_typed(v) for k, v in sql_flags.properties.items()
            }
        soft_fk_section = _section(sql_section, "Soft FK validations")
        if soft_fk_section and soft_fk_section.children:
            de.setdefault("sql", {})["soft_fk_validations"] = [
                _soft_fk_validation_node_to_dict(c) for c in soft_fk_section.children
            ]
        checks_section = _section(sql_section, "Checks")
        if checks_section and checks_section.tables:
            rows = _parse_org_table_rows(checks_section)
            expressions = [r["expression"] for r in rows if r.get("expression")]
            if expressions:
                de.setdefault("sql", {})["extra_checks"] = expressions
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
            qt_out: dict[str, Any] = {}
            for k, v in qt.properties.items():
                qt_out[k.lower()] = _parse_typed(v)
            df = _section(qt, "Detail fields")
            if df:
                qt_out["detail_fields"] = _detail_fields(df)
            qc = _section(qt, "Columns (Qt model)")
            if qc:
                qt_out["columns"] = _qt_columns(qc)
            de["qt"] = qt_out

        # Custom repository methods (the literate fragment mechanism).
        cm = _section(cpp_section, "Custom repository methods")
        if cm:
            de["custom_repository_methods"] = _custom_methods(cm)

    # Implementations by kind UUID — consumed by the post-render
    # ``<<paste:UUID>>`` substitution pass in the codegen driver.
    impls = _collect_implementations(doc.root)
    if impls:
        de["implementations"] = impls

    return {"domain_entity": de}


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
        errors.append("Missing required section: Primary key")
    elif "column" not in de["primary_key"]:
        errors.append("Primary key missing required property: column")

    for col in de.get("columns", []):
        for required in REQUIRED_COLUMN_PROPS:
            if required not in col:
                errors.append(
                    f"Column '{col.get('name','?')}' missing required property: {required}"
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
    fm = doc.frontmatter

    j: dict[str, Any] = {}
    for key in ("product", "schema", "component", "name", "name_singular",
                "name_title", "name_singular_words", "brief"):
        if key in fm:
            j[key] = fm[key]
    if "has_tenant_id" in fm:
        j["has_tenant_id"] = _parse_typed(fm["has_tenant_id"])

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
            entry: dict[str, Any] = {"name": node.title}
            for k, v in node.properties.items():
                key = k.lower()
                if key in ("default", "default_value"):
                    entry[key] = v  # keep raw string; Mustache 0-falsy
                else:
                    entry[key] = _parse_typed(v)
            description, detail = _description_and_detail(node)
            if description:
                entry["description"] = description
            if detail:
                entry["detail"] = detail
            if "generator" in node.src_blocks:
                entry["generator_expr"] = node.src_blocks["generator"]
            columns.append(entry)
    j["columns"] = columns

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
        j["cpp"] = cpp_out

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


def load_org_table_model(path: Path | str) -> dict[str, Any]:
    """Load an org-mode table model into the ``{table: {...}}`` dict
    shape that ``sql_schema_create.mustache`` consumes via the
    ``--profile sql`` model_types=["table"] route."""
    text = Path(path).read_text(encoding="utf-8")
    doc = parse_org(text)
    fm = doc.frontmatter

    t: dict[str, Any] = {}
    for key in ("product", "schema", "component",
                "entity_singular", "entity_plural", "coding_scheme"):
        if key in fm:
            t[key] = fm[key]
    if "has_tenant_id" in fm:
        t["has_tenant_id"] = _parse_typed(fm["has_tenant_id"])
    if "image_id" in fm:
        t["image_id"] = _parse_typed(fm["image_id"])

    body = _strip_body(doc.root)
    if body:
        t["description"] = body

    pk_section = _section(doc.root, "Primary key")
    if pk_section:
        pk: dict[str, Any] = {}
        for k, v in pk_section.properties.items():
            pk[k.lower()] = _parse_typed(v)
        t["primary_key"] = pk

    cols_section = _section(doc.root, "Columns")
    columns: list[dict[str, Any]] = []
    if cols_section:
        for node in cols_section.children:
            entry: dict[str, Any] = {"name": node.title}
            for k, v in node.properties.items():
                key = k.lower()
                if key == "default":
                    entry[key] = v  # raw string; Mustache 0-falsy guard
                else:
                    entry[key] = _parse_typed(v)
            columns.append(entry)
    t["columns"] = columns

    vfn_section = _section(doc.root, "Validation function")
    if vfn_section:
        vfn: dict[str, Any] = {}
        for k, v in vfn_section.properties.items():
            key = k.lower()
            if key in ("default", "default_value"):
                vfn[key] = v  # raw string; Mustache 0-falsy guard
            else:
                vfn[key] = _parse_typed(v)
        t["validation_fn"] = vfn

    insert_section = _section(doc.root, "Insert trigger")
    if insert_section:
        validations_section = _section(insert_section, "Validations")
        if validations_section:
            rows = _parse_org_table_rows(validations_section)
            t["insert_trigger"] = {"validations": rows}

    checks_section = _section(doc.root, "Check constraints")
    if checks_section:
        constraints: list[dict[str, Any]] = []
        for node in checks_section.children:
            entry: dict[str, Any] = {}
            # The converter assigns auto-titles `check_N` to satisfy
            # org's "headings host drawers" rule; those carry no
            # semantic weight. A hand-authored title (anything else)
            # is meaningful — preserve it as the constraint name.
            if not node.title.startswith("check_"):
                entry["name"] = node.title
            for k, v in node.properties.items():
                entry[k.lower()] = v  # keep expression verbatim
            constraints.append(entry)
        t["check_constraints"] = constraints

    indexes_section = _section(doc.root, "Indexes")
    if indexes_section:
        indexes: list[dict[str, Any]] = []
        for node in indexes_section.children:
            entry: dict[str, Any] = {"name": node.title}
            for k, v in node.properties.items():
                entry[k.lower()] = _parse_typed(v)
            indexes.append(entry)
        t["indexes"] = indexes
    else:
        t["indexes"] = []

    return {"table": t}


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
    ``--profile sql`` model_types=["schema"] route.

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

    body = _strip_body(doc.root)
    if body:
        e["description"] = body

    pk_section = _section(doc.root, "Primary key")
    if pk_section:
        pk: dict[str, Any] = {}
        for k, v in pk_section.properties.items():
            key = k.lower()
            if key == "cpp_type":
                pk[key] = v  # raw string
            else:
                pk[key] = _parse_typed(v)
        e["primary_key"] = pk

    cols_section = _section(doc.root, "Columns")
    columns: list[dict[str, Any]] = []
    if cols_section:
        for node in cols_section.children:
            entry: dict[str, Any] = {"name": node.title}
            for k, v in node.properties.items():
                key = k.lower()
                if key in ("default", "default_value"):
                    entry[key] = v  # raw string; Mustache 0-falsy guard
                else:
                    entry[key] = _parse_typed(v)
            columns.append(entry)
    e["columns"] = columns

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


def load_org_service_registry_model(path: Path | str) -> dict[str, Any]:
    """Load the org-mode service-registry model into the
    ``{service_registry: {services: [...]}}`` dict consumed by the
    ``service-registry`` profile (shell vars, IAM users, grants).

    Each top-level ``* <service name>`` heading becomes one
    ``services[]`` entry. The property drawer carries the scalars
    (``:psql_var:``, ``:env_key:``, ``:iam_role:``, ``:description:``,
    optional ``:role:``, ``:email:``); ``** DML prefixes`` /
    ``** Select tables`` / ``** Select prefixes`` sub-headings carry
    the per-service lists as org bullets."""
    text = Path(path).read_text(encoding="utf-8")
    doc = parse_org(text)

    services: list[dict[str, Any]] = []
    for node in doc.root.children:
        # Defensive: skip top-level headings that aren't service entries
        # (e.g. a future "Notes" or "References" section in the doc).
        # Every real service carries :psql_var:.
        if "psql_var" not in node.properties:
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
    # Component kind (flat | api | core | service) selects the scaffolding
    # variant set via the graph's kind discriminator; defaults to "flat".
    c["kind"] = fm.get("component_kind", "flat")
    return {"component": c}
