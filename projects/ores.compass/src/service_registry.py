# -*- coding: utf-8 -*-
"""The service registry: projects/modeling/service_registry.org.

Each top-level heading whose drawer carries :replicas: is one fleet process.
This reads the entries the way the retired codegen.org_loader did, into the
same dictionaries, so systemd_generate, compass_services, image_build and
nats_certs read the registry without the Python generator.
"""

import re
from pathlib import Path

SERVICE_REGISTRY_PATH = "projects/modeling/service_registry.org"

_SCALARS = (
    "psql_var", "env_key", "iam_role", "description", "display_name", "summary",
    "account_var", "role", "email", "runtime", "entry_point",
)
_HEADING = re.compile(r"^(\*+)\s+(.+?)\s*$")
_PROPERTY = re.compile(r"^\s*:([A-Za-z_][A-Za-z0-9_.-]*)\s*:\s*(.*)$")
_BULLET = re.compile(r"^\s*-\s+(.*)$")
_BLOCK_BEGIN = re.compile(r"^\s*#\+begin_(src\s+\S+|comment\s*$)", re.IGNORECASE)
_BLOCK_END = re.compile(r"^\s*#\+end_(src|comment)\s*$", re.IGNORECASE)


def _headings(text):
    """Each heading as (level, title, drawer properties, bullet items)."""
    headings = []
    in_drawer = in_block = False
    for line in text.splitlines():
        if in_block:
            in_block = not _BLOCK_END.match(line)
            continue
        if in_drawer:
            if line.strip().upper() == ":END:":
                in_drawer = False
            elif (m := _PROPERTY.match(line)) and headings:
                headings[-1][2][m.group(1)] = m.group(2).strip()
            continue
        if line.strip().upper() == ":PROPERTIES:":
            in_drawer = True
        elif _BLOCK_BEGIN.match(line):
            in_block = True
        elif m := _HEADING.match(line):
            headings.append((len(m.group(1)), m.group(2).strip(), {}, []))
        elif (m := _BULLET.match(line)) and headings:
            headings[-1][3].append(m.group(1).strip())
    return headings


def _select_table(item):
    pairs = (part.split("=", 1) for part in item.split(",") if "=" in part)
    return {k.strip(): v.strip() for k, v in pairs}


def load_service_registry(project_root):
    """Every fleet process in the registry, in document order."""
    text = (Path(project_root) / SERVICE_REGISTRY_PATH).read_text(encoding="utf-8")
    headings = _headings(text)
    services = []
    for i, (level, title, props, _) in enumerate(headings):
        if level != 1 or "replicas" not in props:
            continue
        sections = {}
        for child_level, child_title, _, bullets in headings[i + 1:]:
            if child_level == 1:
                break
            if child_level == 2:
                sections.setdefault(child_title.lower(), bullets)
        entry = {"name": title}
        entry.update((k, props[k]) for k in _SCALARS if k in props)
        entry["dml_prefixes"] = [{"prefix": b} for b in sections.get("dml prefixes", [])]
        entry["select_tables"] = [t for t in map(_select_table, sections.get("select tables", [])) if t]
        entry["select_prefixes"] = [{"prefix": b} for b in sections.get("select prefixes", [])]
        entry["execute_prefixes"] = [{"prefix": b} for b in sections.get("execute prefixes", [])]
        entry["replicas"] = int(props["replicas"])
        entry["enabled"] = props.get("enabled", "true").strip().lower() == "true"
        entry["depends_on"] = [d.strip() for d in props.get("depends_on", "").split(",") if d.strip()]
        entry["extra_args"] = list(sections.get("extra args", []))
        services.append(entry)
    return services
