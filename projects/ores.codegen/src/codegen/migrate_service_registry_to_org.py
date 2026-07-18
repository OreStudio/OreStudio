"""
Convert ``ores_services_service_registry.json`` to a literate
``service_registry.org`` file.

The service registry is the catalogue of ORE Studio backend
services with their psql var, IAM role, DDL grant prefixes and
read-only select prefixes. Codegen renders it through the
``ores.shell.service`` address into shell vars, IAM user/role
SQL, and matching grant scripts.

Org shape: one ``* <service name>`` heading per service with a
property drawer for the scalars (``:psql_var:``, ``:env_key:``,
``:iam_role:``, ``:description:``, ``:email:``, optional
``:role:``), and ``** DML prefixes`` / ``** Select tables`` /
``** Select prefixes`` sub-headings carrying the per-service
lists as org bullets (one prefix per line).

Usage::

    python3 projects/ores.codegen/src/codegen/migrate_service_registry_to_org.py \\
        --input projects/ores.codegen/models/services/ores_services_service_registry.json \\
        --output projects/modeling/service_registry.org
"""
import argparse
import json
import sys
import uuid
from datetime import date
from pathlib import Path


_SERVICE_SCALARS = (
    "psql_var", "env_key", "iam_role", "description", "role", "email",
)


def _drawer(pairs):
    items = [(k, v) for k, v in pairs if v is not None and v != ""]
    if not items:
        return ""
    width = max(len(k) for k, _ in items)
    lines = [":PROPERTIES:"]
    for k, v in items:
        lines.append(f":{k}:{' ' * (width - len(k) + 1)}{v}")
    lines.append(":END:")
    return "\n".join(lines)


def _frontmatter(fid):
    today = date.today().isoformat()
    return "\n".join([
        f":PROPERTIES:\n:ID: {fid}\n:END:",
        "#+title: ores.services.service_registry",
        "#+description: Codegen service-registry model — catalogue of ORE Studio backend services with their psql vars, IAM roles, and DDL grant prefixes.",
        "#+type: ores.codegen.service_registry",
        "#+filetags: :codegen:service_registry:literate:",
        f"#+created: {today}",
        f"#+updated: {today}",
    ])


def _bullets(items, key):
    """Render a list of dicts ([{prefix: X}, ...]) as org bullets."""
    if not items:
        return ""
    return "\n".join(f"- {it.get(key, '').rstrip()}" for it in items)


def _select_table_bullet(item):
    """select_tables entries vary in shape — keep their dict, render as
    `- key1=value1, key2=value2` to round-trip cleanly. Currently no
    file uses this list (all 17 services have select_tables: []),
    but the converter handles future entries."""
    return "- " + ", ".join(f"{k}={v}" for k, v in item.items())


def _service_block(svc):
    drawer = _drawer((k, svc.get(k)) for k in _SERVICE_SCALARS)
    parts = [f"* {svc['name']}", drawer]
    dml = svc.get("dml_prefixes", [])
    if dml:
        parts += ["", "** DML prefixes", "", _bullets(dml, "prefix")]
    selt = svc.get("select_tables", [])
    if selt:
        parts += ["", "** Select tables", ""]
        parts += [_select_table_bullet(it) for it in selt]
    selp = svc.get("select_prefixes", [])
    if selp:
        parts += ["", "** Select prefixes", "", _bullets(selp, "prefix")]
    return "\n".join(parts)


def convert(model_path, out_path):
    model = json.loads(Path(model_path).read_text(encoding="utf-8"))
    services = model["service_registry"]["services"]
    fid = str(uuid.uuid4()).upper()
    parts = [_frontmatter(fid), ""]
    for svc in services:
        parts.append(_service_block(svc))
        parts.append("")
    out_path.write_text("\n".join(parts).rstrip() + "\n", encoding="utf-8")
    return out_path


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--input", required=True, type=Path)
    p.add_argument("--output", required=True, type=Path)
    args = p.parse_args(argv)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    convert(args.input, args.output)
    print(f"Wrote {args.output}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
