#!/usr/bin/env python3
"""Check that no row-level security policy lets the system tenant read or
write every tenant's rows, unless it is a reviewed exception.

The token names the tenant and row-level security scopes every read from it.
A policy that adds "or the session is the system tenant" lets a service's own
identity see every tenant, and makes the system tenant's own reads list every
tenant's rows. A service that needs a tenant's data acts inside that tenant,
with a token for it (Token Exchange), instead.

*This guard flags one shape, not every mention of the system tenant.* A row
belongs to a tenant, to the system tenant, or to the installation, and the
three want different policies -- see
doc/knowledge/architecture/tenant_ownership_and_the_information_flow.org.

- ``tenant_id = current or tenant_id = system`` admits only the system
  tenant's own rows to everyone: the shared-reference and
  installation-scoped shape. This guard does not flag it, and should not.
- ``tenant_id = current or current = system`` admits every tenant's rows to
  a system-tenant session. This guard flags it.

The difference is which side of the comparison the *session* sits on.

Each policy that still widens is named below with the reason it stays, until
its audit decides. A new widening fails this check.

The check is a guard, not a proof: it matches two spellings of the widening.
A policy that widens with IN (...), with a negation, or through a helper that
wraps the comparison passes it.

Run::

    python3 projects/ores.codegen/scripts/check_rls_system_tenant_widenings.py
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
CREATE_DIR = REPO_ROOT / "projects" / "ores.sql" / "create"

AUDIT = "under audit in 'Audit the policies that let the system tenant read every tenant'"

EXCEPTIONS = {
    "job_definitions_tenant_isolation_policy": AUDIT,
    "job_definitions_party_isolation_policy": AUDIT,
    "job_instances_write_policy": AUDIT,
    "job_instances_party_isolation_policy": AUDIT,
    "job_instances_read_policy": AUDIT,
    "market_data_generation_configs_tenant_isolation_policy": AUDIT,
    "workflow_steps_tenant_isolation_policy": AUDIT,
    "workflow_instances_tenant_isolation_policy": AUDIT,
    "workflow_batch_links_tbl_tenant_isolation_policy": AUDIT,
    "feed_bindings_tbl_tenant_isolation_policy": AUDIT,
}

POLICY_RE = re.compile(r"create\s+policy\s+(\w+)\s+on\s+(\w+)(.*?);", re.I | re.S)
WIDENING_RE = re.compile(
    r"ores_iam_current_tenant_id_fn\(\)\s*=\s*ores_utility_system_tenant_id_fn\(\)"
    r"|ores_utility_system_tenant_id_fn\(\)\s*=\s*ores_iam_current_tenant_id_fn\(\)",
    re.I)


def strip_comments(sql):
    return re.sub(r"--[^\n]*", "", sql)


def widenings(create_dir=CREATE_DIR):
    """Every (policy, table, file) whose body admits the system tenant."""
    found = []
    for path in sorted(create_dir.rglob("*.sql")):
        sql = strip_comments(path.read_text(encoding="utf-8"))
        for match in POLICY_RE.finditer(sql):
            name, table, body = match.groups()
            if WIDENING_RE.search(body):
                found.append((name, table, path.relative_to(REPO_ROOT)))
    return found


def main():
    found = widenings()
    unlisted = [w for w in found if w[0] not in EXCEPTIONS]
    stale = sorted(set(EXCEPTIONS) - {w[0] for w in found})
    for name, table, path in unlisted:
        print(f"error: {path}: policy {name} on {table} lets a system-tenant "
              f"session see every tenant, which hands one tenant's rows to "
              f"another. Act inside the tenant instead, or list it with its "
              f"reason. A policy that admits only the system tenant's own "
              f"rows does not have this shape and is not flagged.")
    for name in stale:
        print(f"error: exception {name} names no widening policy; remove it")
    if unlisted or stale:
        return 1
    print(f"ok: {len(found)} system tenant widening(s), all listed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
