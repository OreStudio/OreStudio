#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#

"""
Service Registry Invariant Validator

Enforces the strict service table isolation invariant:

  Every domain service may only hold database privileges on its own tables
  (those prefixed ores_<service>_).  Cross-component access is permitted only
  during the tracked migration windows declared in migration_exception entries
  in the service registry JSON.

Rules:
  - dml_prefixes: each prefix must start with ores_<service>_
  - select_tables: each table must start with ores_<service>_
  - select_prefixes: each prefix must start with ores_<service>_
  - Any entry that violates the above MUST carry a migration_exception with
    both an 'until' and a 'reason' field.
  - Services with "role": "tooling" are exempt from all checks.

Exit codes:
  0 — no violations
  1 — one or more unannotated cross-component entries found
"""

import json
import sys
from pathlib import Path


def _own_prefix(service_name: str) -> str:
    return f"ores_{service_name}_"


def _has_valid_exception(entry: dict) -> bool:
    exc = entry.get("migration_exception")
    if not exc:
        return False
    return bool(exc.get("until")) and bool(exc.get("reason"))


def validate_service(service: dict) -> tuple[list[str], list[str]]:
    """Return (warnings, errors) for one service entry."""
    warnings = []
    errors = []
    name = service.get("name", "<unknown>")
    own = _own_prefix(name)

    def check_entry(kind: str, key: str, entry: dict):
        value = entry.get(key, "")
        if value.startswith(own):
            return
        # Cross-component access detected
        if _has_valid_exception(entry):
            exc = entry["migration_exception"]
            warnings.append(
                f"  [{name}] {kind} '{value}': tracked exception until {exc['until']}"
                f" — {exc['reason']}"
            )
        else:
            errors.append(
                f"  [{name}] {kind} '{value}': cross-component access without"
                f" migration_exception (add until+reason or remove the entry)"
            )

    for entry in service.get("dml_prefixes", []):
        check_entry("dml_prefix", "prefix", entry)

    for entry in service.get("select_tables", []):
        check_entry("select_table", "table", entry)

    for entry in service.get("select_prefixes", []):
        check_entry("select_prefix", "prefix", entry)

    return warnings, errors


def validate(registry_path: Path) -> bool:
    """Validate the service registry.  Returns True if clean (no errors)."""
    with open(registry_path, encoding="utf-8") as f:
        data = json.load(f)

    services = data.get("service_registry", {}).get("services", [])

    all_warnings: list[str] = []
    all_errors: list[str] = []

    for service in services:
        if service.get("role") == "tooling":
            continue
        w, e = validate_service(service)
        all_warnings.extend(w)
        all_errors.extend(e)

    if all_warnings:
        print("Service registry: tracked migration exceptions (expected, will be removed per plan):")
        for msg in all_warnings:
            print(msg)

    if all_errors:
        print("\nService registry: INVARIANT VIOLATIONS (cross-component access without annotation):")
        for msg in all_errors:
            print(msg)
        print(
            "\nTo fix: add migration_exception: {until: 'phase-N', reason: '...'} to each"
            " flagged entry, or remove the entry if the cross-component access is not needed."
        )
        return False

    if not all_warnings:
        print("Service registry: all services pass strict table isolation invariant.")

    return True


def main():
    import argparse

    parser = argparse.ArgumentParser(
        description="Validate service registry strict table isolation invariant"
    )
    parser.add_argument(
        "registry_path",
        nargs="?",
        default=None,
        help="Path to the service registry JSON file",
    )
    args = parser.parse_args()

    if args.registry_path:
        registry_path = Path(args.registry_path)
    else:
        # Default: relative to this script's location
        registry_path = (
            Path(__file__).parent.parent
            / "models"
            / "services"
            / "ores_services_service_registry.json"
        )

    if not registry_path.exists():
        print(f"Error: registry file not found: {registry_path}", file=sys.stderr)
        sys.exit(1)

    ok = validate(registry_path)
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
