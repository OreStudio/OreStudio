#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil; python-indent-offset: 4 -*-
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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""Every permission the operation catalogue states is one a generated handler
checks.

A generated entity route states the permission its handler checks by a rule,
and the MCP tools hide the operation from a token that lacks it. If a handler
changes its check, the rule would hide or show an operation wrongly and nothing
else would fail. This reads the running server's OpenAPI document and every
has_permission check in the generated handlers, and fails when a stated
permission is one no handler checks.

Usage: check_route_permissions.py <repository root>; the server named by the
root's .env must be running.
"""
import json
import pathlib
import re
import sys
import urllib.request


def main() -> int:
    repo = pathlib.Path(sys.argv[1])
    port = next(line.split("=", 1)[1].strip().strip('"')
                for line in (repo / ".env").read_text().splitlines()
                if line.removeprefix("export ").startswith("ORES_HTTP_PORT="))
    checked = set()
    for header in repo.glob("projects/*/core/include/*/messaging/*_handler.hpp"):
        checked |= set(re.findall(r'has_permission\(req_ctx, "([^"]+)"\)', header.read_text()))
    with urllib.request.urlopen(f"http://localhost:{port}/openapi.json", timeout=30) as answer:
        document = json.load(answer)
    stated = 0
    wrong = []
    for path, operations in document["paths"].items():
        for method, operation in operations.items():
            permission = operation.get("x-ores-permission") if isinstance(operation, dict) else None
            if not permission:
                continue
            stated += 1
            if permission not in checked:
                wrong.append(f"{method.upper()} {path} states {permission}")
    print(f"{stated} operations state a permission; {len(wrong)} name one no handler checks")
    for line in wrong:
        print("  " + line)
    return 1 if wrong else 0


if __name__ == "__main__":
    sys.exit(main())
