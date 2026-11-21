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
"""Every service part is in the service registry.

The registry, projects/modeling/service_registry.org, is the one list every
deployment artefact reads: the systemd units, the container images, the
.env database credentials, the database roles and grants, the expected
services, and the NATS certificates. A service part the registry does not
list builds and passes its tests, and then is never started, deployed or
given a database role or a certificate. A part is a service when its
component overview says #+component_kind: service.

Exit 0 when every service part is registered, 1 naming each that is not.
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REGISTRY = ROOT / "projects" / "modeling" / "service_registry.org"


def registered_services(registry: Path) -> set[str]:
    """The registry's entries: each is a top-level heading named for its binary."""
    return set(re.findall(r"^\* (\S+)\s*$", registry.read_text(encoding="utf-8"), re.M))


def service_parts(projects: Path) -> dict[str, Path]:
    """Every part whose overview declares it a service, by its full name."""
    parts = {}
    for overview in projects.glob("*/*/modeling/component_overview.org"):
        text = overview.read_text(encoding="utf-8")
        if not re.search(r"^#\+component_kind:\s*service\s*$", text, re.M):
            continue
        name = re.search(r"^#\+full_name:\s*(\S+)\s*$", text, re.M)
        if name:
            parts[name.group(1)] = overview
    return parts


def main() -> int:
    registered = registered_services(REGISTRY)
    missing = {name: path for name, path in service_parts(ROOT / "projects").items()
               if name not in registered}
    for name, path in sorted(missing.items()):
        print(f"{path.relative_to(ROOT)}: service {name} is not in "
              f"{REGISTRY.relative_to(ROOT)}")
    if missing:
        print(f"{len(missing)} service part(s) not registered")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
