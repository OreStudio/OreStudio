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
"""Write the artefact rows of the DQ dataset orgmode.knowledge_pages.

The dataset is ORE Studio's user-facing knowledge base: every org page under
doc/ whose frontmatter says #+audience: user, keyed by its org id, with its path
and its text unchanged. The rows are generated, never committed: the pages
change with every edit, so a committed seed would churn. compass db recreate
runs this script and passes the file it writes to the populate as the psql
variable knowledge_pages_populate.

    generate_knowledge_pages_dataset.py               write the default output
    generate_knowledge_pages_dataset.py --output F    write F

Every page written is listed, so the set that reaches every tenant can be
reviewed. A user-facing page without an org id, or two pages with one id, fail
the run: either would publish a page no link can reach.
"""

import argparse
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
DEFAULT_OUTPUT = ROOT / "build" / "output" / "generated" / "sql" / \
    "orgmode_knowledge_pages_artefact_populate.sql"
DATASET = "orgmode.knowledge_pages"
AUDIENCE = re.compile(r"^#\+audience:\s*user\s*$", re.M | re.I)
ORG_ID = re.compile(r"\A\s*:PROPERTIES:\s*\n(?:.*\n)*?\s*:ID:\s*(\S+)\s*\n", re.I)


def pages(root: Path) -> list[tuple[str, str, str]]:
    """Every user-facing page: its org id, its path from the root, its text."""
    found, seen, errors = [], {}, []
    for path in sorted((root / "doc").rglob("*.org")):
        text = path.read_text(encoding="utf-8")
        if not AUDIENCE.search(text):
            continue
        rel = path.relative_to(root).as_posix()
        match = ORG_ID.match(text)
        if not match:
            errors.append(f"{rel}: declares #+audience: user and has no :ID:")
            continue
        org_id = match.group(1).upper()
        if org_id in seen:
            errors.append(f"{rel}: has the id {org_id} of {seen[org_id]}")
            continue
        seen[org_id] = rel
        found.append((org_id, rel, text))
    if errors:
        sys.exit("\n".join(errors))
    return found


def quoted(text: str) -> str:
    """The text as a dollar-quoted literal whose tag the text does not hold."""
    n = 0
    while f"$kp{n}$" in text:
        n += 1
    return f"$kp{n}${text}$kp{n}$"


def render(rows: list[tuple[str, str, str]]) -> str:
    out = [
        "-- AUTO-GENERATED FILE - DO NOT EDIT MANUALLY",
        "-- Written by projects/ores.sql/utility/generate_knowledge_pages_dataset.py",
        f"-- The artefact rows of the DQ dataset {DATASET}: {len(rows)} pages.",
        "",
        "do $outer$",
        "declare",
        "    v_dataset_id uuid;",
        "begin",
        "    select id into v_dataset_id",
        "    from ores_dq_datasets_tbl",
        f"    where code = '{DATASET}'",
        "      and valid_to = ores_utility_infinity_timestamp_fn();",
        "",
        "    if v_dataset_id is null then",
        f"        raise exception 'Dataset not found: {DATASET}';",
        "    end if;",
        "",
        "    delete from ores_dq_knowledge_pages_artefact_tbl",
        "    where dataset_id = v_dataset_id;",
        "end $outer$;",
        "",
    ]
    for org_id, rel, text in rows:
        out += [
            "insert into ores_dq_knowledge_pages_artefact_tbl",
            "    (dataset_id, tenant_id, id, version, path, org)",
            "select d.id, ores_utility_system_tenant_id_fn(),",
            f"       '{org_id}', 0, '{rel}', {quoted(text)}",
            "from ores_dq_datasets_tbl d",
            f"where d.code = '{DATASET}'",
            "  and d.valid_to = ores_utility_infinity_timestamp_fn();",
            "",
        ]
    return "\n".join(out)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    args = parser.parse_args()
    rows = pages(ROOT)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(render(rows), encoding="utf-8")
    for org_id, rel, _ in rows:
        print(f"  {org_id}  {rel}")
    print(f"Wrote {args.output}: {len(rows)} knowledge pages")
    return 0


if __name__ == "__main__":
    sys.exit(main())
