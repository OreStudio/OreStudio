#!/usr/bin/env python3
"""
Append template sections to a literate facet doc.

For each named template, appends to the facet org doc (before the
* See also section) a heading, a TODO prose placeholder, and a
mustache src block carrying the GENERATED FILE header plus the
template's current content, org-escaped (leading commas before lines
starting with '*' or '#+', which tangling strips again). The block
:tangle target is the template itself, so a follow-up tangle is
byte-identical by construction.

One-shot migration helper for the literate-templates rollout; idempotent
per template (skips names already present as :tangle targets).
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
TEMPLATES_DIR = REPO_ROOT / "projects/ores.codegen/library/templates"

HEADER = ("{{! GENERATED FILE — tangled from "
          "projects/ores.codegen/library/templates/%s. "
          "Edit the org source. }}")


def org_escape(text: str) -> str:
    """Comma-escape lines that org would parse as structure."""
    return re.sub(r"^(,*)(\*|#\+)", r",\1\2", text, flags=re.MULTILINE)


def section(doc_name: str, template: str) -> str:
    path = TEMPLATES_DIR / template
    body = path.read_text(encoding="utf-8")
    if body.startswith("{{! GENERATED FILE"):
        body = body.split("\n", 1)[1]  # already migrated once; drop header
    return (
        f"* TODO {template}\n\n"
        f"(TODO: role, model inputs, output artefact, profiles.)\n\n"
        f"#+begin_src mustache :tangle {template}\n"
        f"{HEADER % doc_name}\n"
        f"{org_escape(body).rstrip()}\n"
        f"#+end_src\n\n"
    )


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--facet", required=True,
                        help="Facet doc filename (e.g. assets.org).")
    parser.add_argument("templates", nargs="+",
                        help="Template filenames (e.g. plantuml_er.mustache).")
    args = parser.parse_args(argv)

    doc_path = TEMPLATES_DIR / args.facet
    text = doc_path.read_text(encoding="utf-8")

    new_sections = []
    for template in args.templates:
        if f":tangle {template}" in text:
            print(f"skip (already present): {template}")
            continue
        new_sections.append(section(args.facet, template))

    if not new_sections:
        print("nothing to do.")
        return 0

    marker = "* See also"
    if marker not in text:
        sys.exit(f"error: {doc_path} has no * See also section.")
    text = text.replace(marker, "".join(new_sections) + marker, 1)
    doc_path.write_text(text, encoding="utf-8")
    print(f"added {len(new_sections)} section(s) to {doc_path.name}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
