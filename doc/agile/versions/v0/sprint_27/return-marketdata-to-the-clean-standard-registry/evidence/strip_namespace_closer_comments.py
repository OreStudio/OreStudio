#!/usr/bin/env python3
"""Remove the end-of-line comment from namespace closers in generated C++.

The project comment rules (H03) forbid end-of-line comments, and fourteen
shared templates end every namespace with one: `} // namespace ores::x`. This
rewrites such a line to `}` in:

  * every template source and tangled template that has one, under
    projects/ores.codegen/library/templates/;
  * every generated file that has one. On 2026-10-05 every such file named
    one of those templates in its `Template:` header.

A hand-written file is never touched, and nor is a file in EXCLUDED. Each
file in EXCLUDED already drifts from its template for other reasons. If this
script edited it, the catalogue sweep would hold this change responsible for
that drift. Its component's clean-up story regenerates it. The templates no
longer write the comment, so that regeneration removes the comment. Applying the change to the generated
files, rather than regenerating them, keeps other components' unrelated drift
out of the change; check_component_drift.py then proves the edited files match
a fresh render.

Run from the repository root:

    python3 <this file>            # rewrite
    python3 <this file> --check    # report, change nothing; exit 1 if any remain
"""
import re
import subprocess
import sys
from pathlib import Path

TEMPLATES = Path("projects/ores.codegen/library/templates")
GENERATED_MARKER = "AUTO-GENERATED FILE - DO NOT EDIT MANUALLY"
CLOSER = re.compile(r"^\}[ \t]*// namespace.*$", re.M)
EXCLUDED = frozenset({
    "projects/ores.scheduler/core/src/service/job_definition_service.cpp",
    "projects/ores.synthetic/core/src/service/folder_service.cpp",
    "projects/ores.synthetic/core/src/service/fx_spot_generation_config_service.cpp",
    "projects/ores.synthetic/core/src/service/gmm_component_service.cpp",
    "projects/ores.synthetic/core/src/service/ir_curve_generation_config_process_parameter_value_service.cpp",
    "projects/ores.synthetic/core/src/service/ir_curve_generation_config_service.cpp",
    "projects/ores.synthetic/core/src/service/ir_curve_template_entry_service.cpp",
    "projects/ores.synthetic/core/src/service/market_data_generation_config_service.cpp",
    "projects/ores.synthetic/core/src/service/yield_curve_process_parameter_definition_service.cpp",
    "projects/ores.synthetic/core/src/service/yield_curve_process_type_service.cpp",
})


def read(path):
    return path.read_text(encoding="utf-8", errors="surrogateescape")


def write(path, text):
    path.write_text(text, encoding="utf-8", errors="surrogateescape")


def files_with_closers(paths):
    return [p for p in paths if CLOSER.search(read(p))]


def main(check):
    template_files = files_with_closers(sorted(TEMPLATES.glob("*.org")) +
                                        sorted(TEMPLATES.glob("*.mustache")))

    tracked = subprocess.run(
        ["git", "grep", "-l", "-E", r"^\}[[:space:]]*// namespace", "--",
         "projects/*.cpp", "projects/*.hpp"],
        capture_output=True, text=True, check=False).stdout.split()
    generated = []
    for name in tracked:
        if name in EXCLUDED:
            continue
        p = Path(name)
        if GENERATED_MARKER in read(p):
            generated.append(p)

    targets = template_files + generated
    lines = sum(len(CLOSER.findall(read(p))) for p in targets)
    print(f"{len(template_files)} template files, {len(generated)} generated files, "
          f"{lines} closer comments")
    if check:
        return 1 if lines else 0
    for p in targets:
        write(p, CLOSER.sub("}", read(p)))
    return 0


if __name__ == "__main__":
    sys.exit(main("--check" in sys.argv[1:]))
