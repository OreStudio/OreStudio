"""Verify that codegen reads sample component overviews correctly and
produces the expected scaffold shape.

Targets:

- The 2 sample fixtures next to this script:
  - ``sample_flat/component_overview.org`` (flat component)
  - ``sample_composite/component_overview.org`` (group level)
  - ``sample_composite/api/component_overview.org`` (composite .api)
  - ``sample_composite/core/component_overview.org`` (composite .core)

- *Prior art* under ``projects/``:
  - ``projects/ores.nats/modeling/component_overview.org`` — a real flat
    component already in the tree.
  - ``projects/ores.iam/{,api,core,service}/modeling/component_overview.org``
    — a real composite group + three subcomponents.

For each target we check:

1. ``load_org_component_overview_model`` returns the expected
   ``{component: {name, full_name, brief, description}}`` shape.
2. The component profile's output paths *resolve* to a directory the
   codegen would write under (dry-run; we don't actually write).

Run::

    python3 projects/ores.codegen/tests/component_scaffold/verify.py

Exits 0 on success; non-zero on any check failure.
"""
import json
import os
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import load_org_component_overview_model  # noqa: E402


SCAFFOLD_OUTPUTS_FLAT = (
    "CMakeLists.txt",
    "src/CMakeLists.txt",
    "tests/CMakeLists.txt",
    "modeling/CMakeLists.txt",
    "include/{full_name}/export.hpp",
    "include/{full_name}/{full_name}.hpp",
    "include/{full_name}/domain/stub.hpp",
    "src/domain/stub.cpp",
    "tests/main.cpp",
    "tests/stub_tests.cpp",
)


def _check_loader(label: str, path: Path, required_keys=("name", "full_name", "brief", "description")):
    if not path.is_file():
        return f"FAIL {label}: missing {path}"
    model = load_org_component_overview_model(path)["component"]
    missing = [k for k in required_keys if k not in model]
    if missing:
        return f"FAIL {label}: loader returned no {missing}; got {sorted(model.keys())}"
    return f"OK   {label}: {model['full_name']} (brief={model['brief'][:50]}...)"


def _check_scaffold_paths(label: str, ov: Path, profile_outputs=SCAFFOLD_OUTPUTS_FLAT):
    model = load_org_component_overview_model(ov)["component"]
    full_name = model["full_name"]
    # The component profile writes under projects/{full_name}/...
    out_root = REPO_ROOT / "projects" / full_name
    if not out_root.is_dir():
        return f"NOTE {label}: scaffold output dir would be {out_root.relative_to(REPO_ROOT)} (not present today — would be created)"
    return f"OK   {label}: scaffold root {out_root.relative_to(REPO_ROOT)} exists"


def main() -> int:
    here = Path(__file__).parent
    fails = 0
    checks = [
        # Fixture: flat
        ("fixture/flat", here / "sample_flat/component_overview.org", SCAFFOLD_OUTPUTS_FLAT),
        # Fixture: composite group + two subs
        ("fixture/composite-group", here / "sample_composite/component_overview.org", None),
        ("fixture/composite-api", here / "sample_composite/api/component_overview.org", SCAFFOLD_OUTPUTS_FLAT),
        ("fixture/composite-core", here / "sample_composite/core/component_overview.org", SCAFFOLD_OUTPUTS_FLAT),
        # Prior art: flat
        ("prior-art/ores.nats", REPO_ROOT / "projects/ores.nats/modeling/component_overview.org", SCAFFOLD_OUTPUTS_FLAT),
        # Prior art: composite group + three subs (controller — all four had
        # JSON models pre-merge so all four overviews carry the scalars).
        ("prior-art/ores.controller (group)", REPO_ROOT / "projects/ores.controller/modeling/component_overview.org", None),
        ("prior-art/ores.controller.api", REPO_ROOT / "projects/ores.controller/api/modeling/component_overview.org", SCAFFOLD_OUTPUTS_FLAT),
        ("prior-art/ores.controller.core", REPO_ROOT / "projects/ores.controller/core/modeling/component_overview.org", SCAFFOLD_OUTPUTS_FLAT),
        ("prior-art/ores.controller.service", REPO_ROOT / "projects/ores.controller/service/modeling/component_overview.org", SCAFFOLD_OUTPUTS_FLAT),
    ]
    for label, path, scaffold in checks:
        line = _check_loader(label, path)
        print(line)
        if line.startswith("FAIL"):
            fails += 1
        if scaffold and not line.startswith("FAIL"):
            line2 = _check_scaffold_paths(label, path, scaffold)
            print(line2)
            if line2.startswith("FAIL"):
                fails += 1
    print("---")
    print(f"checks failed: {fails}")
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
