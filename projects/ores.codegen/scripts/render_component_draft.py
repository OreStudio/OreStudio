#!/usr/bin/env python3
"""Render one component's models into a kept directory and leave it there.

A variant of ``check_component_drift.py --dry-run`` that does not delete what it
rendered. The drift check's dry run is the safe way to draft a model, but it
throws the rendered tree away, so the only thing a reader sees is the list of
paths that would change. This keeps the tree, so a draft model's generated files
can be read line by line before anything lands in the repository.

It writes nothing into the repository: every model goes through the same
``_generate_single`` the in-place mode uses, with ``output_root`` pointed at the
output directory.

    render_component_draft.py refdata-cpp /tmp/refdata-draft
    render_component_draft.py variability-cpp /tmp/var-draft ores.sql

The component is a catalogue slug. The address defaults to ``ores``, which
renders every technical space. Read-only with respect to the repository.
"""

import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects" / "ores.codegen" / "scripts"))

import check_component_drift as ccd  # noqa: E402


def main() -> int:
    if len(sys.argv) < 2:
        print(__doc__.strip(), file=sys.stderr)
        return 2

    component = sys.argv[1]
    if len(sys.argv) > 2:
        out_root = Path(sys.argv[2]).resolve()
        out_root.mkdir(parents=True, exist_ok=True)
    else:
        out_root = Path(tempfile.mkdtemp(prefix="ores-draft-render-"))
    address = sys.argv[3] if len(sys.argv) > 3 else "ores"

    ccd.configure(verbose=False)

    seed = REPO_ROOT / ".clang-format"
    if seed.is_file():
        (out_root / ccd._SEEDED_CLANG_FORMAT).write_bytes(seed.read_bytes())

    rc = ccd._render_components([component], address, out_root)
    print(f"\nrendered into: {out_root}")
    return rc


if __name__ == "__main__":
    sys.exit(main())
