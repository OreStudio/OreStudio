"""Every tangled template matches the literate source it comes from.

The .mustache files in the templates directory are generated. The .org file
carries the source in a =#+begin_src mustache :tangle <file>= block, and
=compass build --direct tangle_codegen_templates= writes it out. An edit to the
.mustache is therefore lost on the next tangle, silently: the file looks changed,
the regeneration looks like it did nothing, and the gate that did not fire looks
broken.

This compares the two, so the mistake fails a test instead of costing a build.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_tangle_parity.py
"""
import re
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
TEMPLATES = REPO_ROOT / "projects/ores.codegen/library/templates"

TANGLE_RE = re.compile(
    r"^#\+begin_src\s+mustache\s+:tangle\s+(\S+)\s*$(.*?)^#\+end_src\s*$",
    re.MULTILINE | re.DOTALL)


def tangled_blocks():
    """Every (source, tangle target, block text) for the C++ templates.

    The documentation templates (=doc_*.mustache=) tangle through a different
    pass, so the rule here is stated for the code templates: the ones this
    generator renders and a build regenerates.
    """
    for org in sorted(TEMPLATES.glob("*.org")):
        text = org.read_text(encoding="utf-8")
        # One target may be assembled from several blocks in document order.
        joined: dict[str, str] = {}
        for target, block in TANGLE_RE.findall(text):
            if target.startswith("cpp_"):
                joined[target] = joined.get(target, "") + block
        for target, block in joined.items():
            yield org, target, block


def test_the_directory_declares_at_least_one_tangled_template():
    assert len(list(tangled_blocks())) > 50


@pytest.mark.parametrize("org,target,block", list(tangled_blocks()),
                         ids=lambda v: v.name if isinstance(v, Path) else str(v))
def test_the_tangled_file_is_what_the_source_states(org, target, block):
    tangled = TEMPLATES / target
    assert tangled.exists(), f"{org.name} tangles {target}, which is not on disk"
    # Org writes the block body and one newline that ends the block, and mustache
    # keeps no trailing blank line, so compare with the block's own edges trimmed.
    stated = block.lstrip("\n")
    on_disk = tangled.read_text(encoding="utf-8")
    assert on_disk == stated, (
        f"{target} is not what {org.name} says it is; run "
        f"compass build --direct tangle_codegen_templates")
