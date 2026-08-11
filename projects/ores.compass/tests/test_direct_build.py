"""
Tests for `compass build --direct` target resolution.

Run with:  python -m pytest projects/ores.compass/tests/test_direct_build.py -v
No live database or file system access required.
"""

import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from compass import BUILD_TARGET_ALIASES, EMACS_BUILD_SCRIPTS  # noqa: E402


def test_every_alias_resolves_to_a_direct_build_target():
    """A dangling alias makes `build --direct <alias>` fail at runtime,
    so each alias must name a target the emacs dispatcher knows."""
    for alias, target in BUILD_TARGET_ALIASES.items():
        assert target in EMACS_BUILD_SCRIPTS, (
            f"{alias} -> {target} is not a direct-build target"
        )


def test_codegen_templates_alias():
    """pr-raise's codegen-drift step runs `build --direct codegen_templates`."""
    assert BUILD_TARGET_ALIASES["codegen_templates"] == "tangle_codegen_templates"
