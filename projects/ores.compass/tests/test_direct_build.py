"""
Tests for `compass build --direct` target resolution.

Run with:  python -m pytest projects/ores.compass/tests/test_direct_build.py -v
No live database or file system access required.
"""

import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import pytest  # noqa: E402

from compass import (  # noqa: E402
    BUILD_TARGET_ALIASES,
    EMACS_BUILD_SCRIPTS,
    SETTINGS_SURFACES,
    resolve_build_targets,
)


def test_every_alias_resolves_to_a_direct_build_target():
    """A dangling alias makes `build --direct <alias>` fail at runtime,
    so each alias must name a target the emacs dispatcher knows."""
    for alias, target in BUILD_TARGET_ALIASES.items():
        assert target in EMACS_BUILD_SCRIPTS, (
            f"{alias} -> {target} is not a direct-build target"
        )


def test_codegen_templates_alias():
    """compass-pr-raise's codegen-drift step runs `build --direct codegen_templates`."""
    assert BUILD_TARGET_ALIASES["codegen_templates"] == "tangle_codegen_templates"


def test_settings_deploys_every_surface_by_default():
    """Bare `build settings` deploys every surface, so a fresh checkout gets
    both harnesses from one command."""
    assert resolve_build_targets(["settings"]) == list(SETTINGS_SURFACES.values())


def test_settings_surface_flags_narrow_the_alias():
    assert resolve_build_targets(["settings"], claude=True) == ["deploy_settings"]
    assert resolve_build_targets(["settings"], dsh=True) == ["deploy_dsh_settings"]
    assert resolve_build_targets(["settings"], claude=True, dsh=True) == \
        list(SETTINGS_SURFACES.values())


def test_every_surface_deploys_a_direct_build_target():
    """A surface with no emacs script fails only when it is deployed."""
    for target in SETTINGS_SURFACES.values():
        assert target in EMACS_BUILD_SCRIPTS, (
            f"{target} is not a direct-build target"
        )


def test_surface_flag_without_the_settings_alias_is_refused():
    """Ignoring it would report a successful build of a different target from
    the one the caller narrowed to."""
    with pytest.raises(ValueError):
        resolve_build_targets(["ores.storage.tests"], dsh=True)
