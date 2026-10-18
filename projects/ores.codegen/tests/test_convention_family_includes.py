"""The apply fragment and its includes name the same repositories."""

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
FRAGMENT = ROOT / "projects/ores.refdata/core/src/service/convention_family_apply.inc"
INCLUDES = ROOT / "projects/ores.refdata/core/src/service/convention_family_apply_includes.inc"


def test_every_repository_the_fragment_calls_is_included():
    """A repository the fragment calls but does not include is a compile error."""
    called = set(re.findall(r"([a-z_]+_repository)\(\)", FRAGMENT.read_text()))
    included = set(
        re.findall(r"repository/([a-z_]+)\.hpp", INCLUDES.read_text(encoding="utf-8"))
    )
    assert called == included


def test_every_include_names_a_file_that_exists():
    for header in re.findall(r"repository/([a-z_]+)\.hpp", INCLUDES.read_text()):
        path = ROOT / "projects/ores.refdata/core/include/ores.refdata.core/repository"
        assert (path / f"{header}.hpp").exists(), header
