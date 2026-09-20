"""Tests for the handler-permission seed check.

The check only has value if it can fail. A generated handler whose permission
code no seed defines refuses every caller, so these tests pin both halves: the
code discovery must not come back empty, and a code missing from the seeds
must be reported.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_handler_permissions.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_handler_permissions as check  # noqa: E402


def test_every_handler_permission_is_seeded():
    assert check.check() == []


def test_code_discovery_is_not_vacuous():
    """A parser that found no codes would pass every tree."""
    required = check.required_codes("refdata")
    assert len(required) > 10, "no handler header yielded a permission code"
    assert any(
        "refdata::currency_countries:write" in codes
        for codes in required.values()
    )
    assert "refdata::currency_countries:write" in check.seeded_codes()


def test_a_hand_written_cpp_handler_is_covered():
    """Codegen writes its handlers as headers, and a component also carries
    hand-written ones under messaging/. A glob over headers alone left their
    codes invisible, so a code only they check could stay unseeded with the
    gate green."""
    required = check.required_codes("refdata")
    cpp = {
        path: codes for path, codes in required.items()
        if path.endswith("calendar_materialisation_handler.cpp")
    }
    assert cpp, "the hand-written materialisation handler was not discovered"
    assert "refdata::calendars:write" in next(iter(cpp.values()))


def test_a_code_missing_from_the_seeds_is_reported(tmp_path):
    partial = tmp_path / "iam_permissions_populate.sql"
    seeded = check.seeded_codes()
    dropped = "refdata::currency_countries:write"
    assert dropped in seeded
    partial.write_text(
        "\n".join(f"'{code}'" for code in sorted(seeded - {dropped})),
        encoding="utf-8",
    )

    violations = check.check(seed_path=partial)
    assert [(path.split("/")[-1], code) for path, code in violations] == [
        ("currency_country_handler.hpp", dropped)
    ]
