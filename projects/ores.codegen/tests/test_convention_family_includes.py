"""Every convention family has a generated unit, and each unit reflects its own type."""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / "projects/ores.codegen/src"))

from codegen.convention_family_registry import discover_entries  # noqa: E402

SERVICE = ROOT / "projects/ores.refdata/core/src/service"


def _units():
    return {
        entry.entity: (SERVICE / f"convention_family_{entry.entity}.cpp").read_text()
        for entry in discover_entries(ROOT)
    }


def test_every_registry_family_has_a_generated_unit():
    for entry in discover_entries(ROOT):
        assert (SERVICE / f"convention_family_{entry.entity}.cpp").exists(), entry.entity


def test_every_unit_includes_the_repository_of_its_own_entity_and_no_other():
    for entity, text in _units().items():
        repositories = re.findall(r'#include "ores\.refdata\.core/repository/([a-z_]+)\.hpp"', text)
        assert repositories == [f"{entity}_repository"], entity


def test_every_unit_names_the_domain_type_of_its_own_family_and_no_other():
    for entry in discover_entries(ROOT):
        text = _units()[entry.entity]
        named = set(re.findall(r"ores::refdata::domain::[a-z_]+", text))
        assert named == {entry.domain_type}, entry.entity
