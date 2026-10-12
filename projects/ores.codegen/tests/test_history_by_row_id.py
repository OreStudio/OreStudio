"""Tests for how a generic history addresses an entity whose declared key is shared.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_history_by_row_id.py

A contact is declared by its type, and every party holds a contact of each type,
so the type alone names one contact per party and not one in the tenant. The
generic history of such an entity is addressed by the row's own id. An entity
whose declared key is the whole key keeps reading by it. These tests render the
real models through the provider template and pin that split.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"


def provider(tmp_path, model):
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(REPO_ROOT / "projects" / model),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_history_provider_registrar.cpp.mustache",
        target_output="out.txt",
    )
    return (output_dir / "out.txt").read_text(encoding="utf-8")


def test_a_contact_is_read_by_its_own_id(tmp_path):
    source = provider(tmp_path, "ores.refdata/modeling/ores.refdata.party_contact_information.org")

    assert "boost::lexical_cast<boost::uuids::uuid>(entity_id)" in source
    assert "get_party_contact_information_history(entity_id)" not in source


def test_an_identifier_is_read_by_its_own_id(tmp_path):
    source = provider(tmp_path, "ores.refdata/modeling/ores.refdata.party_identifier.org")

    assert "boost::lexical_cast<boost::uuids::uuid>(entity_id)" in source


def test_an_entity_whose_declared_key_is_the_whole_key_is_read_by_it(tmp_path):
    source = provider(tmp_path, "ores.refdata/modeling/ores.refdata.currency.org")

    assert "lexical_cast" not in source
    assert "_history(entity_id)" in source
