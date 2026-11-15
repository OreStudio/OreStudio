"""Tests for how a service reads a batch addressed by a declared key.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_declared_key_batch_read.py

A service whose declared key is not the storage key reads a batch with the
batch form of the finder by that key. The generated finder has one; a model
that states its own finder states the batch form too, or its service reads one
key at a time. These tests pin the detection and render the real models.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model, states_batch_finder  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"


def service(tmp_path, model):
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(REPO_ROOT / "projects" / model),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_service.cpp.mustache",
        target_output="out.txt",
    )
    return (output_dir / "out.txt").read_text(encoding="utf-8")


def test_a_batch_finder_is_found_whatever_its_layout():
    declaration = """
    std::vector<domain::account>
    read_latest_by_username( const context &ctx,
                             const std::vector< std::string > & names );
    """
    assert states_batch_finder(declaration, "username")


def test_a_single_finder_is_not_a_batch_finder():
    declaration = "read_latest_by_username(const context& ctx, const std::string& username);"
    assert not states_batch_finder(declaration, "username")


def test_a_batch_finder_by_another_key_does_not_count():
    declaration = "read_latest_by_email(const context& ctx, const std::vector<std::string>& emails);"
    assert not states_batch_finder(declaration, "username")


def test_a_model_that_states_the_batch_finder_reads_a_batch_with_it(tmp_path):
    source = service(tmp_path, "ores.iam/modeling/ores.iam.account.org")

    assert "repo.read_latest_by_username(ctx, username_keys)" in source
    assert "found[i] = account_read_one(repo, ctx, keys[i])" not in source


def test_a_generated_finder_reads_a_batch_with_its_batch_form(tmp_path):
    source = service(tmp_path, "ores.dq/modeling/ores.dq.methodology.org")

    assert "repo.read_latest_by_name(ctx, name_keys)" in source
    assert "_read_one(repo, ctx, keys[i])" not in source
