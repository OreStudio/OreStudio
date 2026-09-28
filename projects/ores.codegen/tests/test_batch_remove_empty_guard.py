"""Regression test: a batch removal of nothing is a no-op, not an empty IN ().

Run::

    python3 -m pytest projects/ores.codegen/tests/test_batch_remove_empty_guard.py

The generated batch ``remove`` built its statement straight from the id vector.
When the vector was empty the query builder rendered ``"id"_c.in({})`` as an
empty ``IN ()``, which PostgreSQL refuses as a syntax error, so the call
surfaced as ``internal_error`` rather than as nothing-to-do. The generated
service reaches that state by design: it resolves the keys a caller names and
skips the ones that match no row, so a batch whose keys all miss arrives at the
repository with an empty list. Every generated ``delete-many`` therefore
answered a syntax error instead of an answer when its keys matched nothing.

The batch *read* overloads already guarded the same case; this pins the guard
in the removal, and pins that it precedes the statement it protects.

The render is whitespace-flattened before it is searched, because the template's
own line breaks are not the contract: the guard has to run first, and how the
declaration wraps is clang-format's business, not this test's.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_DIR = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_DIR / "library" / "data"
TEMPLATES_DIR = CODEGEN_DIR / "library" / "templates"

# A live surrogate-keyed entity: its batch overloads take one id vector, which
# is the shape that rendered the empty IN ().
BASE_ENTITY = REPO_ROOT / "projects/ores.refdata/modeling/ores.refdata.book.org"

BATCH_REMOVE = ("book_repository::remove( context ctx, "
                "const std::vector<std::string>& ids) {")
BATCH_READ = ("book_repository::read_latest( context ctx, "
              "const std::vector<std::string>& ids) {")

# Enough of the body to hold the guard and the statement it protects, and not
# so much that the next overload is drawn in.
WINDOW = 1500


def _render(tmp_path) -> str:
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    output_name = "book_repository.cpp"
    generate_from_model(
        str(BASE_ENTITY),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_domain_type_repository.cpp.mustache",
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def _body(rendered: str, signature: str) -> str:
    flat = re.sub(r"\s+", " ", rendered)
    start = flat.index(signature)
    return flat[start : start + WINDOW]


def test_a_batch_removal_of_nothing_returns_before_building_the_statement(tmp_path):
    body = _body(_render(tmp_path), BATCH_REMOVE)
    guard = re.search(r"if \(ids\.empty\(\)\) return;", body)
    assert guard, "the batch removal has no empty-vector guard"
    assert guard.start() < body.index("delete_from"), (
        "the guard must run before the statement is built, or the empty IN () "
        "is rendered on the way to it")


def test_the_batch_read_keeps_its_own_guard(tmp_path):
    body = _body(_render(tmp_path), BATCH_READ)
    assert re.search(r"if \(ids\.empty\(\)\) return \{\};", body)
