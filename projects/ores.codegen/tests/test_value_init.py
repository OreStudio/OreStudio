"""Tests for the value initialiser the C++ member templates emit.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_value_init.py

A scalar member with no initialiser is indeterminate on default construction,
so a serialiser writes uninitialised bytes.
"""
import re
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import _mark_value_init  # noqa: E402

GENERATED_HEADER = re.compile(r"/(domain|messaging|repository)/")
BARE_SCALAR_MEMBER = re.compile(
    r"^\s+(?:std::)?(?:u?int(?:8|16|32|64)_t|size_t|ptrdiff_t|int|long|short"
    r"|unsigned(?: int| long)?|bool|char|double|float)\s+[a-z_][a-z0-9_]*;\s*$",
    re.MULTILINE,
)


@pytest.mark.parametrize("cpp_type", [
    "bool", "int", "unsigned int", "std::uint32_t", "std::size_t", "double"])
def test_a_scalar_field_is_value_initialised(cpp_type):
    field = {"name": "x", "cpp_type": cpp_type}
    _mark_value_init({"fields": [field]}, set())
    assert field["cpp_value_init"] == "{}"


def test_a_class_type_field_gets_an_empty_key_to_stop_the_context_walk():
    field = {"name": "x", "cpp_type": "std::string"}
    _mark_value_init({"fields": [field]}, set())
    assert field["cpp_value_init"] == ""


def test_no_generated_header_declares_a_bare_scalar_member():
    offenders = []
    for path in (REPO_ROOT / "projects").glob("ores.*/**/*.hpp"):
        text = path.read_text(errors="replace")
        generated = "generated" in text[:1500].lower()
        if (generated and GENERATED_HEADER.search(path.as_posix())
                and "/tests/" not in path.as_posix()
                and BARE_SCALAR_MEMBER.search(text)):
            offenders.append(str(path.relative_to(REPO_ROOT)))
    assert offenders == []
