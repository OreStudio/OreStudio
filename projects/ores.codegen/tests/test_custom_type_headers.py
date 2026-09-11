"""Tests for the custom-type to header registry.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_custom_type_headers.py

A column declares its C++ type; the header defining that type used to be
stated separately in the entity's Domain includes block, with nothing
connecting the two. The registry in projects/modeling/cpp_custom_types.org
binds them, so declaring the type is enough.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import (  # noqa: E402
    _headers_for_types,
    _load_custom_type_headers,
    _with_registered_headers,
)

CRON = '"ores.scheduler.api/domain/cron_expression.hpp"'


def test_registry_loads():
    assert dict(_load_custom_type_headers())["cron_expression"] == CRON


def test_bare_type_resolves():
    assert _headers_for_types(["cron_expression"]) == [CRON]


def test_wrapped_type_resolves_to_the_same_header():
    """The wrapper is not what needs including."""
    assert _headers_for_types(["std::optional<cron_expression>"]) == [CRON]
    assert _headers_for_types(["std::vector<cron_expression>"]) == [CRON]


def test_unregistered_type_needs_nothing():
    assert _headers_for_types(["std::string", "int"]) == []


def test_a_longer_identifier_is_not_a_match():
    """Substring matching would bind any name containing a registered one."""
    assert _headers_for_types(["prefixed_cron_expression"]) == []
    assert _headers_for_types(["cron_expression_list"]) == []


def test_injection_adds_only_what_is_missing():
    includes = {"domain": ["<string>"]}
    _with_registered_headers(includes, [{"cpp_type": "cron_expression"}])
    assert includes["domain"] == ["<string>", CRON]


def test_injection_leaves_a_declared_header_alone():
    """What keeps regeneration byte-identical for models predating this."""
    includes = {"domain": [CRON, "<string>"]}
    _with_registered_headers(includes, [{"cpp_type": "cron_expression"}])
    assert includes["domain"] == [CRON, "<string>"]


def test_columns_without_types_are_ignored():
    includes = {"domain": ["<string>"]}
    _with_registered_headers(includes, [{"column": "id"}])
    assert includes["domain"] == ["<string>"]


def test_every_registered_header_exists_on_disk():
    """A registry entry pointing at a header nobody has is a broken promise."""
    missing = []
    for name, header in _load_custom_type_headers():
        if not header.startswith('"'):
            continue  # a standard library header, not a repo path
        rel = header.strip('"')
        # A part of a composite nests one level deeper than a simple
        # component: ores.scheduler/api/include/... against
        # ores.utility/include/...
        hits = (list((REPO_ROOT / "projects").glob(f"*/*/include/{rel}"))
                + list((REPO_ROOT / "projects").glob(f"*/include/{rel}")))
        if not hits:
            missing.append((name, header))
    assert missing == []
