"""A generated domain header declares only the standard headers it uses.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_domain_includes.py

The domain class template emits whatever the entity model's ``* Domain
includes`` block names. Nothing checks that the list is true: an entity whose
members are all concrete can still declare ``<optional>``, and clang-format
does not remove it, because it sorts includes rather than reading them.

Thirteen entities did exactly that, and the header they produced included a
header no member needed.

Only headers codegen still owns are checked. An entity whose model is gone has
no regeneration behind it, so its leftovers are a different defect: the header
is compiled and tested but nothing produces it.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROJECTS = REPO_ROOT / "projects"

# Header to the token that proves the header is used. Each entry is a standard
# header a domain header may declare and the one spelling that can only come
# from it.
USED_BY = {
    "#include <optional>": "std::optional",
}


def _entity_models() -> set:
    """The entity names that still have a model producing a domain header."""
    names = set()
    for org in PROJECTS.rglob("*/modeling/*.org"):
        stem = org.stem
        if stem.startswith("ores."):
            names.add(stem.rsplit(".", 1)[-1])
    return names


def _domain_headers():
    for header in PROJECTS.rglob("*/domain/*.hpp"):
        if "include" in header.parts:
            yield header


def test_no_domain_header_declares_a_standard_header_it_does_not_use():
    owned = _entity_models()
    offenders = []
    for header in _domain_headers():
        if header.stem not in owned:
            continue
        text = header.read_text(encoding="utf-8", errors="replace")
        for include, token in USED_BY.items():
            if include in text and token not in text:
                offenders.append(
                    f"{header.relative_to(REPO_ROOT)} declares {include} "
                    f"but uses no {token}")

    assert offenders == []
