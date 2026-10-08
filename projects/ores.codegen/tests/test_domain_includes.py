"""A generated domain header declares the standard headers it uses, and no more.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_domain_includes.py

The domain class template emits whatever the entity model's ``* Domain
includes`` block names. Nothing checks that the list is true in either
direction.

Declared but unused: an entity whose members are all concrete can still declare
``<optional>``, and clang-format does not remove it, because it sorts includes
rather than reading them. Thirteen entities did exactly that.

Used but undeclared: the template emits ``std::chrono::system_clock::
time_point recorded_at`` for every entity with audit columns, whatever the
model says. 126 models never declared ``<chrono>``, so their headers compiled
only where the consuming translation unit happened to pull it in first. One
translation unit did not, and the build stopped there.

Only headers codegen still owns are checked, for the declared-but-unused rule:
an entity whose model is gone has no regeneration behind it, so its leftovers
are a different defect. The used-but-undeclared rule has no such exemption --
any generated header that names the type has to include it, whether a model
still produces it or not.
"""
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROJECTS = REPO_ROOT / "projects"
MARKER = "AUTO-GENERATED FILE"

# Header to the token that proves the header is used. Each entry is a standard
# header a domain header may declare and the one spelling that can only come
# from it.
USED_BY = {
    "#include <optional>": "std::optional",
}

# Token to the header that has to be declared for it. The template emits the
# member whether or not the model lists the header, so a model that forgets it
# produces a header that compiles only by accident of inclusion order.
NEEDED_BY = {
    "std::chrono::": "#include <chrono>",
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


def test_no_domain_header_uses_a_standard_type_it_does_not_declare():
    offenders = []
    for header in _domain_headers():
        text = header.read_text(encoding="utf-8", errors="replace")
        if MARKER not in text[:1500]:
            continue
        for token, include in NEEDED_BY.items():
            if token in text and include not in text:
                offenders.append(
                    f"{header.relative_to(REPO_ROOT)} uses {token} "
                    f"but does not declare {include}")

    assert offenders == []
