"""Regression test: a run scoped to one space still renders the facets that depend on another.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_protocol_dependency_scope.py

The protocol-dependency gate drops the facets that name a derived protocol's
request types when the model has no protocol. It asked that question of the
address-narrowed facet set rather than of the model's supported set, so the
answer depended on the address: a run scoped to ``ores.doc`` never contains
``ores.cpp.protocol``, so the gate fired and dropped ``ores.doc.shell-recipe``
as well. Every per-space run -- which is exactly what the drift check does --
therefore regenerated no shell recipe at all, and the family rotted against the
generator in silence: the checked-in scripts still omitted the session-party
positional and still sent ``__none__`` where the shell wants its absent token,
so 71 of refdata's 732 generated commands aborted.

This file pins the half that matters to refdata: that its recipe is reached
from every address, including the ones that cannot see the protocol. The gate's
own negative case -- a model that suppresses its protocol gets no recipe and no
shell unit -- is pinned in ``test_protocol_dependent_facets_gate.py``, against
a fixture. It used to be asserted here against a live dq junction, until that
junction was changed to keep its protocol; a gate's negative case pinned to a
live model is a statement about the tree, not about the gate.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402

CODEGEN_DIR = REPO_ROOT / "projects/ores.codegen"

# An entity that derives its protocol, so its shell unit and its recipe exist.
PROTOCOL_ENTITY = REPO_ROOT / "projects/ores.refdata/modeling/ores.refdata.book.org"
RECIPE_OUTPUT = "doc/recipes/shell/books/book.org"


def _outputs(model: Path, address: str) -> set[str]:
    units, _model_type, _data = resolve_targets(model, CODEGEN_DIR, address=address)
    return {unit["output"] for unit in units}


def test_a_run_scoped_to_the_doc_space_still_renders_the_shell_recipe():
    assert RECIPE_OUTPUT in _outputs(PROTOCOL_ENTITY, "ores.doc")


def test_the_recipe_facet_is_the_one_the_doc_address_selects():
    """The recipe is reached by ``ores.doc``, so no address can see it indirectly."""
    assert RECIPE_OUTPUT in _outputs(PROTOCOL_ENTITY, "ores.doc.shell-recipe")


def test_an_unscoped_run_still_renders_the_shell_recipe():
    assert RECIPE_OUTPUT in _outputs(PROTOCOL_ENTITY, "ores")

