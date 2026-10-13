"""Tests for the staging-only facet gate in ``resolve_targets``.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_staging_only_facets_gate.py

A model that binds ``artefact-staging-only`` is the staging table some
component's entity is imported through. Where that entity belongs to *another*
component, the shim must render no user-facing shell command unit: the shell
names a menu after the entity's plural, so the shim generates the same menu as
the entity it stages, and ``cli::Menu::Insert`` accepts the duplicate while
dispatch answers from whichever menu registered first.

No model in the tree triggers the gate now: the dq shims that copied another
component's entity were moved onto their owners, as the DQ artefact guide
requires. The gate stays so that such a shim cannot come back silently, and the
test builds one: a copy of a dq staging shim whose plural is one that refdata
models.

Two controls keep the gate honest. A dq entity that is not a staging shim keeps
its shell unit, so the gate cannot pass by dropping the shell facet everywhere.
And a staging shim that is its entity's *only* owner keeps its unit too --
`lei_entity` and two others bind the same profile, dq owns them, and their
command units are the only surface those entities have. Suppressing those would
trade a collision for a lost capability.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"

# A dq staging shim, copied with a plural another component also models.
STAGING_SHIM = REPO_ROOT / "projects/ores.dq/modeling/ores.dq.lei_entity.org"
RIVAL_PLURAL = "netting_sets"

# A dq entity on the standard stack, used as the positive control.
NOT_STAGING_ONLY = REPO_ROOT / "projects/ores.dq/modeling/ores.dq.badge_definition.org"

# A staging shim whose entity no other component models, so dq owns it and its
# command unit is the only surface it has.
STAGING_ONLY_WITHOUT_A_RIVAL = (
    REPO_ROOT / "projects/ores.dq/modeling/ores.dq.lei_entity.org"
)

# The generated shell command unit and the literate recipe that documents it.
SHELL_TEMPLATES = frozenset({
    "cpp_shell_command_header.hpp.mustache",
    "cpp_shell_command_impl.cpp.mustache",
    "cpp_shell_command_tests.cpp.mustache",
    "shell_recipe.org.mustache",
})

# The stack the gate must not touch: the staging shim still generates its
# artefact table and its C++ surface, which the publish path reads.
STAGING_KEEPS = frozenset({
    "sql_schema_domain_entity_artefact_create.mustache",
    "cpp_domain_type_class.hpp.mustache",
    "cpp_domain_type_repository.hpp.mustache",
    "cpp_nats_handler.hpp.mustache",
})


def _shim_with_a_rival(tmp_path: Path) -> Path:
    modeling = tmp_path / "ores.dq" / "modeling"
    modeling.mkdir(parents=True)
    model = modeling / STAGING_SHIM.name
    model.write_text(re.sub(r"(?m)^#\+entity_plural: .*$",
                            f"#+entity_plural: {RIVAL_PLURAL}",
                            STAGING_SHIM.read_text()))
    return model


def test_a_staging_only_entity_resolves_no_command_unit(tmp_path):
    units, model_type, _ = resolve_targets(_shim_with_a_rival(tmp_path), CODEGEN_BASE)
    assert model_type == "domain_entity"
    templates = {u["template"] for u in units}
    assert not (SHELL_TEMPLATES & templates)
    # The gate must not over-drop: the shim keeps the surface it exists for.
    missing = STAGING_KEEPS - templates
    assert not missing, f"gate dropped facets the shim needs: {sorted(missing)}"


def test_a_staging_shim_that_is_the_only_owner_keeps_its_command_unit():
    units, model_type, _ = resolve_targets(STAGING_ONLY_WITHOUT_A_RIVAL, CODEGEN_BASE)
    assert model_type == "domain_entity"
    templates = {u["template"] for u in units}
    assert SHELL_TEMPLATES <= templates


def test_an_entity_that_is_not_a_staging_shim_keeps_its_command_unit():
    units, model_type, _ = resolve_targets(NOT_STAGING_ONLY, CODEGEN_BASE)
    assert model_type == "domain_entity"
    templates = {u["template"] for u in units}
    assert SHELL_TEMPLATES <= templates
