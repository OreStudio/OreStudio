"""Tests for the staging-only eventing gate, and for the loud profile conflict.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_staging_only_eventing_gate.py

A model that binds ``artefact-staging-only`` is a staging surface with no
same-component main table. The profile withdraws the notify trigger that would
announce a change, so the entity has nothing to announce, and the eventing
facets go: the event type, its registrar, and the integration test that asserts
an event arriving over NATS.

The gate is unconditional, unlike the shell gate it sits beside. A shell
collision needs a second component modelling the same plural, but the missing
trigger is a property of the profile itself, so every staging shim lacks it.

The last test is the regression guard for the conflict that hid this one: the
profile's own Physical space supplies the withdrawn main table, and that only
survives while a merge error is not swallowed into an empty override set. The
swallow itself is pinned in ``test_profile_binding_error_is_loud.py``.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"

# The facets that announce a change. With no notify trigger there is no
# publisher for any of them.
ANNOUNCEMENT_TEMPLATES = frozenset({
    "cpp_nats_changed_event.hpp.mustache",
    "cpp_nats_event_registrar.hpp.mustache",
    "cpp_nats_event_registrar.cpp.mustache",
    "cpp_nats_integration_test.cpp.mustache",
})

# The facets that serve the entity's own reads. A staging shim still answers
# list and get, so the gate must not take these along with the announcement.
MESSAGING_TEMPLATES = frozenset({
    "cpp_nats_handler.hpp.mustache",
    "cpp_nats_registrar.hpp.mustache",
    "cpp_nats_registrar.cpp.mustache",
})

# A staging shim that is not current-state.
STAGING = REPO_ROOT / "projects/ores.dq/modeling/ores.dq.synthetic_fx_spot_config.org"

# A staging shim that is also current-state.
STAGING_CURRENT_STATE = REPO_ROOT / "projects/ores.dq/modeling/ores.dq.lei_entity.org"

# A dq entity on the standard stack, used as the positive control.
NOT_STAGING = REPO_ROOT / "projects/ores.dq/modeling/ores.dq.badge_definition.org"


def _templates(model):
    units, model_type, _ = resolve_targets(model, CODEGEN_BASE)
    assert model_type == "domain_entity"
    return {u["template"] for u in units}


def test_a_staging_shim_resolves_no_announcement():
    templates = _templates(STAGING)
    assert not (ANNOUNCEMENT_TEMPLATES & templates)
    missing = MESSAGING_TEMPLATES - templates
    assert not missing, f"gate dropped facets the shim needs: {sorted(missing)}"


def test_a_staging_shim_that_is_current_state_resolves_no_announcement():
    templates = _templates(STAGING_CURRENT_STATE)
    assert not (ANNOUNCEMENT_TEMPLATES & templates)


def test_an_entity_that_is_not_a_staging_shim_keeps_its_announcement():
    templates = _templates(NOT_STAGING)
    assert ANNOUNCEMENT_TEMPLATES <= templates


def test_a_staging_shim_still_withdraws_its_main_table():
    # The profile's own Physical space supplies this, and it survives only while
    # a merge error is not swallowed into an empty override set.
    templates = _templates(STAGING)
    assert "sql_schema_domain_entity_create.mustache" not in templates
    assert "sql_schema_domain_entity_artefact_create.mustache" in templates
