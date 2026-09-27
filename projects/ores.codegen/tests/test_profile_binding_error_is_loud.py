"""A profile binding error must reach the caller, not be swallowed into ``{}``.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_profile_binding_error_is_loud.py

``_read_drawer_properties`` merges a model's physical-space overrides inside a
``try`` whose ``except`` returned ``{}`` for any exception. A profile conflict
raises while the overrides are being *merged*, so the empty dict discarded every
override with it: the entity rendered as though it had bound no profile at all,
and a staging shim silently regained the main table its profile had withdrawn.

A binding error is a model defect, and ``_ensure_profile_binding`` exists so one
fails loudly on every path rather than as a silent no-op.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import codegen.org_loader as org_loader  # noqa: E402
from codegen.generate import _read_drawer_properties  # noqa: E402


def test_a_profile_binding_error_is_not_swallowed(monkeypatch, tmp_path):
    model = tmp_path / "model.org"
    model.write_text(
        "#+title: model\n\n* Flags\n:PROPERTIES:\n:profile: simple-lookup\n:END:\n",
        encoding="utf-8",
    )

    def conflict(_doc):
        raise ValueError("two profiles fix one address differently")

    monkeypatch.setattr(org_loader, "read_physical_space_overrides", conflict)

    # Returning {} instead would drop every override the profile supplies,
    # which is a silent wrong answer rather than a reported one.
    with pytest.raises(ValueError):
        _read_drawer_properties(model)
