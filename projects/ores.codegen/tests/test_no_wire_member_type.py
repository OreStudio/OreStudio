"""A wire-hidden column keeps its plain type where the model has its own codec.

A :no_wire: column must stay out of every serialised form. A model still on the
reflector hides it by wrapping the member in rfl::Skip; a model with its own
codec leaves it out of the field list, so the member needs no wrapper and the
domain header needs no reflector. account_credential is the one model with such
columns, and it has its own codec; rendering it pins both halves of the rule.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_no_wire_member_type.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import pystache  # noqa: E402

import codegen.core as core  # noqa: E402

MODEL = REPO_ROOT / "projects/ores.iam/modeling/ores.iam.account_credential.org"
TEMPLATES = REPO_ROOT / "projects/ores.codegen/library/templates"


def render_data():
    captured = []
    original = core._RENDERER.render
    core._RENDERER.render = lambda t, d, *a, **k: (captured.append(d), "")[1]
    try:
        core.generate_from_model(
            str(MODEL),
            REPO_ROOT / "projects/ores.codegen/library/data",
            TEMPLATES,
            REPO_ROOT / ".runtime/render/no_wire",
            is_processing_batch=False,
            target_template="cpp_domain_type_class.hpp.mustache",
            target_output="probe.hpp")
    finally:
        core._RENDERER.render = original
    assert len(captured) == 1
    return captured[0]


def test_a_model_with_its_own_codec_keeps_wire_hidden_members_plain():
    entity = render_data()["domain_entity"]
    hidden = [c for c in entity["columns"] if c.get("no_wire")]
    assert hidden
    assert entity["has_generated_wire_codec"]
    assert not any(c["rfl_skip"] for c in hidden)
    assert not entity["has_rfl_skip_columns"]


def test_a_model_on_the_reflector_wraps_wire_hidden_members():
    data = render_data()
    entity = data["domain_entity"]
    entity["has_rfl_skip_columns"] = True
    for c in entity["columns"]:
        c["rfl_skip"] = bool(c.get("no_wire"))
    text = pystache.render((TEMPLATES / "cpp_domain_type_class.hpp.mustache").read_text(), data)
    assert "rfl::Skip<std::string> password_hash;" in text
    assert "skip_comparison.hpp" in text


def test_the_rendered_header_names_no_reflector():
    data = render_data()
    text = pystache.render((TEMPLATES / "cpp_domain_type_class.hpp.mustache").read_text(), data)
    assert "rfl::Skip" not in text
    assert "std::string password_hash;" in text
