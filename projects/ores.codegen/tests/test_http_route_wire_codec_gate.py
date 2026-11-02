"""An http route encodes through the model's own codec where the model has one.

A route forwards a request and answers with the reply, and both are messages of
the model it serves. A model that opted into the wire codec has generated
functions for them, so its route reads the body and writes the reply through
those functions and names no reflector; a model that did not keeps reflect-cpp.
Both route templates, the entity one and the operation one, are rendered from a
real opted-in model, then again with the switch off.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_http_route_wire_codec_gate.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import pystache  # noqa: E402

import codegen.core as core  # noqa: E402

TEMPLATES = REPO_ROOT / "projects/ores.codegen/library/templates"
IAM = REPO_ROOT / "projects/ores.iam/modeling"


def render_data(model, template):
    captured = []
    original = core._RENDERER.render
    core._RENDERER.render = lambda t, d, *a, **k: (captured.append(d), "")[1]
    try:
        core.generate_from_model(
            str(IAM / model),
            REPO_ROOT / "projects/ores.codegen/library/data",
            TEMPLATES,
            REPO_ROOT / ".runtime/render/http_route",
            is_processing_batch=False,
            target_template=template,
            target_output="probe.cpp")
    finally:
        core._RENDERER.render = original
    assert len(captured) == 1
    return captured[0]


def render(model, template, section, opted_in):
    data = render_data(model, template)
    data[section]["has_generated_wire_codec"] = opted_in
    return pystache.render((TEMPLATES / template).read_text(encoding="utf-8"), data)


CASES = [
    ("ores.iam.account_credential.org", "cpp_http_route_impl.cpp.mustache", "domain_entity"),
    ("ores.iam.geo_operations.org", "cpp_http_route_operation_implementation.cpp.mustache",
     "operation"),
]


def test_a_real_opted_in_model_reaches_the_template_opted_in():
    for model, template, section in CASES:
        assert render_data(model, template)[section]["has_generated_wire_codec"], model


def test_an_opted_in_route_names_no_reflector():
    for model, template, section in CASES:
        text = render(model, template, section, True)
        assert "rfl::" not in text, model
        assert "<rfl/" not in text, model
        assert "rfl/reflectors" not in text, model
        assert "write_json(*decoded)" in text, model


def test_a_route_that_did_not_opt_in_keeps_the_reflector():
    for model, template, section in CASES:
        text = render(model, template, section, False)
        assert "rfl::json::write(*decoded)" in text, model
        assert "wire_traits" not in text, model
