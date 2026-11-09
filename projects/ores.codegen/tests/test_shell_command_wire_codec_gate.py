"""A shell command writes its reply through the model's own codec where it has one.

A generated command sends a request and prints the reply. A model that opted
into the wire codec has a generated function for the reply, so its command
prints through that function. Both templates, the entity one and the operation one, are
rendered from a real opted-in model, then again with the switch off.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_shell_command_wire_codec_gate.py
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
            REPO_ROOT / ".runtime/render/shell_command",
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
    ("ores.iam.tenant_type.org", "cpp_shell_command_impl.cpp.mustache", "domain_entity"),
    ("ores.iam.geo_operations.org", "cpp_shell_operation_implementation.cpp.mustache",
     "operation"),
]


def test_a_real_opted_in_model_reaches_the_template_opted_in():
    for model, template, section in CASES:
        assert render_data(model, template)[section]["has_generated_wire_codec"], model


def test_an_opted_in_command_uses_its_codec():
    for model, template, section in CASES:
        text = render(model, template, section, True)
        assert "::write_json(" in text, model
