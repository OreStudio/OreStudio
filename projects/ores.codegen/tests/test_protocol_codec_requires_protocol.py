"""A protocol codec renders only where the model renders its protocol header.

The protocol codec defines the wire functions the protocol header declares. A
model that turns its protocol off, as an internal table does, has no header for
the codec to include, so emitting the codec anyway is a unit that cannot
compile. compute's grid_sample opts into the wire codec and turns its protocol
off; compute's app does neither, so the two resolve both ways.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_protocol_codec_requires_protocol.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen import generate  # noqa: E402

BASE_DIR = REPO_ROOT / "projects/ores.codegen"
COMPUTE = REPO_ROOT / "projects/ores.compute/modeling"


def outputs(model):
    units, _, _ = generate.resolve_targets(COMPUTE / model, BASE_DIR, address="ores.cpp")
    return [unit["output"] for unit in units]


def test_a_model_without_a_protocol_renders_no_protocol_codec():
    rendered = outputs("ores.compute.grid_sample.org")
    assert any(o.endswith("domain/grid_sample_codec.cpp") for o in rendered)
    assert not any(o.endswith("messaging/grid_sample_protocol.hpp") for o in rendered)
    assert not any(o.endswith("messaging/grid_sample_protocol_codec.cpp") for o in rendered)


def test_a_model_with_a_protocol_renders_its_protocol_codec():
    rendered = outputs("ores.compute.app.org")
    assert any(o.endswith("messaging/app_protocol.hpp") for o in rendered)
    assert any(o.endswith("messaging/app_protocol_codec.cpp") for o in rendered)
