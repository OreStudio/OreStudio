"""Tests for the include guard an operation model's C++ header carries.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_operation_protocol_include_guard.py

An operation model renders ``<entity>_protocol.hpp`` into the part the model
names. The guard has to carry that part, the way a domain entity's does.
It did not: the operation section of the template used the bare component, so
``ores.ore.api`` and ``ores.ore.core`` would both render
``ORES_ORE_MESSAGING_<X>_PROTOCOL_HPP`` for an operation of the same name, and
the preprocessor would silently drop whichever header the compiler reached
second. Nothing detects that: the dropped header still compiles on its own,
and a translation unit that includes only one of the two sees no problem.

The collision needs a composite with two parts, and every operation model in
the tree lives in a simple component, so the guard is the first place the
difference shows.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

# One operation model, rendered into two parts of the same composite. Only
# the subcomponent differs, which is exactly the pair the guard has to tell
# apart.
MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000C2
:END:
#+title: ores.testcomp.thing_messages
#+type: ores.codegen.operation
#+component: testcomp
#+subcomponent: {subcomponent}
#+entity_singular: thing
#+namespace: ores::testcomp::messaging
#+brief: Test messages for the include guard.

* Messages

** thing_request
:PROPERTIES:
:subject: testcomp.v1.things.list
:END:

*** limit
:PROPERTIES:
:cpp_type: int
:END:
"""


def _guard(tmp_path, subcomponent):
    model_path = tmp_path / "ores.testcomp.thing_messages.org"
    model_path.write_text(MODEL.format(subcomponent=subcomponent),
                          encoding="utf-8")
    output_dir = tmp_path / f"out_{subcomponent}"
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_protocol.hpp.mustache",
        target_output="thing_protocol.hpp",
    )
    header = (output_dir / "thing_protocol.hpp").read_text(encoding="utf-8")
    return [line for line in header.splitlines() if line.startswith("#ifndef")][0]


def test_the_guard_names_the_part(tmp_path):
    assert _guard(tmp_path, "api") == (
        "#ifndef ORES_TESTCOMP_API_MESSAGING_THING_PROTOCOL_HPP")


def test_two_parts_of_one_composite_do_not_share_a_guard(tmp_path):
    api = _guard(tmp_path, "api")
    core = _guard(tmp_path, "core")
    assert api != core
    assert core == "#ifndef ORES_TESTCOMP_CORE_MESSAGING_THING_PROTOCOL_HPP"
