"""Tests that a model can guard its reads, not only its writes.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_guarded_reads.py

By default a generated read proves authentication alone, which is the
estate's rule: the permission a write needs is the one the resource
already names for that change, and a read names none. An entity whose
reads expose material a signed-in caller must not see says
:guard_reads: true, and each read then checks the resource's own read
code. Without the flag nothing changes, which is what keeps the flag from
becoming a house-wide behaviour change.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000043
:END:
#+title: ores.testcomp.probe
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: probe
#+entity_plural: probes
#+entity_title: Probe

Probe entity.

* Flags
:PROPERTIES:
:schema:      public
:product:     ores
:component:   testcomp
:subcomponent: api
:guard_reads: true
:END:

* Columns

** id
:PROPERTIES:
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

Primary key.

** label
:PROPERTIES:
:type:     text
:cpp_type: std::string
:END:

A plain string column.
"""

PLAIN_ENTITY = ENTITY.replace(":guard_reads: true\n", "")


def _generate(tmp_path, output, body=ENTITY):
    model_path = tmp_path / "ores.testcomp.probe.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_nats_handler.hpp.mustache",
        target_output=output,
    )
    return (output_dir / output).read_text(encoding="utf-8")


def _blocks(handler):
    """``{method: body}`` for every handler method the template rendered."""
    starts = [i for i in range(len(handler))
              if handler.startswith("    void ", i)]
    out = {}
    for start, end in zip(starts, starts[1:] + [len(handler)]):
        block = handler[start:end]
        name = block.split("(", 1)[0].strip().split()[-1]
        out[name] = block
    return out


def test_a_guarded_read_checks_the_resource_read_code(tmp_path):
    handler = _generate(tmp_path, "probe_handler.hpp")
    blocks = _blocks(handler)

    reads = [name for name in blocks if name.startswith(("list_", "get_"))]
    writes = [name for name in blocks if name.startswith(("put_", "delete_"))]
    assert reads, "the entity derived no read operation"
    assert writes, "the entity derived no write operation"

    for name in reads:
        assert '"testcomp::probes:read"' in blocks[name], name
    for name in writes:
        assert "has_permission(" in blocks[name], name
        assert '"testcomp::probes:read"' not in blocks[name], name


def test_a_model_without_the_flag_leaves_its_reads_unguarded(tmp_path):
    handler = _generate(tmp_path, "probe_handler.hpp", body=PLAIN_ENTITY)
    blocks = _blocks(handler)

    assert '"testcomp::probes:read"' not in handler
    for name, block in blocks.items():
        if name.startswith(("list_", "get_")):
            assert "has_permission(" not in block, name
    # The writes keep the check they always had.
    assert '"testcomp::probes:write"' in handler
