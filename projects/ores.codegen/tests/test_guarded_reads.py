"""Tests that every generated read checks its resource's read code.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_guarded_reads.py

A read checks the resource's own read code, as a write checks its write or
delete code. A model opens a read to every signed-in caller only by naming
its subject in :open_reads:, and the security documentation allow-lists each
such subject. A subject that is not one of the resource's reads fails the
generation, so the model and the allow-list cannot drift by a typo.
"""
import sys
from pathlib import Path

import pytest

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

OPEN_LIST_ENTITY = ENTITY.replace(
    ":subcomponent: api\n", ":subcomponent: api\n:open_reads: testcomp.v1.probes.list\n")
MISSPELT_ENTITY = ENTITY.replace(
    ":subcomponent: api\n", ":subcomponent: api\n:open_reads: testcomp.v1.probe.list\n")


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


def test_every_read_checks_the_resource_read_code(tmp_path):
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


def test_an_open_read_needs_no_permission_and_the_others_stay_guarded(tmp_path):
    handler = _generate(tmp_path, "probe_handler.hpp", body=OPEN_LIST_ENTITY)
    blocks = _blocks(handler)

    assert "has_permission(" not in blocks["list_probes"]
    other_reads = [name for name in blocks
                   if name.startswith(("list_", "get_")) and name != "list_probes"]
    assert other_reads, "the entity derived no other read"
    for name in other_reads:
        assert '"testcomp::probes:read"' in blocks[name], name
    assert '"testcomp::probes:write"' in handler


def test_an_open_read_that_is_not_a_read_subject_fails_the_generation(tmp_path):
    with pytest.raises(ValueError, match="testcomp.v1.probe.list"):
        _generate(tmp_path, "probe_handler.hpp", body=MISSPELT_ENTITY)


JUNCTION = REPO_ROOT / "projects/ores.iam/modeling/ores.iam.role_grant_request_role.org"


def _generate_junction(tmp_path):
    body = JUNCTION.read_text(encoding="utf-8")
    model_path = tmp_path / JUNCTION.name
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    output = "role_grant_request_role_handler.hpp"
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


def test_a_junction_checks_its_read_code(tmp_path):
    blocks = _blocks(_generate_junction(tmp_path))

    reads = [name for name in blocks if name.startswith(("list_", "get_"))]
    assert reads, "the junction derived no read operation"
    for name in reads:
        assert '"iam::role_grant_request_roles:read"' in blocks[name], name
