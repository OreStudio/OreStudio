"""Tests for the self-referential repository using-directive guard.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_repository_namespace_guard.py

An entity in the ``database`` component generates C++ that opens
``namespace ores::database::repository`` and then pulls in
``ores::database::repository`` with a using-directive from inside that
same namespace. MSVC reports C4515 on the redundant directive and /WX
promotes the warning to error C2220, which broke the Windows build for
``database_info_mapper.cpp`` and ``database_info_repository.cpp``.

The templates now withhold the directive for the ``database`` component
alone. These tests pin both halves of that seam: a database-component
entity must not emit the self-reference, and every other component must
keep emitting it, because they sit outside the namespace they pull in.

The drift gate cannot catch a regression here. It proves regeneration is
stable against the checked-in tree, which stays true if the guard stops
working and the database-component files are regenerated to match.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

SELF_REFERENCE = "using namespace ores::database::repository;"

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000002
:END:
#+title: ores.{component}.namespace_probe
#+type: ores.codegen.entity
#+component: {component}
#+entity_singular: namespace_probe
#+entity_plural: namespace_probes
#+entity_title: Namespace Probe
#+has_tenant_id: true

Probe entity for the repository namespace guard.

* Flags
:PROPERTIES:
:schema:      public
:product:     ores
:component:   {component}
:subcomponent: api
:END:

* Columns

** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:primary_key: true
:END:

Probe key.

** description
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

Probe payload.
"""


def _generate(tmp_path, component, template, output):
    model_path = tmp_path / f"ores.{component}.namespace_probe.org"
    model_path.write_text(MODEL.format(component=component), encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template=template,
        target_output=output,
    )
    return (output_dir / output).read_text(encoding="utf-8")


def _generate_mapper(tmp_path, component):
    return _generate(
        tmp_path, component, "cpp_domain_type_mapper.cpp.mustache", "probe_mapper.cpp"
    )


def _generate_repository(tmp_path, component):
    return _generate(
        tmp_path,
        component,
        "cpp_domain_type_repository.cpp.mustache",
        "probe_repository.cpp",
    )


def test_mapper_omits_self_reference_for_database_component(tmp_path):
    out = _generate_mapper(tmp_path, "database")
    assert "namespace ores::database::repository {" in out
    assert SELF_REFERENCE not in out


def test_mapper_keeps_directive_for_other_components(tmp_path):
    out = _generate_mapper(tmp_path, "testcomp")
    assert "namespace ores::testcomp::repository {" in out
    assert SELF_REFERENCE in out


def test_repository_omits_self_reference_for_database_component(tmp_path):
    out = _generate_repository(tmp_path, "database")
    assert "namespace ores::database::repository {" in out
    assert SELF_REFERENCE not in out


def test_repository_keeps_directive_for_other_components(tmp_path):
    out = _generate_repository(tmp_path, "testcomp")
    assert "namespace ores::testcomp::repository {" in out
    assert SELF_REFERENCE in out


def test_logging_directive_survives_the_guard(tmp_path):
    """Only the self-reference is withheld; the mapper still needs lg()."""
    out = _generate_mapper(tmp_path, "database")
    assert "using namespace ores::logging;" in out
