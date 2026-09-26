"""A quoted :default_value: on a scalar column is refused.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_quoted_scalar_default.py

The C++ templates render ``default_value`` with a triple-mustache, so the model's
text reaches the generated initialiser unchanged. A model that writes

    :cpp_type:      bool
    :default_value: "true"

therefore produces ``bool is_active = "true";`` -- a string literal decaying to
a pointer, which the compiler accepts as ``true`` with no diagnostic under this
project's flags. The scheduler model carried exactly that, and no gate saw it:
the SQL was right, the type was right, and the C++ was wrong in a way that
compiles.

A string default is quoted because it is a string; a scalar one never is, so the
loader refuses the quoted form rather than stripping it. Stripping would make
the model read one way and the C++ another.
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

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F3
:END:
#+title: ores.testcomp.quoted_default_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: quoted_default_entity
#+entity_plural: quoted_default_entities
#+entity_title: Quoted Default Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

An entity whose boolean column states its default the wrong way.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:has_tenant_id: true
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

The surrogate key.

** is_active
:PROPERTIES:
:type:          boolean
:cpp_type:      bool
:nullable:      false
:default_value: {default}
:END:

Whether the row is active.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_quoted_default_entities_tbl
:END:
"""


def _render(tmp_path, default):
    model_path = tmp_path / "ores.testcomp.quoted_default_entity.org"
    model_path.write_text(MODEL.format(default=default), encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_domain_type_class.hpp.mustache",
        target_output="quoted_default_entity.hpp",
    )
    return (output_dir / "quoted_default_entity.hpp").read_text(encoding="utf-8")


def test_quoted_bool_default_is_refused(tmp_path):
    """The exact shape that produced `bool is_active = "true";`."""
    with pytest.raises(ValueError) as caught:
        _render(tmp_path, '"true"')
    message = str(caught.value)
    assert "is_active" in message
    assert "quoted" in message
    assert "bool" in message


def test_unquoted_bool_default_renders_as_a_scalar(tmp_path):
    """The shape the sibling marketdata model uses, and what made the fix work."""
    domain = _render(tmp_path, "true")
    assert "bool is_active = true;" in domain
    assert 'bool is_active = "true";' not in domain


def test_quoted_string_default_still_renders_quoted(tmp_path):
    """A string default is quoted because it is a string; that stays."""
    model = MODEL.format(default='"{}"').replace(":cpp_type:      bool",
                                                 ":cpp_type:      std::string")
    model_path = tmp_path / "ores.testcomp.quoted_default_entity.org"
    model_path.write_text(model, encoding="utf-8")
    output_dir = tmp_path / "out2"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_domain_type_class.hpp.mustache",
        target_output="quoted_default_entity.hpp",
    )
    domain = (output_dir / "quoted_default_entity.hpp").read_text(encoding="utf-8")
    assert 'std::string is_active = "{}";' in domain
