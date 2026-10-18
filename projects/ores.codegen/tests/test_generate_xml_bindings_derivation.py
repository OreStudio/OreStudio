"""A derived type carries its base's fields and then its own."""

import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
GENERATOR = ROOT / "projects/ores.ore/scripts/generate_xml_bindings.py"

BASE = """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:complexType name="baseType">
    <xs:sequence>
      <xs:element type="xs:string" name="Alpha"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id" use="required"/>
  </xs:complexType>
</xs:schema>
"""

DERIVED = """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:include schemaLocation="base.xsd"/>
  <xs:element type="derivedType" name="Root"/>
  <xs:complexType name="derivedType">
    <xs:complexContent>
      <xs:extension base="baseType">
        <xs:sequence>
          <xs:element type="xs:string" name="Beta"/>
        </xs:sequence>
        <xs:attribute type="xs:string" name="kind"/>
      </xs:extension>
    </xs:complexContent>
  </xs:complexType>
</xs:schema>
"""


def test_a_derived_type_flattens_its_base(tmp_path):
    """The record needs the base's fields, in the base's order, then its own."""
    (tmp_path / "base.xsd").write_text(BASE)
    (tmp_path / "derived.xsd").write_text(DERIVED)
    header = tmp_path / "out.hpp"
    source = tmp_path / "out.cpp"

    subprocess.run(
        [
            sys.executable,
            str(GENERATOR),
            "--schema",
            str(tmp_path / "base.xsd"),
            "--schema",
            str(tmp_path / "derived.xsd"),
            "--root-element",
            "Root",
            "--root-type",
            "derivedType",
            "--accessor",
            "root_document",
            "--header",
            str(header),
            "--source",
            str(source),
        ],
        check=True,
        capture_output=True,
        text=True,
    )

    emitted = header.read_text()
    derived = emitted[emitted.index("struct derived_type {") :]
    derived = derived[: derived.index("};")]
    for member in ("alpha", "beta", "id", "kind"):
        assert member in derived, member
    # The base's element comes before the extension's, which is the order the
    # document writes and the order the table states.
    assert derived.index("alpha") < derived.index("beta")


def _generate(tmp_path, schema, root="Root", root_type="derivedType"):
    header = tmp_path / "out.hpp"
    return subprocess.run(
        [
            sys.executable,
            str(GENERATOR),
            "--schema",
            str(tmp_path / schema),
            "--root-element",
            root,
            "--root-type",
            root_type,
            "--accessor",
            "root_document",
            "--header",
            str(header),
            "--source",
            str(tmp_path / "out.cpp"),
        ],
        capture_output=True,
        text=True,
    )


def test_a_restriction_is_refused(tmp_path):
    """A restriction removes fields, which needs its own decision."""
    (tmp_path / "base.xsd").write_text(BASE)
    (tmp_path / "restricted.xsd").write_text(
        DERIVED.replace("<xs:extension", "<xs:restriction").replace(
            "</xs:extension>", "</xs:restriction>"
        ).replace('name="derivedType"', 'name="derivedType"')
    )
    result = _generate(tmp_path, "restricted.xsd")
    assert result.returncode != 0
    assert "restricts a base type" in result.stderr


def test_a_derivation_cycle_is_refused(tmp_path):
    """Two types that extend each other stop with a message, not a traceback."""
    (tmp_path / "loop.xsd").write_text(
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="aType" name="Root"/>
  <xs:complexType name="aType">
    <xs:complexContent><xs:extension base="bType">
      <xs:sequence><xs:element type="xs:string" name="Alpha"/></xs:sequence>
    </xs:extension></xs:complexContent>
  </xs:complexType>
  <xs:complexType name="bType">
    <xs:complexContent><xs:extension base="aType">
      <xs:sequence><xs:element type="xs:string" name="Beta"/></xs:sequence>
    </xs:extension></xs:complexContent>
  </xs:complexType>
</xs:schema>
"""
    )
    result = _generate(tmp_path, "loop.xsd", root_type="aType")
    assert result.returncode != 0
    assert "cycle" in result.stderr
