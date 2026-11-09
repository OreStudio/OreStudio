"""Tests for build/scripts/generate_ore_trade_requirements.py.

Run::

    python3 -m pytest build/scripts/tests/test_generate_ore_trade_requirements.py

The required terms drive what a trade builder demands, so a term reported as
required when ORE lets it be absent, or the reverse, misleads every builder.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "generate_ore_trade_requirements",
    REPO_ROOT / "build" / "scripts" / "generate_ore_trade_requirements.py")
generator = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = generator
SPEC.loader.exec_module(generator)

XSD = """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:group name="ids"><xs:sequence><xs:element name="Id" type="xs:string"/></xs:sequence></xs:group>
  <xs:complexType name="base"><xs:sequence><xs:element name="Base" type="xs:string"/></xs:sequence></xs:complexType>
  <xs:complexType name="product"><xs:complexContent><xs:extension base="base"><xs:sequence>
    <xs:element name="Must" type="xs:string"/>
    <xs:element name="May" type="xs:string" minOccurs="0"/>
    <xs:group ref="ids"/>
    <xs:group ref="ids" minOccurs="0"/>
    <xs:choice><xs:element name="A" type="xs:string"/><xs:element name="B" type="xs:string"/></xs:choice>
    <xs:choice minOccurs="0"><xs:element name="C" type="xs:string"/></xs:choice>
  </xs:sequence></xs:extension></xs:complexContent></xs:complexType>
</xs:schema>"""


def required(tmp_path):
    (tmp_path / "t.xsd").write_text(XSD)
    return [t.name for t in generator.Schema.load(tmp_path).required("product")]


def test_required_terms_include_base_groups_and_a_required_choice(tmp_path):
    assert required(tmp_path) == ["Base", "Must", "Id", "A or B"]


def test_optional_elements_groups_and_choices_are_not_required(tmp_path):
    assert not {"May", "C"} & set(required(tmp_path))


def test_an_unknown_type_requires_nothing(tmp_path):
    (tmp_path / "t.xsd").write_text(XSD)
    assert generator.Schema.load(tmp_path).required("missing") == []
