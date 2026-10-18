"""A derived type carries its base's fields and then its own."""

import re
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
        )
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


NESTED_CHOICE = """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="choiceType" name="Root"/>
  <xs:complexType name="choiceType">
    <xs:sequence>
      <xs:element type="xs:string" name="Head"/>
      <xs:choice>
        <xs:sequence>
          <xs:element type="xs:string" name="Alpha"/>
          <xs:element type="xs:string" name="Beta"/>
        </xs:sequence>
        <xs:sequence>
          <xs:element type="xs:string" name="Gamma" minOccurs="0"/>
          <xs:element type="xs:int" name="Delta"/>
        </xs:sequence>
      </xs:choice>
      <xs:element type="xs:string" name="Tail" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>
</xs:schema>
"""


def test_a_choice_of_sequences_keeps_every_member(tmp_path):
    """A choice whose alternatives are sequences still offers all their elements.

    The record is flat, so each element of each alternative is a member of it.
    Dropping them would leave the table unable to bind a document the schema
    allows.
    """
    (tmp_path / "choice.xsd").write_text(NESTED_CHOICE)
    header = tmp_path / "out.hpp"
    subprocess.run(
        [
            sys.executable,
            str(GENERATOR),
            "--schema",
            str(tmp_path / "choice.xsd"),
            "--root-element",
            "Root",
            "--root-type",
            "choiceType",
            "--accessor",
            "root_document",
            "--header",
            str(header),
            "--source",
            str(tmp_path / "out.cpp"),
        ],
        check=True,
        capture_output=True,
        text=True,
    )

    emitted = header.read_text()
    record = emitted[emitted.index("struct choice_type {") :]
    record = record[: record.index("};")]
    for member in ("head", "alpha", "beta", "gamma", "delta", "tail"):
        assert member in record, member
    assert record.index("alpha") < record.index("gamma")


DUPLICATE_ALTERNATIVES = """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="choiceType" name="Root"/>
  <xs:complexType name="choiceType">
    <xs:choice>
      <xs:sequence>
        <xs:element type="xs:int" name="Shared"/>
        <xs:element type="xs:string" name="Alpha"/>
      </xs:sequence>
      <xs:sequence>
        <xs:element type="xs:int" name="Shared"/>
        <xs:element type="xs:string" name="Beta"/>
      </xs:sequence>
    </xs:choice>
  </xs:complexType>
</xs:schema>
"""


def test_one_element_named_by_two_alternatives_is_one_member(tmp_path):
    """A choice may offer the same element twice; the record holds it once."""
    (tmp_path / "choice.xsd").write_text(DUPLICATE_ALTERNATIVES)
    header = tmp_path / "out.hpp"
    subprocess.run(
        [
            sys.executable,
            str(GENERATOR),
            "--schema",
            str(tmp_path / "choice.xsd"),
            "--root-element",
            "Root",
            "--root-type",
            "choiceType",
            "--accessor",
            "root_document",
            "--header",
            str(header),
            "--source",
            str(tmp_path / "out.cpp"),
        ],
        check=True,
        capture_output=True,
        text=True,
    )

    emitted = header.read_text()
    record = emitted[emitted.index("struct choice_type {") :]
    record = record[: record.index("};")]
    assert record.count("shared") == 1
    assert "alpha" in record
    assert "beta" in record


def test_two_alternatives_that_disagree_about_one_name_are_refused(tmp_path):
    """One record member cannot hold two shapes, so the run stops."""
    (tmp_path / "choice.xsd").write_text(
        DUPLICATE_ALTERNATIVES.replace(
            '<xs:element type="xs:int" name="Shared"/>\n        <xs:element type="xs:string" name="Beta"/>',
            '<xs:element type="xs:date" name="Shared"/>\n        <xs:element type="xs:string" name="Beta"/>',
        )
    )
    result = _generate(tmp_path, "choice.xsd", root_type="choiceType")
    assert result.returncode != 0
    assert "Shared" in result.stderr


def _emit(tmp_path, schema_text, root_type="rootType"):
    """The generated header and source for one schema, as text."""
    (tmp_path / "schema.xsd").write_text(schema_text)
    header = tmp_path / "out.hpp"
    source = tmp_path / "out.cpp"
    subprocess.run(
        [
            sys.executable,
            str(GENERATOR),
            "--schema",
            str(tmp_path / "schema.xsd"),
            "--root-element",
            "Root",
            "--root-type",
            root_type,
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
    return header.read_text(), source.read_text()


def _record(emitted, name):
    record = emitted[emitted.index(f"struct {name} {{") :]
    return record[: record.index("\n};")]


def _row(source, name):
    """The field row one element states, as a list of its fields.

    The member offset is an `offsetof(type, member)` expression, and its
    argument list holds a comma, so it is replaced before splitting.
    """
    row = source[source.index('{"' + name + '"') :]
    row = row[: row.index("}")]
    return re.sub(r"offsetof\([^)]*\)", "offset", row).split(",")


def test_an_annotation_inside_a_compositor_is_ignored(tmp_path):
    """An annotation is documentation, not a member, wherever it is written."""
    emitted, _source = _emit(
        tmp_path,
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence>
      <xs:annotation><xs:documentation>Notes to a reader.</xs:documentation></xs:annotation>
      <xs:element type="xs:string" name="Alpha"/>
    </xs:sequence>
  </xs:complexType>
</xs:schema>
""",
    )
    record = _record(emitted, "root_type")
    assert "alpha" in record
    assert "documentation" not in record


def test_a_repeated_choice_repeats_its_members(tmp_path):
    """A choice that repeats hands the record one vector per member."""
    emitted, _source = _emit(
        tmp_path,
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:choice maxOccurs="unbounded">
      <xs:element type="xs:string" name="Alpha"/>
      <xs:element type="xs:string" name="Beta"/>
    </xs:choice>
  </xs:complexType>
</xs:schema>
""",
    )
    record = _record(emitted, "root_type")
    assert "std::vector<std::string> alpha" in record
    assert "std::vector<std::string> beta" in record


def test_a_member_under_an_optional_group_is_not_required(tmp_path):
    """An optional group may be absent whole, so its members may be too."""
    emitted, source = _emit(
        tmp_path,
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence>
      <xs:sequence minOccurs="0">
        <xs:element type="xs:string" name="Alpha"/>
      </xs:sequence>
      <xs:element type="xs:string" name="Beta"/>
    </xs:sequence>
  </xs:complexType>
</xs:schema>
""",
    )
    assert "alpha" in _record(emitted, "root_type")

    def required(name):
        # name, hash, kind, offset, bit, attribute, required, ...
        return _row(source, name)[6].strip()

    assert required("Alpha") == "false"
    assert required("Beta") == "true"


def test_a_member_named_like_an_earlier_type_is_qualified(tmp_path):
    """A member declared earlier hides a type of the same name.

    A string member named `leaf` is in scope when an element of type `leaf` is
    declared after it, so the type has to be spelled with the struct keyword.
    """
    emitted, _source = _emit(
        tmp_path,
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence>
      <xs:element type="xs:string" name="leaf"/>
      <xs:element type="leaf" name="first"/>
      <xs:element type="leaf" name="second"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="leaf">
    <xs:sequence><xs:element type="xs:string" name="Value" minOccurs="0"/></xs:sequence>
  </xs:complexType>
</xs:schema>
""",
    )
    record = _record(emitted, "root_type")
    assert "std::string leaf;" in record
    assert "struct leaf first;" in record
    assert "struct leaf second;" in record


def test_two_alternatives_with_different_cardinality_are_refused(tmp_path):
    """A scalar and a repeated form of one name are two shapes, not one."""
    (tmp_path / "choice.xsd").write_text(
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="choiceType" name="Root"/>
  <xs:complexType name="choiceType">
    <xs:choice>
      <xs:element type="xs:int" name="Shared"/>
      <xs:element type="xs:int" name="Shared" maxOccurs="unbounded"/>
    </xs:choice>
  </xs:complexType>
</xs:schema>
"""
    )
    result = _generate(tmp_path, "choice.xsd", root_type="choiceType")
    assert result.returncode != 0
    assert "Shared" in result.stderr


def test_a_member_named_like_its_own_type_is_also_qualified_when_used_again(tmp_path):
    """`leaf leaf;` compiles, but a later `leaf` of the same type does not."""
    emitted, _source = _emit(
        tmp_path,
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence>
      <xs:element type="leaf" name="leaf"/>
      <xs:element type="leaf" name="second"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="leaf">
    <xs:sequence><xs:element type="xs:string" name="Value" minOccurs="0"/></xs:sequence>
  </xs:complexType>
</xs:schema>
""",
    )
    record = _record(emitted, "root_type")
    assert "struct leaf leaf;" in record
    assert "struct leaf second;" in record


def test_an_attribute_named_after_a_type_shadows_it_too(tmp_path):
    """Attributes are declared before elements, so they shadow for every element."""
    emitted, _source = _emit(
        tmp_path,
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence>
      <xs:element type="leaf" name="first"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="leaf"/>
  </xs:complexType>
  <xs:complexType name="leaf">
    <xs:sequence><xs:element type="xs:string" name="Value" minOccurs="0"/></xs:sequence>
  </xs:complexType>
</xs:schema>
""",
    )
    record = _record(emitted, "root_type")
    assert "std::string leaf;" in record
    assert "struct leaf first;" in record


def test_two_alternatives_with_different_inline_types_are_refused(tmp_path):
    """Two inline types of the same size are still two shapes."""
    (tmp_path / "choice.xsd").write_text(
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="choiceType" name="Root"/>
  <xs:complexType name="choiceType">
    <xs:choice>
      <xs:element name="Shared">
        <xs:complexType><xs:sequence>
          <xs:element type="xs:string" name="Alpha"/>
        </xs:sequence></xs:complexType>
      </xs:element>
      <xs:element name="Shared">
        <xs:complexType><xs:sequence>
          <xs:element type="xs:int" name="Beta"/>
        </xs:sequence></xs:complexType>
      </xs:element>
    </xs:choice>
  </xs:complexType>
</xs:schema>
"""
    )
    result = _generate(tmp_path, "choice.xsd", root_type="choiceType")
    assert result.returncode != 0
    assert "Shared" in result.stderr


def test_a_reference_to_a_global_element_becomes_a_member_of_its_name(tmp_path):
    """`ref` states the name through the global element it points at."""
    emitted, _source = _emit(
        tmp_path,
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="rootType" name="Root"/>
  <xs:element type="xs:string" name="Gamma"/>
  <xs:element type="xs:string" name="Delta"/>
  <xs:complexType name="rootType">
    <xs:sequence>
      <xs:element ref="Gamma"/>
      <xs:element ref="Delta"/>
    </xs:sequence>
  </xs:complexType>
</xs:schema>
""",
    )
    record = _record(emitted, "root_type")
    assert "std::string gamma;" in record
    assert "std::string delta;" in record


def test_one_reference_with_two_cardinalities_is_refused(tmp_path):
    """The same element referenced twice is one member only if it means one shape."""
    (tmp_path / "refs.xsd").write_text(
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="rootType" name="Root"/>
  <xs:element type="xs:string" name="Gamma"/>
  <xs:complexType name="rootType">
    <xs:choice>
      <xs:element ref="Gamma"/>
      <xs:element ref="Gamma" maxOccurs="unbounded"/>
    </xs:choice>
  </xs:complexType>
</xs:schema>
"""
    )
    result = _generate(tmp_path, "refs.xsd", root_type="rootType")
    assert result.returncode != 0
    assert "Gamma" in result.stderr


def test_two_alternatives_with_different_inline_enumerations_are_refused(tmp_path):
    """Two inline enumerations are two spellings, so they are two shapes."""
    (tmp_path / "choice.xsd").write_text(
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="choiceType" name="Root"/>
  <xs:complexType name="choiceType">
    <xs:choice>
      <xs:element name="Shared">
        <xs:simpleType><xs:restriction base="xs:string">
          <xs:enumeration value="A"/>
        </xs:restriction></xs:simpleType>
      </xs:element>
      <xs:element name="Shared">
        <xs:simpleType><xs:restriction base="xs:string">
          <xs:enumeration value="B"/>
        </xs:restriction></xs:simpleType>
      </xs:element>
    </xs:choice>
  </xs:complexType>
</xs:schema>
"""
    )
    result = _generate(tmp_path, "choice.xsd", root_type="choiceType")
    assert result.returncode != 0
    assert "Shared" in result.stderr


def test_text_typed_by_an_enumerated_simple_type_keeps_its_spellings(tmp_path):
    """xs:simpleContent over an enumeration stores the index, not the text."""
    emitted, source = _emit(
        tmp_path,
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:simpleType name="scheme">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Forward"/>
      <xs:enumeration value="Backward"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:complexType name="entry">
    <xs:simpleContent>
      <xs:extension base="scheme">
        <xs:attribute type="xs:string" name="id"/>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence>
      <xs:element type="entry" name="Entry"/>
    </xs:sequence>
  </xs:complexType>
</xs:schema>
""",
    )
    record = _record(emitted, "entry")
    assert "std::int16_t value" in record
    # The row names the spellings, so the reading refuses other text.
    own_row = source[source.index('{"", 0u, field_kind::enumeration'):]
    own_row = own_row[: own_row.index("}")]
    assert "scheme_spellings" in own_row
    assert 'const char* scheme_spellings[] = {"Forward", "Backward", nullptr};' in source


def test_an_enumeration_behind_a_restriction_chain_keeps_its_spellings(tmp_path):
    """A simpleContent base may name any link in a chain of restrictions."""
    emitted, source = _emit(
        tmp_path,
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:simpleType name="scheme">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Forward"/>
      <xs:enumeration value="Backward"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:simpleType name="schemeAlias">
    <xs:restriction base="scheme"/>
  </xs:simpleType>
  <xs:complexType name="entry">
    <xs:simpleContent><xs:extension base="schemeAlias"/></xs:simpleContent>
  </xs:complexType>
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence><xs:element type="entry" name="Entry"/></xs:sequence>
  </xs:complexType>
</xs:schema>
""",
    )
    assert "std::int16_t value" in _record(emitted, "entry")
    assert "scheme_spellings" in source


GROUP = """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:group name="common">
    <xs:sequence>
      <xs:element type="xs:string" name="Alpha"/>
      <xs:element type="xs:string" name="Beta" minOccurs="0"/>
    </xs:sequence>
  </xs:group>
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence>
      <xs:element type="xs:string" name="Head"/>
      <xs:group ref="common" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
</xs:schema>
"""


def test_a_model_group_is_flattened_where_it_is_referenced(tmp_path):
    """A group is not a member: its members are the type's members."""
    emitted, source = _emit(tmp_path, GROUP)
    record = _record(emitted, "root_type")
    assert "std::string head" in record
    assert "std::vector<std::string> alpha" in record
    assert "std::vector<std::string> beta" in record


SUBSTITUTION_INLINE = """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="headGroup" abstract="true"/>
  <xs:element name="Member" substitutionGroup="headGroup">
    <xs:complexType>
      <xs:sequence><xs:element type="xs:string" name="Alpha"/></xs:sequence>
    </xs:complexType>
  </xs:element>
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence>
      <xs:element ref="headGroup" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>
</xs:schema>
"""


def test_a_substitution_member_that_writes_its_own_type_is_a_record(tmp_path):
    """The member carries its type inline, so the member is the record."""
    emitted, _source = _emit(tmp_path, SUBSTITUTION_INLINE)
    record = _record(emitted, "root_type")
    assert "member member" in record
    assert "std::string alpha" in _record(emitted, "member")


UNTYPED = """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence><xs:element name="Free"/></xs:sequence>
  </xs:complexType>
</xs:schema>
"""


def test_an_element_with_no_type_is_carried_as_text(tmp_path):
    """xs:anyType is not described by the schema, so it cannot be a record."""
    emitted, _source = _emit(tmp_path, UNTYPED)
    assert "std::string free" in _record(emitted, "root_type")


def _wide_schema(fields):
    body = "\n".join(
        f'      <xs:element type="xs:string" name="F{i}" minOccurs="0"/>' for i in range(fields)
    )
    return f"""<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence>
{body}
    </xs:sequence>
  </xs:complexType>
</xs:schema>
"""


def test_a_type_with_more_than_one_mask_word_states_its_mask_as_an_array(tmp_path):
    """A record with more than 64 fields needs more than one presence word."""
    emitted, _source = _emit(tmp_path, _wide_schema(70))
    record = _record(emitted, "root_type")
    assert "std::uint64_t present[2] = {0" in record
    narrow, _source = _emit(tmp_path, _wide_schema(3))
    assert "std::uint64_t present = 0" in _record(narrow, "root_type")


RECURSIVE_SCHEMA = """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence>
      <xs:element type="xs:string" name="Head"/>
      <xs:element type="node" name="Node" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="node">
    <xs:sequence>
      <xs:element type="xs:string" name="Value" minOccurs="0"/>
      <xs:element type="node" name="Child" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>
</xs:schema>
"""


def test_a_type_that_holds_itself_loses_only_the_edge_that_closes_it(tmp_path):
    """A record cannot hold itself, so the recursive member is left out."""
    emitted, _source = _emit(tmp_path, RECURSIVE_SCHEMA)
    node = _record(emitted, "node")
    assert "std::string value" in node
    assert "child" not in node
    assert "std::string head" in _record(emitted, "root_type")


def _required(source, name):
    row = source[source.index('{"' + name + '"') :]
    row = row[: row.index("}")]
    row = re.sub(r"offsetof\([^)]*\)", "offset", row)
    return row.split(",")[6].strip()


def test_a_required_member_of_a_sequence_group_stays_required(tmp_path):
    """A group is not a choice: its required members are required."""
    emitted, source = _emit(tmp_path, GROUP)
    assert _required(source, "Head") == "true"
    assert _required(source, "Alpha") == "true"


GROUP_REUSED = """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:group name="common">
    <xs:sequence><xs:element type="xs:string" name="Alpha"/></xs:sequence>
  </xs:group>
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence>
      <xs:element type="oneType" name="One"/>
      <xs:element type="twoType" name="Two"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="oneType">
    <xs:sequence><xs:group ref="common" maxOccurs="unbounded"/></xs:sequence>
  </xs:complexType>
  <xs:complexType name="twoType">
    <xs:sequence><xs:group ref="common"/></xs:sequence>
  </xs:complexType>
</xs:schema>
"""


def test_one_group_referenced_twice_keeps_each_reference_cardinality(tmp_path):
    """A reference states its own cardinality; the group is not changed by it."""
    emitted, _source = _emit(tmp_path, GROUP_REUSED)
    assert "std::vector<std::string> alpha" in _record(emitted, "one_type")
    assert "std::string alpha" in _record(emitted, "two_type")


CROSS_FILE = {
    "first.xsd": """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="Shared" type="sharedType"/>
  <xs:complexType name="sharedType">
    <xs:sequence><xs:element type="xs:string" name="Alpha"/></xs:sequence>
  </xs:complexType>
</xs:schema>
""",
    "second.xsd": """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:include schemaLocation="first.xsd"/>
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence><xs:element ref="Shared"/></xs:sequence>
  </xs:complexType>
</xs:schema>
""",
}


def test_a_reference_to_a_global_element_of_an_earlier_schema(tmp_path):
    """The globals of every schema read are one index."""
    (tmp_path / "first.xsd").write_text(CROSS_FILE["first.xsd"])
    (tmp_path / "second.xsd").write_text(CROSS_FILE["second.xsd"])
    header = tmp_path / "out.hpp"
    subprocess.run(
        [
            sys.executable,
            str(GENERATOR),
            "--schema",
            str(tmp_path / "first.xsd"),
            "--schema",
            str(tmp_path / "second.xsd"),
            "--root-element",
            "Root",
            "--root-type",
            "rootType",
            "--accessor",
            "root_document",
            "--header",
            str(header),
            "--source",
            str(tmp_path / "out.cpp"),
        ],
        check=True,
        capture_output=True,
        text=True,
    )
    assert "shared_type shared" in _record(header.read_text(), "root_type")


def test_a_cut_member_is_named_in_the_generated_header(tmp_path):
    """The file says which member a record cannot hold, and why."""
    emitted, _source = _emit(tmp_path, RECURSIVE_SCHEMA)
    assert "Child (node reaches itself)" in emitted
    assert "A document that states it is refused" in emitted


GROUP_ENUM = """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:group name="colourGroup">
    <xs:sequence>
      <xs:element name="Colour">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="Red"/>
            <xs:enumeration value="Blue"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
    </xs:sequence>
  </xs:group>
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence><xs:group ref="colourGroup"/></xs:sequence>
  </xs:complexType>
</xs:schema>
"""


def test_an_inline_enumeration_on_a_group_member_keeps_its_spellings(tmp_path):
    """A group member is copied, and the copy carries its inline spellings."""
    emitted, source = _emit(tmp_path, GROUP_ENUM)
    assert "std::int16_t colour" in _record(emitted, "root_type")
    assert "_colour_spellings[] = {" in source
    assert "Red" in source and "Blue" in source


def test_two_schemas_that_disagree_about_one_global_element_are_refused(tmp_path):
    """One name must mean one type, whichever file declared it."""
    (tmp_path / "first.xsd").write_text(
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="Shared" type="firstType"/>
  <xs:complexType name="firstType">
    <xs:sequence><xs:element type="xs:string" name="Alpha"/></xs:sequence>
  </xs:complexType>
</xs:schema>
"""
    )
    (tmp_path / "second.xsd").write_text(
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="Shared" type="secondType"/>
  <xs:complexType name="secondType">
    <xs:sequence><xs:element type="xs:string" name="Beta"/></xs:sequence>
  </xs:complexType>
  <xs:element type="rootType" name="Root"/>
  <xs:complexType name="rootType">
    <xs:sequence><xs:element ref="Shared"/></xs:sequence>
  </xs:complexType>
</xs:schema>
"""
    )
    result = subprocess.run(
        [
            sys.executable,
            str(GENERATOR),
            "--schema",
            str(tmp_path / "first.xsd"),
            "--schema",
            str(tmp_path / "second.xsd"),
            "--root-element",
            "Root",
            "--root-type",
            "rootType",
            "--accessor",
            "root_document",
            "--header",
            str(tmp_path / "out.hpp"),
            "--source",
            str(tmp_path / "out.cpp"),
        ],
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    assert "Shared" in result.stderr
