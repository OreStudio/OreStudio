#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
FPML Genericode XML Parser

Parses FPML coding scheme XML files (OASIS Genericode format) and generates:
1. JSON entity models for SQL schema generation
2. SQL populate scripts for reference data
3. Coding scheme entries for dq_coding_schemes_tbl

Usage:
    python fpml_parser.py <input_dir> <output_dir> [--coding-schemes-only]

The parser handles directories with multiple XML files by merging them,
tracking the source coding scheme for each row.
"""

import argparse
import json
import os
import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass, field, asdict
from pathlib import Path
from typing import Optional


# XML namespaces used in FPML Genericode files
GCL_NS = '{http://docs.oasis-open.org/codelist/ns/genericode/1.0/}'
DOC_NS = '{http://www.fpml.org/coding-scheme/documentation}'

# For findall with namespaces
NAMESPACES = {
    'gcl': 'http://docs.oasis-open.org/codelist/ns/genericode/1.0/',
    'doc': 'http://www.fpml.org/coding-scheme/documentation',
}


@dataclass
class CodingScheme:
    """Represents a coding scheme extracted from FPML XML."""
    short_name: str
    version: str
    canonical_uri: str
    canonical_version_uri: str
    location_uri: str
    definition: str
    publication_date: Optional[str] = None

    def to_code(self) -> str:
        """Generate a coding scheme code from the canonical URI."""
        # Extract the scheme name from URI and convert to uppercase with underscores
        # e.g., http://www.fpml.org/coding-scheme/entity-type -> FPML_ENTITY_TYPE
        uri_parts = self.canonical_uri.rstrip('/').split('/')
        scheme_name = uri_parts[-1]
        code = 'FPML_' + scheme_name.upper().replace('-', '_')
        return code

    def to_sql_insert(self) -> str:
        """Generate SQL upsert statement for this coding scheme."""
        code = self.to_code()
        name = self.short_name.replace("'", "''")
        definition = self.definition.replace("'", "''") if self.definition else ''

        return f"""select ores.upsert_dq_coding_schemes(
    '{code}',
    '{name}',
    'industry',
    'Reference Data',
    'Reference Data',
    '{self.canonical_uri}',
    '{definition}'
);"""


@dataclass
class CodeListRow:
    """Represents a single row in a code list."""
    code: str
    source: str
    description: str
    coding_scheme_code: str
    source_file: str


@dataclass
class CodeList:
    """Represents a complete code list with metadata and rows."""
    coding_scheme: CodingScheme
    rows: list[CodeListRow] = field(default_factory=list)


@dataclass
class MergedEntity:
    """Represents a merged entity from multiple coding schemes."""
    entity_name: str
    entity_plural: str
    description: str
    coding_schemes: list[CodingScheme] = field(default_factory=list)
    rows: list[CodeListRow] = field(default_factory=list)

    def to_json_model(self) -> dict:
        """Generate JSON model for entity schema generation.

        Note: Uses (code, coding_scheme_code) as the logical composite key.
        The bi-temporal PK will be (code, coding_scheme_code, valid_from, valid_to).

        The coding_scheme_code column is added via has_coding_scheme flag, not in columns.
        """
        return {
            "entity": {
                "component": "refdata",
                "entity_singular": self.entity_name,
                "entity_plural": self.entity_plural,
                "description": self.description,
                "primary_key": {
                    "column": "code",
                    "type": "text"
                },
                "composite_key": True,
                "composite_key_columns": ["code", "coding_scheme_code"],
                "columns": [
                    {"name": "source", "type": "text", "nullable": True},
                    {"name": "description", "type": "text", "nullable": True}
                ],
                "has_coding_scheme": True,
                "has_image_id": False,
                "indexes": [
                    {"name": "coding_scheme", "columns": "coding_scheme_code", "current_only": True}
                ],
                "artefact_indexes": [
                    {"name": "coding_scheme", "columns": "coding_scheme_code"}
                ],
                "has_artefact_insert_fn": False
            }
        }

    def to_populate_data(self) -> dict:
        """Generate JSON data for populate script generation."""
        items = []
        for row in self.rows:
            items.append({
                "code": row.code,
                "coding_scheme_code": row.coding_scheme_code,
                "source": row.source,
                "description": row.description
            })
        return {
            self.entity_plural: items,
            "coding_schemes": [cs.to_code() for cs in self.coding_schemes]
        }


def find_element(parent: ET.Element, local_name: str) -> Optional[ET.Element]:
    """Find element by local name, handling both namespaced and non-namespaced elements."""
    # Try without namespace first (common case)
    elem = parent.find(local_name)
    if elem is not None:
        return elem
    # Try with GCL namespace
    elem = parent.find(f'{GCL_NS}{local_name}')
    if elem is not None:
        return elem
    # Try with doc namespace
    elem = parent.find(f'{DOC_NS}{local_name}')
    return elem


def find_text(parent: ET.Element, local_name: str, default: str = '') -> str:
    """Find element text by local name."""
    elem = find_element(parent, local_name)
    if elem is not None and elem.text:
        return elem.text.strip()
    return default


def find_all_elements(parent: ET.Element, local_name: str) -> list[ET.Element]:
    """Find all elements by local name."""
    # Try without namespace first
    elems = parent.findall(local_name)
    if elems:
        return elems
    # Try with GCL namespace
    return parent.findall(f'{GCL_NS}{local_name}')


def parse_coding_scheme(root: ET.Element) -> CodingScheme:
    """Extract coding scheme metadata from XML root element."""
    ident = find_element(root, 'Identification')
    if ident is None:
        raise ValueError("No Identification element found")

    short_name = find_text(ident, 'ShortName')
    version = find_text(ident, 'Version')
    canonical_uri = find_text(ident, 'CanonicalUri')
    canonical_version_uri = find_text(ident, 'CanonicalVersionUri')
    location_uri = find_text(ident, 'LocationUri')

    # Extract definition from annotation
    definition = ''
    publication_date = None
    annotation = find_element(root, 'Annotation')
    if annotation is not None:
        desc = find_element(annotation, 'Description')
        if desc is not None:
            def_elem = find_element(desc, 'definition')
            if def_elem is not None and def_elem.text:
                definition = ' '.join(def_elem.text.split())
            pub_date_elem = find_element(desc, 'publicationDate')
            if pub_date_elem is not None and pub_date_elem.text:
                publication_date = pub_date_elem.text.strip()

    return CodingScheme(
        short_name=short_name,
        version=version,
        canonical_uri=canonical_uri,
        canonical_version_uri=canonical_version_uri,
        location_uri=location_uri,
        definition=definition,
        publication_date=publication_date
    )


def parse_rows(root: ET.Element, coding_scheme: CodingScheme, source_file: str) -> list[CodeListRow]:
    """Extract code list rows from XML root element."""
    rows = []
    coding_scheme_code = coding_scheme.to_code()

    simple_code_list = find_element(root, 'SimpleCodeList')
    if simple_code_list is None:
        return rows

    for row_elem in find_all_elements(simple_code_list, 'Row'):
        values = find_all_elements(row_elem, 'Value')

        code = ''
        source = ''
        description = ''

        # Values are in order: Code, Source, Description
        for i, value_elem in enumerate(values):
            simple_value = find_element(value_elem, 'SimpleValue')
            if simple_value is not None and simple_value.text:
                text = ' '.join(simple_value.text.split())
                if i == 0:
                    code = text
                elif i == 1:
                    source = text
                elif i == 2:
                    description = text

        if code:
            rows.append(CodeListRow(
                code=code,
                source=source,
                description=description,
                coding_scheme_code=coding_scheme_code,
                source_file=source_file
            ))

    return rows


def parse_xml_file(file_path: Path) -> CodeList:
    """Parse a single FPML XML file."""
    tree = ET.parse(file_path)
    root = tree.getroot()

    coding_scheme = parse_coding_scheme(root)
    rows = parse_rows(root, coding_scheme, file_path.name)

    return CodeList(coding_scheme=coding_scheme, rows=rows)


def derive_entity_name(directory_name: str) -> tuple[str, str]:
    """
    Derive singular and plural entity names from directory name.

    Examples:
        party-roles -> (party_role, party_roles)
        business-centres -> (business_centre, business_centres)
        entity-classifications -> (entity_classification, entity_classifications)
    """
    # Convert kebab-case to snake_case
    plural = directory_name.replace('-', '_')

    # Derive singular (simple heuristic)
    if plural.endswith('ies'):
        singular = plural[:-3] + 'y'
    elif plural.endswith('sses'):
        singular = plural[:-2]
    elif plural.endswith('xes') or plural.endswith('ches') or plural.endswith('shes'):
        singular = plural[:-2]
    elif plural.endswith('s'):
        singular = plural[:-1]
    else:
        singular = plural

    return singular, plural


def process_directory(dir_path: Path) -> MergedEntity:
    """
    Process a directory containing FPML XML files.

    If multiple files exist, merge them tracking each row's source coding scheme.
    """
    xml_files = sorted(dir_path.glob('*.xml'))
    if not xml_files:
        raise ValueError(f"No XML files found in {dir_path}")

    entity_singular, entity_plural = derive_entity_name(dir_path.name)

    merged = MergedEntity(
        entity_name=entity_singular,
        entity_plural=entity_plural,
        description=f"Reference data for {entity_plural.replace('_', ' ')}"
    )

    seen_codes: dict[tuple[str, str], CodeListRow] = {}

    for xml_file in xml_files:
        print(f"  Parsing {xml_file.name}...")
        code_list = parse_xml_file(xml_file)
        merged.coding_schemes.append(code_list.coding_scheme)

        for row in code_list.rows:
            key = (row.code, row.coding_scheme_code)
            if key not in seen_codes:
                seen_codes[key] = row
                merged.rows.append(row)
            else:
                print(f"    Warning: Duplicate code '{row.code}' in scheme "
                      f"'{row.coding_scheme_code}', keeping first occurrence")

    # Update description from first coding scheme
    if merged.coding_schemes:
        merged.description = merged.coding_schemes[0].definition or merged.description

    print(f"  Merged {len(merged.rows)} rows from {len(merged.coding_schemes)} scheme(s)")
    return merged


def generate_coding_schemes_sql(entities: list[MergedEntity], output_path: Path):
    """Generate SQL file with coding scheme upserts."""
    lines = [
        "/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-",
        " *",
        " * FPML Coding Schemes Population Script",
        " *",
        " * Auto-generated from FPML Genericode XML files.",
        " * This script is idempotent.",
        " */",
        "",
        "set schema 'ores';",
        "",
        "-- =============================================================================",
        "-- FPML Coding Schemes",
        "-- =============================================================================",
        "",
        "\\echo '--- FPML Coding Schemes ---'",
        ""
    ]

    seen_schemes = set()
    for entity in entities:
        for cs in entity.coding_schemes:
            code = cs.to_code()
            if code not in seen_schemes:
                seen_schemes.add(code)
                lines.append(cs.to_sql_insert())
                lines.append("")

    output_path.write_text('\n'.join(lines))
    print(f"Generated: {output_path}")


def generate_entity_model(entity: MergedEntity, output_dir: Path):
    """Generate JSON model file for entity schema generation."""
    output_path = output_dir / f"{entity.entity_plural}_entity.json"
    model = entity.to_json_model()

    with open(output_path, 'w') as f:
        json.dump(model, f, indent=2)

    print(f"Generated: {output_path}")


def generate_populate_data(entity: MergedEntity, output_dir: Path):
    """Generate JSON data file for populate script generation."""
    output_path = output_dir / f"{entity.entity_plural}_data.json"
    data = entity.to_populate_data()

    with open(output_path, 'w') as f:
        json.dump(data, f, indent=2)

    print(f"Generated: {output_path}")


def main():
    parser = argparse.ArgumentParser(
        description='Parse FPML Genericode XML files and generate SQL/JSON outputs'
    )
    parser.add_argument('input_dir', type=Path,
                        help='Directory containing FPML data subdirectories')
    parser.add_argument('output_dir', type=Path,
                        help='Output directory for generated files')
    parser.add_argument('--coding-schemes-only', action='store_true',
                        help='Only generate coding schemes SQL')
    parser.add_argument('--entities', type=str, nargs='*',
                        help='Specific entity directories to process (default: all)')
    parser.add_argument('--exclude', type=str, nargs='*', default=[],
                        help='Entity directories to exclude (e.g., currencies)')

    args = parser.parse_args()

    if not args.input_dir.exists():
        print(f"Error: Input directory does not exist: {args.input_dir}")
        sys.exit(1)

    args.output_dir.mkdir(parents=True, exist_ok=True)

    # Find all subdirectories with XML files
    exclude_set = set(args.exclude) if args.exclude else set()

    if args.entities:
        subdirs = [args.input_dir / e for e in args.entities if e not in exclude_set]
    else:
        subdirs = sorted([d for d in args.input_dir.iterdir()
                         if d.is_dir() and list(d.glob('*.xml')) and d.name not in exclude_set])

    if exclude_set:
        print(f"Excluding directories: {', '.join(exclude_set)}")

    if not subdirs:
        print("No directories with XML files found")
        sys.exit(1)

    print(f"Processing {len(subdirs)} entity directories...")
    entities: list[MergedEntity] = []

    for subdir in subdirs:
        if not subdir.exists():
            print(f"Warning: Directory does not exist: {subdir}")
            continue

        print(f"\nProcessing: {subdir.name}")
        try:
            entity = process_directory(subdir)
            entities.append(entity)
        except Exception as e:
            print(f"  Error: {e}")
            continue

    if not entities:
        print("No entities processed successfully")
        sys.exit(1)

    # Generate outputs
    print(f"\n--- Generating outputs to {args.output_dir} ---")

    # Always generate coding schemes SQL
    generate_coding_schemes_sql(
        entities,
        args.output_dir / "fpml_coding_schemes_populate.sql"
    )

    if not args.coding_schemes_only:
        # Create subdirectories for models and data
        models_dir = args.output_dir / "models"
        data_dir = args.output_dir / "data"
        models_dir.mkdir(exist_ok=True)
        data_dir.mkdir(exist_ok=True)

        for entity in entities:
            generate_entity_model(entity, models_dir)
            generate_populate_data(entity, data_dir)

    print(f"\nDone! Processed {len(entities)} entities.")


if __name__ == '__main__':
    main()
