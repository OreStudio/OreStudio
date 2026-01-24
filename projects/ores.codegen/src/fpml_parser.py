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

# Mapping of entity directory names to subject areas
ENTITY_SUBJECT_AREA_MAP = {
    # Parties - party identification and classification
    'party-roles': 'Parties',
    'person-roles': 'Parties',
    'party-relationships': 'Parties',
    'entity-classifications': 'Parties',
    # Trading - trading infrastructure
    'account-types': 'Trading',
    'business-centres': 'Trading',
    'business-processes': 'Trading',
    'cashflow-types': 'Trading',
    # Market Data - asset and benchmark reference
    'asset-classes': 'Market Data',
    'asset-measures': 'Market Data',
    'benchmark-rates': 'Market Data',
    # Regulatory - regulatory bodies and regimes
    'local-jurisdictions': 'Regulatory',
    'regulatory-corporate-sectors': 'Regulatory',
    'reporting-regimes': 'Regulatory',
    'supervisory-bodies': 'Regulatory',
    # Currencies - currency codes
    'non-iso-currencies': 'Currencies',
}

# Mapping of entity names to file patterns for flat directory structure
# The pattern is used to find XML files when they're not in subdirectories
ENTITY_FILE_PATTERNS = {
    'account-types': ['account-type-*.xml'],
    'asset-classes': ['asset-class-*.xml'],
    'asset-measures': ['asset-measure-*.xml'],
    'benchmark-rates': ['benchmark-rate-*.xml'],
    'business-centres': ['business-center-*.xml'],
    'business-processes': ['business-process-*.xml'],
    'cashflow-types': ['cashflow-type-*.xml'],
    'entity-classifications': ['entity-type-*.xml', 'cftc-entity-classification-*.xml', 'cftc-organization-type-*.xml'],
    'local-jurisdictions': ['local-jurisdiction-*.xml'],
    'non-iso-currencies': ['non-iso-currency-*.xml'],
    'party-relationships': ['party-relationship-type-*.xml', 'hkma-rewrite-party-relationship-type-*.xml'],
    'party-roles': ['party-role-*.xml'],
    'person-roles': ['person-role-*.xml'],
    'regulatory-corporate-sectors': ['regulatory-corporate-sector-*.xml', 'hkma-rewrite-regulatory-corporate-sector-*.xml'],
    'reporting-regimes': ['reporting-regime-*.xml'],
    'supervisory-bodies': ['supervisory-body-*.xml'],
}

# Enrichment data for non-ISO currencies
# These currencies are from FPML but need additional metadata to populate
# the shared dq_currencies_artefact_tbl (which has a richer schema than
# standard FPML reference data tables).
#
# Fields: symbol, fraction_symbol, fractions_per_unit, rounding_type,
#         rounding_precision, format, currency_type, flag_key
NON_ISO_CURRENCY_ENRICHMENT = {
    # Offshore Chinese Yuan variants
    'CNH': {
        'name': 'Offshore Chinese Yuan (Hong Kong)',
        'symbol': '¥', 'fraction_symbol': '分', 'fractions_per_unit': 100,
        'rounding_type': 'standard', 'rounding_precision': 2,
        'format': '¥#,##0.00', 'currency_type': 'fiat.offshore', 'flag_key': 'hk'
    },
    'CNT': {
        'name': 'Offshore Chinese Yuan (Taiwan)',
        'symbol': '¥', 'fraction_symbol': '分', 'fractions_per_unit': 100,
        'rounding_type': 'standard', 'rounding_precision': 2,
        'format': '¥#,##0.00', 'currency_type': 'fiat.offshore', 'flag_key': 'tw'
    },
    # British Crown Dependencies (pegged to GBP)
    'GGP': {
        'name': 'Guernsey Pound',
        'symbol': '£', 'fraction_symbol': 'p', 'fractions_per_unit': 100,
        'rounding_type': 'standard', 'rounding_precision': 2,
        'format': '£#,##0.00', 'currency_type': 'fiat.emerging', 'flag_key': 'gg'
    },
    'IMP': {
        'name': 'Isle of Man Pound',
        'symbol': '£', 'fraction_symbol': 'p', 'fractions_per_unit': 100,
        'rounding_type': 'standard', 'rounding_precision': 2,
        'format': '£#,##0.00', 'currency_type': 'fiat.emerging', 'flag_key': 'im'
    },
    'JEP': {
        'name': 'Jersey Pound',
        'symbol': '£', 'fraction_symbol': 'p', 'fractions_per_unit': 100,
        'rounding_type': 'standard', 'rounding_precision': 2,
        'format': '£#,##0.00', 'currency_type': 'fiat.emerging', 'flag_key': 'je'
    },
    # Pacific Island currencies (pegged to AUD)
    'KID': {
        'name': 'Kiribati Dollar',
        'symbol': '$', 'fraction_symbol': '¢', 'fractions_per_unit': 100,
        'rounding_type': 'standard', 'rounding_precision': 2,
        'format': '$#,##0.00', 'currency_type': 'fiat.emerging', 'flag_key': 'ki'
    },
    'TVD': {
        'name': 'Tuvalu Dollar',
        'symbol': '$', 'fraction_symbol': '¢', 'fractions_per_unit': 100,
        'rounding_type': 'standard', 'rounding_precision': 2,
        'format': '$#,##0.00', 'currency_type': 'fiat.emerging', 'flag_key': 'tv'
    },
    # Historical European currencies
    'MCF': {
        'name': 'Monegasque Franc',
        'symbol': '₣', 'fraction_symbol': 'c', 'fractions_per_unit': 100,
        'rounding_type': 'standard', 'rounding_precision': 2,
        'format': '₣#,##0.00', 'currency_type': 'fiat.historical', 'flag_key': 'mc'
    },
    'SML': {
        'name': 'Sammarinese Lira',
        'symbol': '₤', 'fraction_symbol': 'c', 'fractions_per_unit': 100,
        'rounding_type': 'standard', 'rounding_precision': 2,
        'format': '₤#,##0.00', 'currency_type': 'fiat.historical', 'flag_key': 'sm'
    },
    'VAL': {
        'name': 'Vatican Lira',
        'symbol': '₤', 'fraction_symbol': 'c', 'fractions_per_unit': 100,
        'rounding_type': 'standard', 'rounding_precision': 2,
        'format': '₤#,##0.00', 'currency_type': 'fiat.historical', 'flag_key': 'va'
    },
}

# Entities that use shared tables instead of their own entity-specific tables
# These entities have their artefact data stored in existing tables with richer schemas
ENTITIES_WITH_SHARED_TABLES = {
    'non-iso-currencies': {
        'artefact_table': 'dq_currencies_artefact_tbl',
        'production_table': 'refdata_currencies_tbl',
        'populate_function': 'dq_populate_currencies',
        'skip_schema_generation': True,
    }
}


def get_subject_area(entity_dir_name: str) -> str:
    """Get the subject area for an entity based on its directory name."""
    return ENTITY_SUBJECT_AREA_MAP.get(entity_dir_name, 'General')


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

    def to_sql_insert(self, subject_area: str = 'General') -> str:
        """Generate SQL upsert statement for this coding scheme."""
        code = self.to_code()
        name = self.short_name.replace("'", "''")
        definition = self.definition.replace("'", "''") if self.definition else ''
        uri = self.canonical_uri.replace("'", "''")

        return f"""select ores.upsert_dq_coding_schemes(
    '{code}',
    '{name}',
    'industry',
    '{subject_area}',
    'Reference Data',
    '{uri}',
    '{definition}'
);"""

    def to_artefact_insert(self, subject_area: str = 'General', use_variable: bool = True) -> str:
        """Generate SQL artefact insert statement for this coding scheme.

        Args:
            subject_area: The subject area for this coding scheme.
            use_variable: If True, use :v_dataset_id variable (requires \\gset setup).
        """
        code = self.to_code()
        name = self.short_name.replace("'", "''")
        definition = self.definition.replace("'", "''") if self.definition else ''
        uri = self.canonical_uri.replace("'", "''")

        return f"""insert into ores.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    '{code}', 0, '{name}', 'industry',
    '{subject_area}', 'Reference Data', '{uri}', '{definition}'
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
    subject_area: str = 'General'
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
                "subject_area": self.subject_area,
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
                "has_image_id": self.entity_plural == "business_centres",
                "indexes": [
                    {"name": "coding_scheme", "columns": "coding_scheme_code", "current_only": True}
                ],
                "artefact_indexes": [
                    {"name": "coding_scheme", "columns": "coding_scheme_code"}
                ],
                "has_artefact_insert_fn": False,
                **({"image_linking": {
                    "flags_dataset_code": "assets.country_flags",
                    "key_expression": "lower(substring(bc.code, 1, 2))",
                    "placeholder_key": "xx"
                }} if self.entity_plural == "business_centres" else {})
            }
        }

    def get_datasets(self) -> list[dict]:
        """Generate a list of dataset info dicts, one per coding scheme.

        For entities with multiple coding schemes (e.g., entity_classifications),
        this produces multiple datasets. Each dataset contains only the rows
        belonging to that coding scheme.

        For non-ISO currencies, enrichment data is added to populate the
        shared dq_currencies_artefact_tbl (which has a richer schema).
        """
        datasets = []
        is_non_iso_currency = self.entity_plural == 'non_iso_currencies'

        for cs in self.coding_schemes:
            coding_scheme_code = cs.to_code()
            # Filter rows for this coding scheme
            scheme_rows = [r for r in self.rows if r.coding_scheme_code == coding_scheme_code]

            # Derive dataset code from coding scheme (e.g., FPML_ENTITY_TYPE -> fpml.entity_types)
            # Remove FPML_ prefix and convert to lowercase with dots
            scheme_suffix = coding_scheme_code.replace('FPML_', '').lower()
            dataset_code = f"fpml.{scheme_suffix}"

            # Convert to display name (e.g., FPML_ENTITY_TYPE -> FpML Entity Type)
            display_name = 'FpML ' + ' '.join(
                word.capitalize() for word in scheme_suffix.split('_')
            )

            items = []
            for row in scheme_rows:
                item = {
                    "code": row.code,
                    "coding_scheme_code": row.coding_scheme_code,
                    "source": row.source,
                    "description": row.description
                }
                # Add enrichment data for non-ISO currencies
                if is_non_iso_currency and row.code in NON_ISO_CURRENCY_ENRICHMENT:
                    enrichment = NON_ISO_CURRENCY_ENRICHMENT[row.code]
                    item.update({
                        "name": enrichment['name'],
                        "numeric_code": "",  # Non-ISO currencies don't have ISO numeric codes
                        "symbol": enrichment['symbol'],
                        "fraction_symbol": enrichment['fraction_symbol'],
                        "fractions_per_unit": enrichment['fractions_per_unit'],
                        "rounding_type": enrichment['rounding_type'],
                        "rounding_precision": enrichment['rounding_precision'],
                        "format": enrichment['format'],
                        "currency_type": enrichment['currency_type'],
                        "flag_key": enrichment['flag_key'],
                    })
                items.append(item)

            entity_data = {
                "entity_singular": self.entity_name,
                "entity_plural": self.entity_plural,
                "subject_area": self.subject_area,
            }
            # Add image linking config for entities that need it
            if self.entity_plural == "business_centres":
                entity_data["image_linking"] = {
                    "flags_dataset_code": "assets.country_flags",
                    "key_expression": "lower(substring(bc.code, 1, 2))",
                    "placeholder_key": "xx"
                }

            dataset_entry = {
                "dataset": {
                    "code": dataset_code,
                    "name": display_name,
                    "coding_scheme_code": coding_scheme_code,
                    "description": cs.definition or f"Reference data for {display_name}",
                    "source_version": cs.version,
                    "canonical_version_uri": cs.canonical_version_uri,
                    "publication_date": cs.publication_date,
                },
                "entity": entity_data,
                "items": items,
            }

            # Mark non-ISO currencies as using shared tables
            if is_non_iso_currency:
                dataset_entry["uses_shared_currency_table"] = True
                dataset_entry["shared_table_config"] = {
                    "artefact_table": "dq_currencies_artefact_tbl",
                    "production_table": "refdata_currencies_tbl",
                    "populate_function": "dq_populate_currencies",
                }

            datasets.append(dataset_entry)

        return datasets

    def to_populate_data(self) -> dict:
        """Generate JSON data for populate script generation.

        DEPRECATED: Use get_datasets() instead for proper per-coding-scheme datasets.
        This method is kept for backward compatibility but returns data for first scheme only.
        """
        datasets = self.get_datasets()
        if not datasets:
            return {}
        # Return first dataset for backward compatibility
        return datasets[0]


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


def process_entity(input_dir: Path, entity_name: str) -> MergedEntity:
    """
    Process FPML XML files for a given entity from a flat directory.

    Uses ENTITY_FILE_PATTERNS to find matching files.
    If multiple files exist, merge them tracking each row's source coding scheme.
    """
    patterns = ENTITY_FILE_PATTERNS.get(entity_name, [])
    if not patterns:
        raise ValueError(f"No file patterns defined for entity: {entity_name}")

    xml_files = []
    for pattern in patterns:
        xml_files.extend(input_dir.glob(pattern))

    if not xml_files:
        raise ValueError(f"No XML files found for entity {entity_name}")

    entity_singular, entity_plural = derive_entity_name(entity_name)
    subject_area = get_subject_area(entity_name)

    merged = MergedEntity(
        entity_name=entity_singular,
        entity_plural=entity_plural,
        description=f"Reference data for {entity_plural.replace('_', ' ')}",
        subject_area=subject_area
    )

    seen_codes: dict[tuple[str, str], CodeListRow] = {}

    for xml_file in sorted(xml_files):
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


def load_manifest(manifest_path: Path) -> dict:
    """Load the data manifest JSON file."""
    if not manifest_path.exists():
        return {}
    with open(manifest_path, 'r') as f:
        return json.load(f)


def load_methodology_text(methodology_path: Path) -> str:
    """Load the methodology text file."""
    if not methodology_path.exists():
        return ''
    return methodology_path.read_text().strip()


def generate_catalog_sql(manifest: dict, output_path: Path):
    """Generate SQL file with catalog upsert from manifest."""
    if not manifest:
        print("Warning: No manifest found, skipping catalog generation")
        return

    catalog = manifest.get('catalog')
    if not catalog:
        print("Warning: No catalog in manifest, skipping catalog generation")
        return

    name = catalog.get('name', 'FpML Standards')
    description = catalog.get('description', '')
    owner = catalog.get('owner', 'Reference Data Team')

    # Escape single quotes for SQL
    name_escaped = name.replace("'", "''")
    description_escaped = description.replace("'", "''")
    owner_escaped = owner.replace("'", "''")

    lines = [
        "/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-",
        " *",
        " * FPML Catalog Population Script",
        " *",
        " * Auto-generated from external/fpml/manifest.json",
        " * This script is idempotent.",
        " */",
        "",
        "set schema 'ores';",
        "",
        "-- =============================================================================",
        "-- FpML Standards Catalog",
        "-- =============================================================================",
        "",
        "\\echo '--- FpML Standards Catalog ---'",
        "",
        f"select ores.upsert_dq_catalogs(",
        f"    '{name_escaped}',",
        f"    '{description_escaped}',",
        f"    '{owner_escaped}'",
        ");",
        ""
    ]

    output_path.write_text('\n'.join(lines))
    print(f"Generated: {output_path}")


def generate_methodology_sql(manifest: dict, methodology_text: str, output_path: Path):
    """Generate SQL file with methodology upsert from manifest."""
    if not manifest:
        print("Warning: No manifest found, skipping methodology generation")
        return

    name = manifest.get('name', 'FpML Genericode Download')
    description = manifest.get('description', '')
    source_url = manifest.get('source_url', '')
    downloaded_at = manifest.get('downloaded_at', '')

    # Escape single quotes for SQL
    name_escaped = name.replace("'", "''")
    description_escaped = description.replace("'", "''")
    source_url_escaped = source_url.replace("'", "''")

    # Build implementation details with download date
    impl_details_parts = []
    if downloaded_at:
        impl_details_parts.append(f"Last Download: {downloaded_at}")
        impl_details_parts.append("")
    if methodology_text:
        impl_details_parts.append(methodology_text)

    implementation_details = '\n'.join(impl_details_parts).replace("'", "''")

    lines = [
        "/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-",
        " *",
        " * FPML Methodology Population Script",
        " *",
        " * Auto-generated from external/fpml/manifest.json",
        " * This script is idempotent.",
        " */",
        "",
        "set schema 'ores';",
        "",
        "-- =============================================================================",
        "-- FPML Data Sourcing Methodology",
        "-- =============================================================================",
        "",
        "\\echo '--- FPML Methodology ---'",
        "",
        f"select ores.upsert_dq_methodologies(",
        f"    '{name_escaped}',",
        f"    '{description_escaped}',",
        f"    '{source_url_escaped}',",
        f"    '{implementation_details}'",
        ");",
        ""
    ]

    output_path.write_text('\n'.join(lines))
    print(f"Generated: {output_path}")


def generate_coding_schemes_dataset_sql(manifest: dict, output_path: Path):
    """Generate SQL file with coding schemes dataset upsert from manifest."""
    datasets = manifest.get('datasets', [])
    coding_scheme_datasets = [d for d in datasets if d.get('artefact_type') == 'coding_schemes']

    if not coding_scheme_datasets:
        print("Warning: No coding_schemes dataset in manifest, skipping")
        return

    lines = [
        "/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-",
        " *",
        " * FPML Coding Schemes Dataset Population Script",
        " *",
        " * Auto-generated from external/fpml/manifest.json",
        " * This must be run before other datasets that reference these coding schemes.",
        " */",
        "",
        "set schema 'ores';",
        "",
        "-- =============================================================================",
        "-- FPML Coding Schemes Dataset",
        "-- =============================================================================",
        "",
        "\\echo '--- FPML Coding Schemes Dataset ---'",
        ""
    ]

    for dataset in coding_scheme_datasets:
        name = dataset['name'].replace("'", "''")
        code = dataset['code']
        catalog = dataset['catalog'].replace("'", "''")
        subject_area = dataset['subject_area'].replace("'", "''")
        domain = dataset['domain'].replace("'", "''")
        coding_scheme = dataset['coding_scheme']
        methodology = dataset['methodology'].replace("'", "''")
        description = dataset['description'].replace("'", "''")
        source_system = dataset['source_system'].replace("'", "''")
        business_context = dataset['business_context'].replace("'", "''")
        license_info = dataset['license'].replace("'", "''")
        artefact_type = dataset['artefact_type']
        target_table = dataset['target_table']
        populate_function = dataset['populate_function']

        lines.append(f"-- {name}")
        lines.append("select ores.upsert_dq_datasets(")
        lines.append(f"    '{code}',")
        lines.append(f"    '{catalog}',")
        lines.append(f"    '{subject_area}',")
        lines.append(f"    '{domain}',")
        lines.append(f"    '{coding_scheme}',")
        lines.append("    'Primary',")
        lines.append("    'Actual',")
        lines.append("    'Raw',")
        lines.append(f"    '{methodology}',")
        lines.append(f"    '{name}',")
        lines.append(f"    '{description}',")
        lines.append(f"    '{source_system}',")
        lines.append(f"    '{business_context}',")
        lines.append(f"    '{license_info}',")
        lines.append(f"    '{artefact_type}',")
        lines.append(f"    '{target_table}',")
        lines.append(f"    '{populate_function}'")
        lines.append(");")
        lines.append("")

    output_path.write_text('\n'.join(lines))
    print(f"Generated: {output_path}")


def generate_dataset_dependency_sql(manifest: dict, output_path: Path):
    """Generate SQL file with dataset dependency upserts from manifest."""
    dependencies = manifest.get('dependencies', [])

    lines = [
        "/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-",
        " *",
        " * FPML Dataset Dependencies Population Script",
        " *",
        " * Auto-generated from external/fpml/manifest.json",
        " * Must be run after fpml dataset populate scripts.",
        " */",
        "",
        "set schema 'ores';",
        "",
        "-- =============================================================================",
        "-- FPML Dataset Dependencies",
        "-- =============================================================================",
        "",
        "\\echo '--- FPML Dataset Dependencies ---'",
        ""
    ]

    for dep in dependencies:
        dataset_code = dep['dataset_code']
        dependency_code = dep['dependency_code']
        role = dep['role']

        lines.append(f"select ores.upsert_dq_dataset_dependency(")
        lines.append(f"    '{dataset_code}',")
        lines.append(f"    '{dependency_code}',")
        lines.append(f"    '{role}'")
        lines.append(");")
        lines.append("")

    output_path.write_text('\n'.join(lines))
    print(f"Generated: {output_path}")


def generate_fpml_sql(output_dir: Path, dataset_files: list[str], artefact_files: list[str]):
    """Generate fpml.sql master include file with all FPML files in correct order."""
    lines = [
        "/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-",
        " *",
        " * FPML Reference Data Master Include File",
        " *",
        " * Auto-generated by generate_fpml_refdata.sh",
        " * Includes all FPML SQL files in the correct dependency order.",
        " */",
        "",
        "-- =============================================================================",
        "-- FpML Standards Catalog (must come first)",
        "-- =============================================================================",
        "",
        "\\echo '--- FpML Standards Catalog ---'",
        "\\ir fpml_catalog_populate.sql",
        "",
        "-- =============================================================================",
        "-- FPML Methodology",
        "-- =============================================================================",
        "",
        "\\echo '--- FPML Methodology ---'",
        "\\ir fpml_methodology_populate.sql",
        "",
        "-- =============================================================================",
        "-- FPML Coding Schemes Dataset (must come first - other datasets reference these schemes)",
        "-- =============================================================================",
        "",
        "\\echo '--- FPML Coding Schemes Dataset ---'",
        "\\ir fpml_coding_schemes_dataset_populate.sql",
        "",
        "-- =============================================================================",
        "-- FPML Coding Schemes Artefacts",
        "-- =============================================================================",
        "",
        "\\echo '--- FPML Coding Schemes Artefacts ---'",
        "\\ir fpml_coding_schemes_artefact_populate.sql",
        "",
        "-- Publish coding schemes to production (required before other datasets can reference them)",
        "\\echo '--- Publishing FPML Coding Schemes ---'",
        "select * from ores.dq_populate_coding_schemes(",
        "    (select id from ores.dq_datasets_tbl where code = 'fpml.coding_schemes' and valid_to = ores.utility_infinity_timestamp_fn()),",
        "    'upsert'",
        ");",
        "",
        "-- =============================================================================",
        "-- FPML Datasets (depend on coding schemes being published)",
        "-- =============================================================================",
        "",
        "\\echo '--- FPML Datasets ---'",
    ]

    for f in sorted(dataset_files):
        lines.append(f"\\ir {f}")

    lines.extend([
        "",
        "-- =============================================================================",
        "-- FPML Dataset Dependencies",
        "-- =============================================================================",
        "",
        "\\echo '--- FPML Dataset Dependencies ---'",
        "\\ir fpml_dataset_dependency_populate.sql",
        "",
        "-- =============================================================================",
        "-- FPML Artefacts",
        "-- =============================================================================",
        "",
        "\\echo '--- FPML Artefacts ---'",
    ])

    for f in sorted(artefact_files):
        lines.append(f"\\ir {f}")

    lines.append("")

    output_path = output_dir / "fpml.sql"
    output_path.write_text('\n'.join(lines))
    print(f"Generated: {output_path}")


def generate_coding_schemes_sql(entities: list[MergedEntity], output_path: Path):
    """Generate SQL file with coding scheme artefact inserts."""
    dataset_code = 'fpml.coding_schemes'
    lines = [
        "/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-",
        " *",
        " * FPML Coding Schemes Artefact Population Script",
        " *",
        " * Auto-generated from FPML Genericode XML files.",
        " * Populates the dq_coding_schemes_artefact_tbl staging table.",
        " *",
        " * To publish to production:",
        f" *   SELECT * FROM ores.dq_populate_coding_schemes(",
        f" *       (SELECT id FROM ores.dq_datasets_tbl WHERE code = '{dataset_code}' AND valid_to = ores.utility_infinity_timestamp_fn()),",
        " *       'upsert'",
        " *   );",
        " */",
        "",
        "set schema 'ores';",
        "",
        "-- =============================================================================",
        "-- FPML Coding Schemes Artefacts",
        "-- =============================================================================",
        "",
        "\\echo '--- FPML Coding Schemes Artefacts ---'",
        "",
        "-- Store dataset_id in psql variable for reuse",
        f"select id as v_dataset_id from ores.dq_datasets_tbl where code = '{dataset_code}' and valid_to = ores.utility_infinity_timestamp_fn() \\gset",
        "",
        "-- Clear existing artefacts for this dataset before inserting",
        "delete from ores.dq_coding_schemes_artefact_tbl",
        "where dataset_id = :'v_dataset_id';",
        ""
    ]

    seen_schemes = set()
    for entity in entities:
        for cs in entity.coding_schemes:
            code = cs.to_code()
            if code not in seen_schemes:
                seen_schemes.add(code)
                lines.append(cs.to_artefact_insert(entity.subject_area))
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
    """Generate JSON data files for populate script generation.

    Generates one file per coding scheme/dataset. For entities with multiple
    coding schemes, multiple files are created.
    """
    datasets = entity.get_datasets()

    for dataset in datasets:
        # Use dataset code for filename (e.g., fpml.entity_type -> entity_type_data.json)
        dataset_code = dataset['dataset']['code']
        # Remove fpml. prefix and use as filename
        filename_base = dataset_code.replace('fpml.', '').replace('.', '_')
        output_path = output_dir / f"{filename_base}_data.json"

        with open(output_path, 'w') as f:
            json.dump(dataset, f, indent=2)

        print(f"Generated: {output_path}")


def main():
    parser = argparse.ArgumentParser(
        description='Parse FPML Genericode XML files and generate SQL/JSON outputs'
    )
    parser.add_argument('input_dir', type=Path,
                        help='Directory containing FPML codelist XML files (flat structure)')
    parser.add_argument('output_dir', type=Path,
                        help='Output directory for generated files')
    parser.add_argument('--external-dir', type=Path, default=None,
                        help='External data directory containing manifest.json (default: parent of input_dir)')
    parser.add_argument('--coding-schemes-only', action='store_true',
                        help='Only generate coding schemes SQL')
    parser.add_argument('--entities', type=str, nargs='*',
                        help='Specific entities to process (default: all)')
    parser.add_argument('--exclude', type=str, nargs='*', default=[],
                        help='Entities to exclude (e.g., currencies)')

    args = parser.parse_args()

    if not args.input_dir.exists():
        print(f"Error: Input directory does not exist: {args.input_dir}")
        sys.exit(1)

    args.output_dir.mkdir(parents=True, exist_ok=True)

    # Determine external directory (contains manifest.json and methodology.txt)
    external_dir = args.external_dir if args.external_dir else args.input_dir.parent

    exclude_set = set(args.exclude) if args.exclude else set()
    if exclude_set:
        print(f"Excluding entities: {', '.join(exclude_set)}")

    # Determine which entities to process
    if args.entities:
        entity_names = [e for e in args.entities if e not in exclude_set]
    else:
        entity_names = [e for e in ENTITY_FILE_PATTERNS.keys() if e not in exclude_set]

    print(f"Processing {len(entity_names)} entities from {args.input_dir}...")

    entities: list[MergedEntity] = []

    for entity_name in entity_names:
        print(f"\nProcessing: {entity_name}")
        try:
            entity = process_entity(args.input_dir, entity_name)
            entities.append(entity)
        except Exception as e:
            print(f"  Error: {e}")
            continue

    if not entities:
        print("No entities processed successfully")
        sys.exit(1)

    # Generate outputs
    print(f"\n--- Generating outputs to {args.output_dir} ---")

    # Load manifest and methodology text
    manifest = load_manifest(external_dir / "manifest.json")
    methodology_text = load_methodology_text(external_dir / "methodology.txt")

    # Generate catalog SQL
    generate_catalog_sql(
        manifest,
        args.output_dir / "fpml_catalog_populate.sql"
    )

    # Generate methodology SQL
    generate_methodology_sql(
        manifest,
        methodology_text,
        args.output_dir / "fpml_methodology_populate.sql"
    )

    # Generate dataset dependency SQL
    generate_dataset_dependency_sql(
        manifest,
        args.output_dir / "fpml_dataset_dependency_populate.sql"
    )

    # Generate coding schemes dataset SQL (must come before artefacts)
    generate_coding_schemes_dataset_sql(
        manifest,
        args.output_dir / "fpml_coding_schemes_dataset_populate.sql"
    )

    # Always generate coding schemes artefact SQL
    generate_coding_schemes_sql(
        entities,
        args.output_dir / "fpml_coding_schemes_artefact_populate.sql"
    )

    if not args.coding_schemes_only:
        # Create subdirectories for models and data
        models_dir = args.output_dir / "models"
        data_dir = args.output_dir / "data"
        models_dir.mkdir(exist_ok=True)
        data_dir.mkdir(exist_ok=True)

        # Track generated files for fpml.sql
        dataset_files = []
        artefact_files = []

        for entity in entities:
            # Skip schema generation for entities using shared tables (e.g., non-ISO currencies)
            # They use existing tables like dq_currencies_artefact_tbl
            entity_key = entity.entity_plural.replace('_', '-')
            if entity_key not in ENTITIES_WITH_SHARED_TABLES:
                generate_entity_model(entity, models_dir)
            else:
                print(f"  Skipping schema generation for {entity.entity_plural} (uses shared table)")

            generate_populate_data(entity, data_dir)

            # Collect dataset and artefact filenames
            datasets = entity.get_datasets()
            for ds in datasets:
                dataset_code = ds['dataset']['code']
                filename_base = dataset_code.replace('fpml.', '').replace('.', '_')
                dataset_files.append(f"fpml_{filename_base}_dataset_populate.sql")
                # All FPML artefacts use fpml_ prefix
                artefact_files.append(f"fpml_{filename_base}_artefact_populate.sql")

        # Generate fpml.sql master include file
        generate_fpml_sql(args.output_dir, dataset_files, artefact_files)

    print(f"\nDone! Processed {len(entities)} entities.")


if __name__ == '__main__':
    main()
