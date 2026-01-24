#!/usr/bin/env python3
"""
Generates SQL populate scripts for ISO Standards metadata.

Reads the manifest.json from external/iso/ and generates:
  - iso_catalog_populate.sql
  - iso_coding_schemes_populate.sql
  - iso_methodology_populate.sql
  - iso_dataset_populate.sql
  - iso_dataset_tag_populate.sql
  - iso_dataset_dependency_populate.sql
  - iso.sql (master include file)

Usage:
    python3 iso_generate_metadata_sql.py
    python3 iso_generate_metadata_sql.py --manifest-dir /path/to/external/iso
    python3 iso_generate_metadata_sql.py --output-dir /path/to/output
"""

import argparse
import json
import sys
from pathlib import Path
from datetime import datetime


def get_repo_root() -> Path:
    """Get the repository root directory."""
    current = Path(__file__).resolve()
    for parent in current.parents:
        if (parent / '.git').exists():
            return parent
    raise RuntimeError("Could not find repository root")


def get_header() -> str:
    """Generate SQL file header."""
    year = datetime.now().year
    return f"""/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) {year} Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
"""


def escape_sql_string(s: str) -> str:
    """Escape single quotes for SQL strings."""
    if s is None:
        return ''
    return s.replace("'", "''")


def generate_catalog_sql(manifest: dict, output_file: Path):
    """Generate the catalog populate SQL file."""
    print(f"Generating {output_file.name}...")

    catalog = manifest.get('catalog')
    if not catalog:
        print("  No catalog defined in manifest, skipping")
        return

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * ISO Standards Catalog Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- ISO Standards Catalog
-- =============================================================================

\\echo '--- ISO Standards Catalog ---'

""")
        name = escape_sql_string(catalog['name'])
        description = escape_sql_string(catalog['description'])
        owner = escape_sql_string(catalog['owner'])

        f.write(f"""select ores.upsert_dq_catalogs(
    '{name}',
    '{description}',
    '{owner}'
);
""")

    print(f"  Generated catalog: {catalog['name']}")


def generate_coding_schemes_sql(manifest: dict, output_file: Path):
    """Generate the coding schemes populate SQL file."""
    print(f"Generating {output_file.name}...")

    coding_schemes = manifest.get('coding_schemes', [])

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * ISO Standards Coding Schemes Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- ISO Standards Coding Schemes
-- =============================================================================

\\echo '--- ISO Standards Coding Schemes ---'

""")

        for cs in coding_schemes:
            code = cs['code']
            name = escape_sql_string(cs['name'])
            authority_type = cs['authority_type']
            subject_area = escape_sql_string(cs['subject_area'])
            domain = escape_sql_string(cs['domain'])
            uri = cs.get('uri') or 'null'
            if uri != 'null':
                uri = f"'{uri}'"
            description = escape_sql_string(cs['description'])

            f.write(f"""select ores.upsert_dq_coding_schemes(
    '{code}',
    '{name}',
    '{authority_type}',
    '{subject_area}',
    '{domain}',
    {uri},
    '{description}'
);

""")

    print(f"  Generated {len(coding_schemes)} coding scheme entries")


def generate_methodology_sql(manifest: dict, output_file: Path):
    """Generate the methodology populate SQL file."""
    print(f"Generating {output_file.name}...")

    methodologies = manifest.get('methodologies', [])

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * ISO Standards Methodology Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- ISO Standards Methodologies
-- =============================================================================

\\echo '--- ISO Standards Methodologies ---'

""")

        for meth in methodologies:
            name = escape_sql_string(meth['name'])
            description = escape_sql_string(meth['description'])
            url = meth['url']

            # Build methodology steps
            methodology_steps = f"""Data sourced from {url}

See methodology documentation for detailed steps."""

            f.write(f"""select ores.upsert_dq_methodologies(
    '{name}',
    '{description}',
    '{url}',
    '{escape_sql_string(methodology_steps)}'
);

""")

    print(f"  Generated {len(methodologies)} methodology entries")


def generate_dataset_sql(manifest: dict, output_file: Path):
    """Generate the dataset populate SQL file."""
    print(f"Generating {output_file.name}...")

    datasets = manifest.get('datasets', [])

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * ISO Standards Dataset Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- ISO Standards Datasets
-- =============================================================================

\\echo '--- ISO Standards Datasets ---'

""")

        for dataset in datasets:
            name = escape_sql_string(dataset['name'])
            code = dataset['code']
            catalog = escape_sql_string(dataset['catalog'])
            subject_area = escape_sql_string(dataset['subject_area'])
            domain = escape_sql_string(dataset['domain'])
            coding_scheme = dataset['coding_scheme']
            methodology = escape_sql_string(dataset['methodology'])
            description = escape_sql_string(dataset['description'])
            source_system = escape_sql_string(dataset['source_system'])
            business_context = escape_sql_string(dataset['business_context'])
            license_info = escape_sql_string(dataset['license'])
            artefact_type = dataset['artefact_type']
            target_table = dataset['target_table']
            populate_function = dataset['populate_function']

            f.write(f"""-- {name}
select ores.upsert_dq_datasets(
    '{code}',
    '{catalog}',
    '{subject_area}',
    '{domain}',
    '{coding_scheme}',
    'Primary',
    'Actual',
    'Raw',
    '{methodology}',
    '{name}',
    '{description}',
    '{source_system}',
    '{business_context}',
    current_date,
    '{license_info}',
    '{artefact_type}',
    '{target_table}',
    '{populate_function}'
);

""")

    print(f"  Generated {len(datasets)} dataset entries")


def generate_dataset_tag_sql(manifest: dict, output_file: Path):
    """Generate the dataset tag populate SQL file."""
    print(f"Generating {output_file.name}...")

    datasets = manifest.get('datasets', [])

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * ISO Standards Dataset Tags
 *
 * Auto-generated from external/iso/manifest.json
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- ISO Standards Dataset Tags
-- =============================================================================

\\echo '--- ISO Standards Dataset Tags ---'

""")

        for dataset in datasets:
            if 'tag_code' not in dataset:
                continue

            name = escape_sql_string(dataset['name'])
            subject_area = escape_sql_string(dataset['subject_area'])
            domain = escape_sql_string(dataset['domain'])
            tag_code = dataset['tag_code']
            tag_description = escape_sql_string(dataset['tag_description'])

            f.write(f"""select ores.upsert_dq_tag(
    '{name}',
    '{subject_area}',
    '{domain}',
    '{tag_code}',
    '{tag_description}'
);

""")

    tag_count = sum(1 for d in datasets if 'tag_code' in d)
    print(f"  Generated {tag_count} dataset tag entries")


def generate_dataset_dependency_sql(manifest: dict, output_file: Path):
    """Generate the dataset dependency populate SQL file."""
    print(f"Generating {output_file.name}...")

    dependencies = manifest.get('dependencies', [])

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * ISO Standards Dataset Dependencies
 *
 * Auto-generated from external/iso/manifest.json
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- ISO Standards Dataset Dependencies
-- =============================================================================

\\echo '--- ISO Standards Dataset Dependencies ---'

""")

        for dep in dependencies:
            dataset_code = dep['dataset_code']
            dependency_code = dep['dependency_code']
            role = dep['role']

            f.write(f"""select ores.upsert_dq_dataset_dependency(
    '{dataset_code}',
    '{dependency_code}',
    '{role}'
);

""")

    print(f"  Generated {len(dependencies)} dataset dependency entries")


def generate_master_sql(output_file: Path):
    """Generate the iso.sql master include file."""
    print(f"Generating {output_file.name}...")

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write("""/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * ISO Standards Reference Data Master Include File
 *
 * Auto-generated by iso_generate_metadata_sql.py
 * Includes all ISO Standards SQL files in the correct dependency order.
 */

-- =============================================================================
-- ISO Standards Catalog (must come first)
-- =============================================================================

\\echo '--- ISO Standards Catalog ---'
\\ir iso_catalog_populate.sql

-- =============================================================================
-- ISO Standards Coding Schemes
-- =============================================================================

\\echo '--- ISO Standards Coding Schemes ---'
\\ir iso_coding_schemes_populate.sql

-- =============================================================================
-- ISO Standards Methodology
-- =============================================================================

\\echo '--- ISO Standards Methodology ---'
\\ir iso_methodology_populate.sql

-- =============================================================================
-- ISO Standards Datasets
-- =============================================================================

\\echo '--- ISO Standards Datasets ---'
\\ir iso_dataset_populate.sql

-- =============================================================================
-- ISO Standards Dataset Tags
-- =============================================================================

\\echo '--- ISO Standards Dataset Tags ---'
\\ir iso_dataset_tag_populate.sql

-- =============================================================================
-- ISO Standards Dataset Dependencies
-- =============================================================================

\\echo '--- ISO Standards Dataset Dependencies ---'
\\ir iso_dataset_dependency_populate.sql

-- =============================================================================
-- ISO Countries Artefacts
-- =============================================================================

\\echo '--- ISO Countries Artefacts ---'
\\ir iso_countries_artefact_populate.sql

-- =============================================================================
-- ISO Currencies Artefacts
-- =============================================================================

\\echo '--- ISO Currencies Artefacts ---'
\\ir iso_currencies_artefact_populate.sql
""")


def main():
    parser = argparse.ArgumentParser(
        description='Generate SQL populate scripts for ISO Standards metadata.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument('--manifest-dir', '-m',
                        help='Directory containing manifest.json')
    parser.add_argument('--output-dir', '-o',
                        help='Output directory for generated SQL files')

    args = parser.parse_args()

    # Determine paths
    repo_root = get_repo_root()

    if args.manifest_dir:
        manifest_dir = Path(args.manifest_dir)
    else:
        manifest_dir = repo_root / 'external' / 'iso'

    if args.output_dir:
        output_dir = Path(args.output_dir)
    else:
        output_dir = repo_root / 'projects' / 'ores.sql' / 'populate' / 'iso'

    # Validate paths
    manifest_file = manifest_dir / 'manifest.json'

    if not manifest_file.exists():
        print(f"Error: Manifest file not found: {manifest_file}", file=sys.stderr)
        sys.exit(1)

    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"ISO Standards Metadata SQL Generator")
    print(f"====================================")
    print(f"Manifest: {manifest_file}")
    print(f"Output:   {output_dir}")
    print()

    # Load manifest
    with open(manifest_file, 'r', encoding='utf-8') as f:
        manifest = json.load(f)

    # Generate files
    generate_catalog_sql(manifest, output_dir / 'iso_catalog_populate.sql')
    generate_coding_schemes_sql(manifest, output_dir / 'iso_coding_schemes_populate.sql')
    generate_methodology_sql(manifest, output_dir / 'iso_methodology_populate.sql')
    generate_dataset_sql(manifest, output_dir / 'iso_dataset_populate.sql')
    generate_dataset_tag_sql(manifest, output_dir / 'iso_dataset_tag_populate.sql')
    generate_dataset_dependency_sql(manifest, output_dir / 'iso_dataset_dependency_populate.sql')
    generate_master_sql(output_dir / 'iso.sql')

    print()
    print("Generation complete!")


if __name__ == '__main__':
    main()
