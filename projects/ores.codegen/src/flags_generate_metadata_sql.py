#!/usr/bin/env python3
"""
Generates SQL populate scripts for flag metadata (methodology, datasets, tags).

Reads the manifest.json and methodology.txt from external/flags/ and generates:
  - flags_methodology_populate.sql
  - flags_dataset_populate.sql
  - flags_dataset_tag_populate.sql
  - populate_flags.sql (master include file)

Usage:
    python3 flags_generate_metadata_sql.py
    python3 flags_generate_metadata_sql.py --manifest-dir /path/to/external/flags
    python3 flags_generate_metadata_sql.py --output-dir /path/to/output
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
 * Visual Assets Catalog Population Script
 *
 * Auto-generated from external/flags/manifest.json
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- Visual Assets Catalog
-- =============================================================================

\\echo '--- Visual Assets Catalog ---'

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


def generate_methodology_sql(manifest: dict, methodology_text: str, output_file: Path):
    """Generate the methodology populate SQL file."""
    print(f"Generating {output_file.name}...")

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * Flag Icons Methodology Population Script
 *
 * Auto-generated from external/flags/manifest.json
 * This script is idempotent.
 */

set schema 'ores';

-- =============================================================================
-- Flag Icons Data Sourcing Methodologies
-- =============================================================================

\\echo '--- Flag Icons Methodologies ---'

""")

        # Generate a methodology entry for each source
        for source in manifest.get('sources', []):
            name = escape_sql_string(source['name'])
            description = escape_sql_string(source['description'])
            url = source['url']

            # Build methodology steps from the methodology.txt content
            methodology_steps = escape_sql_string(methodology_text)

            f.write(f"""select ores.upsert_dq_methodologies(
    '{name}',
    '{description}',
    '{url}',
    'Last Download: {manifest.get("downloaded_at", "Unknown")}

{methodology_steps}'
);

""")

    print(f"  Generated {len(manifest.get('sources', []))} methodology entries")


def generate_dataset_sql(manifest: dict, output_file: Path):
    """Generate the dataset populate SQL file."""
    print(f"Generating {output_file.name}...")

    datasets = manifest.get('datasets', [])

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * Flag Icons Dataset Population Script
 *
 * Creates the dataset entries for flag icon reference data.
 * Auto-generated from external/flags/manifest.json
 * This must be run before populating the artefact tables.
 */

set schema 'ores';

-- =============================================================================
-- Flag Icons Datasets
-- =============================================================================

\\echo '--- Flag Icons Datasets ---'

""")

        for dataset in datasets:
            name = escape_sql_string(dataset['name'])
            catalog = escape_sql_string(dataset['catalog'])
            subject_area = escape_sql_string(dataset['subject_area'])
            domain = escape_sql_string(dataset['domain'])
            coding_scheme = dataset['coding_scheme']
            methodology = escape_sql_string(dataset['methodology'])
            description = escape_sql_string(dataset['description'])
            source_system = escape_sql_string(dataset['source_system'])
            business_context = escape_sql_string(dataset['business_context'])
            as_of_date = dataset['as_of_date']
            license_info = escape_sql_string(dataset['license'])
            artefact_type = dataset['artefact_type']

            code = dataset['code']

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
    '{as_of_date}'::date,
    '{license_info}',
    '{artefact_type}'
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
 * Flag Icons Dataset Tags
 *
 * Tags for flag icon datasets.
 * Auto-generated from external/flags/manifest.json
 * Must be run after flags_dataset_populate.sql.
 */

set schema 'ores';

-- =============================================================================
-- Flag Icons Dataset Tags
-- =============================================================================

\\echo '--- Flag Icons Dataset Tags ---'

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

    datasets = manifest.get('datasets', [])

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * Flag Icons Dataset Dependencies
 *
 * Dependencies between flag icon datasets.
 * Auto-generated from external/flags/manifest.json
 * Must be run after flags_dataset_populate.sql.
 */

set schema 'ores';

-- =============================================================================
-- Flag Icons Dataset Dependencies
-- =============================================================================

\\echo '--- Flag Icons Dataset Dependencies ---'

""")

        dep_count = 0
        for dataset in datasets:
            if 'dependencies' not in dataset:
                continue

            dataset_code = dataset['code']
            for dep in dataset['dependencies']:
                dep_code = dep['code']
                role = dep['role']

                f.write(f"""select ores.upsert_dq_dataset_dependency(
    '{dataset_code}',
    '{dep_code}',
    '{role}'
);

""")
                dep_count += 1

    print(f"  Generated {dep_count} dataset dependency entries")


def generate_master_sql(output_file: Path):
    """Generate the populate_flags.sql master include file."""
    print(f"Generating {output_file.name}...")

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write("""/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Flag Icons Reference Data Master Include File
 *
 * Auto-generated by flags_generate_metadata_sql.py
 * Includes all flag icon SQL files in the correct dependency order.
 */

-- =============================================================================
-- Visual Assets Catalog (must come first)
-- =============================================================================

\\echo '--- Visual Assets Catalog ---'
\\ir flags_catalog_populate.sql

-- =============================================================================
-- Flag Icons Methodology
-- =============================================================================

\\echo '--- Flag Icons Methodology ---'
\\ir flags_methodology_populate.sql

-- =============================================================================
-- Flag Icons Datasets
-- =============================================================================

\\echo '--- Flag Icons Datasets ---'
\\ir flags_dataset_populate.sql

-- =============================================================================
-- Flag Icons Dataset Tags
-- =============================================================================

\\echo '--- Flag Icons Dataset Tags ---'
\\ir flags_dataset_tag_populate.sql

-- =============================================================================
-- Flag Icons Dataset Dependencies
-- =============================================================================

\\echo '--- Flag Icons Dataset Dependencies ---'
\\ir flags_dataset_dependency_populate.sql

-- =============================================================================
-- Flag Icon Images
-- =============================================================================

\\echo '--- Flag Icon Images ---'
\\ir flags_images_artefact_populate.sql
""")


def main():
    parser = argparse.ArgumentParser(
        description='Generate SQL populate scripts for flag icon metadata.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument('--manifest-dir', '-m',
                        help='Directory containing manifest.json and methodology.txt')
    parser.add_argument('--output-dir', '-o',
                        help='Output directory for generated SQL files')

    args = parser.parse_args()

    # Determine paths
    repo_root = get_repo_root()

    if args.manifest_dir:
        manifest_dir = Path(args.manifest_dir)
    else:
        manifest_dir = repo_root / 'external' / 'flags'

    if args.output_dir:
        output_dir = Path(args.output_dir)
    else:
        output_dir = repo_root / 'projects' / 'ores.sql' / 'populate' / 'flags'

    # Validate paths
    manifest_file = manifest_dir / 'manifest.json'
    methodology_file = manifest_dir / 'methodology.txt'

    if not manifest_file.exists():
        print(f"Error: Manifest file not found: {manifest_file}", file=sys.stderr)
        sys.exit(1)

    if not methodology_file.exists():
        print(f"Error: Methodology file not found: {methodology_file}", file=sys.stderr)
        sys.exit(1)

    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"Flag Icons Metadata SQL Generator")
    print(f"==================================")
    print(f"Manifest:   {manifest_file}")
    print(f"Methodology: {methodology_file}")
    print(f"Output:     {output_dir}")
    print()

    # Load manifest
    with open(manifest_file, 'r', encoding='utf-8') as f:
        manifest = json.load(f)

    # Load methodology text
    with open(methodology_file, 'r', encoding='utf-8') as f:
        methodology_text = f.read().strip()

    # Generate files
    generate_catalog_sql(manifest, output_dir / 'flags_catalog_populate.sql')
    generate_methodology_sql(manifest, methodology_text,
                              output_dir / 'flags_methodology_populate.sql')
    generate_dataset_sql(manifest, output_dir / 'flags_dataset_populate.sql')
    generate_dataset_tag_sql(manifest, output_dir / 'flags_dataset_tag_populate.sql')
    generate_dataset_dependency_sql(manifest, output_dir / 'flags_dataset_dependency_populate.sql')
    generate_master_sql(output_dir / 'populate_flags.sql')

    print()
    print("Generation complete!")


if __name__ == '__main__':
    main()
