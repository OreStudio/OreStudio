#!/usr/bin/env python3
"""
Generates SQL populate scripts for ISO Standards metadata.

Reads manifest.json and methodology.txt from this directory and writes to
projects/ores.sql/populate/iso/:
  - iso_catalog_populate.sql
  - iso_methodology_populate.sql
  - iso_coding_schemes_dataset_populate.sql
  - iso_coding_schemes_artefact_populate.sql
  - iso_dataset_populate.sql
  - iso_dataset_tag_populate.sql
  - iso_dataset_dependency_populate.sql
  - iso_populate.sql (master include file)

iso_countries_artefact_populate.sql and iso_currencies_artefact_populate.sql
are maintained by hand. Nothing here derives their rows, because the standards
are published as documents and not as a redistributable list.

Usage:
    python3 external/iso/iso_generate_metadata_sql.py
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

    name = escape_sql_string(catalog['name'])
    description = escape_sql_string(catalog['description'])
    owner = escape_sql_string(catalog['owner'])

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write(f"""
/**
 * ISO Standards Catalog Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * This script is idempotent.
 */

DO $$
BEGIN
    -- =============================================================================
    -- ISO Standards Catalog
    -- =============================================================================

    -- --- ISO Standards Catalog ---

    PERFORM ores_dq_catalogs_upsert_fn(ores_utility_system_tenant_id_fn(),
        '{name}',
        '{description}',
        '{owner}'
    );
END $$;

""")

    print(f"  Generated catalog: {catalog['name']}")


def generate_coding_schemes_artefact_sql(manifest: dict, output_file: Path):
    """Generate the coding schemes artefact populate SQL file."""
    print(f"Generating {output_file.name}...")

    coding_schemes = manifest.get('coding_schemes', [])

    # Find the coding schemes dataset to get its code for UUID generation
    datasets = manifest.get('datasets', [])
    coding_scheme_dataset = next(
        (d for d in datasets if d.get('artefact_type') == 'coding_schemes'),
        None
    )

    if not coding_scheme_dataset:
        print("  No coding_schemes dataset defined, skipping")
        return

    dataset_code = coding_scheme_dataset['code']

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write(f"""
/**
 * ISO Standards Coding Schemes Artefact Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * Populates the dq_coding_schemes_artefact_tbl staging table.
 *
 * To publish to production:
 *   SELECT * FROM ores_dq_coding_schemes_publish_fn(
 *       (SELECT id FROM ores_dq_datasets_tbl WHERE code = '{dataset_code}' AND valid_to = ores_utility_infinity_timestamp_fn()),
 *       ores_utility_system_tenant_id_fn(),
 *       'upsert'
 *   );
 */

-- =============================================================================
-- ISO Standards Coding Schemes Artefacts
-- =============================================================================

\\echo '--- ISO Standards Coding Schemes Artefacts ---'

-- Store dataset_id in psql variable for reuse
select id as v_dataset_id from ores_dq_datasets_tbl where code = '{dataset_code}' and valid_to = ores_utility_infinity_timestamp_fn() \\gset

-- Clear existing artefacts for this dataset before inserting
delete from ores_dq_coding_schemes_artefact_tbl
where dataset_id = :'v_dataset_id';

""")

        for cs in coding_schemes:
            code = cs['code']
            name = escape_sql_string(cs['name'])
            authority_type = cs['authority_type']
            subject_area = escape_sql_string(cs['subject_area'])
            domain = escape_sql_string(cs['domain'])
            uri = cs.get('uri')
            uri_sql = "'" + uri.replace("'", "''") + "'" if uri else 'null'
            description = escape_sql_string(cs['description'])

            f.write(f"""insert into ores_dq_coding_schemes_artefact_tbl (
    tenant_id,
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
)
values (
    ores_utility_system_tenant_id_fn(),
    :'v_dataset_id',
    '{code}', 0, '{name}', '{authority_type}',
    '{subject_area}', '{domain}', {uri_sql}, '{description}'
);

""")

    print(f"  Generated {len(coding_schemes)} coding scheme artefact entries")


def generate_methodology_sql(manifest: dict, methodology_text: str, output_file: Path):
    """Generate the methodology populate SQL file."""
    print(f"Generating {output_file.name}...")

    methodologies = manifest.get('methodologies', [])

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * ISO Standards Methodology Population Script
 *
 * Auto-generated from external/iso/manifest.json and methodology.txt
 * This script is idempotent.
 */

DO $$
BEGIN
    -- =============================================================================
    -- ISO Standards Methodologies
    -- =============================================================================

    -- --- ISO Standards Methodologies ---

""")

        statements = []
        for meth in methodologies:
            name = escape_sql_string(meth['name'])
            description = escape_sql_string(meth['description'])
            url = meth['url']
            methodology_steps = escape_sql_string(methodology_text)

            statements.append(f"""    PERFORM ores_dq_methodologies_upsert_fn(ores_utility_system_tenant_id_fn(),
        '{name}',
        '{description}',
        '{url}',
        '{methodology_steps}'
    );
""")

        f.write("\n".join(statements))
        f.write("END $$;\n\n")

    print(f"  Generated {len(methodologies)} methodology entries")


def generate_coding_schemes_dataset_sql(manifest: dict, output_file: Path):
    """Generate the coding schemes dataset populate SQL file (separate for ordering)."""
    print(f"Generating {output_file.name}...")

    datasets = manifest.get('datasets', [])
    coding_scheme_datasets = [d for d in datasets if d.get('artefact_type') == 'coding_schemes']

    if not coding_scheme_datasets:
        print("  No coding_schemes dataset defined, skipping")
        return

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * ISO Coding Schemes Dataset Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * This must be run before other datasets that reference these coding schemes.
 */

DO $$
BEGIN
    -- =============================================================================
    -- ISO Coding Schemes Dataset
    -- =============================================================================

    -- --- ISO Coding Schemes Dataset ---

""")

        statements = []
        for dataset in coding_scheme_datasets:
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

            statements.append(f"""    -- {name}
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
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
        '{artefact_type}'
    );
""")

        f.write("\n".join(statements))
        f.write("END $$;\n\n")

    print(f"  Generated {len(coding_scheme_datasets)} coding schemes dataset entries")


def generate_dataset_sql(manifest: dict, output_file: Path):
    """Generate the dataset populate SQL file (excludes coding_schemes datasets)."""
    print(f"Generating {output_file.name}...")

    datasets = manifest.get('datasets', [])
    # Exclude coding_schemes datasets - they're in a separate file for ordering
    other_datasets = [d for d in datasets if d.get('artefact_type') != 'coding_schemes']

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(get_header())
        f.write("""
/**
 * ISO Standards Dataset Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * This script is idempotent.
 *
 * Note: Coding schemes dataset is in iso_coding_schemes_dataset_populate.sql
 */

DO $$
BEGIN
    -- =============================================================================
    -- ISO Standards Datasets
    -- =============================================================================

    -- --- ISO Standards Datasets ---

""")

        statements = []
        for dataset in other_datasets:
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

            statements.append(f"""    -- {name}
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
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
        '{artefact_type}'
    );
""")

        f.write("\n".join(statements))
        f.write("END $$;\n\n")

    print(f"  Generated {len(other_datasets)} dataset entries")


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

DO $$
BEGIN
    -- =============================================================================
    -- ISO Standards Dataset Tags
    -- =============================================================================

    -- --- ISO Standards Dataset Tags ---

""")

        statements = []
        for dataset in datasets:
            if 'tag_code' not in dataset:
                continue

            name = escape_sql_string(dataset['name'])
            subject_area = escape_sql_string(dataset['subject_area'])
            domain = escape_sql_string(dataset['domain'])
            tag_code = dataset['tag_code']
            tag_description = escape_sql_string(dataset['tag_description'])

            statements.append(f"""    PERFORM ores_dq_tags_upsert_fn(ores_utility_system_tenant_id_fn(),
        '{name}',
        '{subject_area}',
        '{domain}',
        '{tag_code}',
        '{tag_description}'
    );
""")

        f.write("\n".join(statements))
        f.write("END $$;\n\n")

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

DO $$
BEGIN
    -- =============================================================================
    -- ISO Standards Dataset Dependencies
    -- =============================================================================

    -- --- ISO Standards Dataset Dependencies ---

""")

        statements = []
        for dep in dependencies:
            dataset_code = dep['dataset_code']
            dependency_code = dep['dependency_code']
            role = dep['role']

            statements.append(f"""    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        '{dataset_code}',
        '{dependency_code}',
        '{role}'
    );
""")

        f.write("\n".join(statements))
        f.write("END $$;\n\n")

    print(f"  Generated {len(dependencies)} dataset dependency entries")


def generate_master_sql(output_file: Path):
    """Generate the iso_populate.sql master include file."""
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
-- ISO Standards Methodology
-- =============================================================================

\\echo '--- ISO Standards Methodology ---'
\\ir iso_methodology_populate.sql

-- =============================================================================
-- ISO Coding Schemes Dataset (must come first - other datasets reference these schemes)
-- =============================================================================

\\echo '--- ISO Coding Schemes Dataset ---'
\\ir iso_coding_schemes_dataset_populate.sql

-- =============================================================================
-- ISO Standards Coding Schemes Artefacts
-- =============================================================================

\\echo '--- ISO Standards Coding Schemes Artefacts ---'
\\ir iso_coding_schemes_artefact_populate.sql

-- Publish coding schemes to production (required before other datasets can reference them)
\\echo '--- Publishing ISO Coding Schemes ---'
select * from ores_dq_coding_schemes_publish_fn(
    (select id from ores_dq_datasets_tbl where code = 'iso.coding_schemes' and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_utility_system_tenant_id_fn(),
    'upsert'
);

-- =============================================================================
-- ISO Standards Datasets (countries, currencies - depend on coding schemes)
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
        manifest_dir = Path(__file__).resolve().parent

    if args.output_dir:
        output_dir = Path(args.output_dir)
    else:
        output_dir = repo_root / 'projects' / 'ores.sql' / 'populate' / 'iso'

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

    print(f"ISO Standards Metadata SQL Generator")
    print(f"====================================")
    print(f"Manifest:    {manifest_file}")
    print(f"Methodology: {methodology_file}")
    print(f"Output:      {output_dir}")
    print()

    # Load manifest
    with open(manifest_file, 'r', encoding='utf-8') as f:
        manifest = json.load(f)

    with open(methodology_file, 'r', encoding='utf-8') as f:
        methodology_text = f.read().strip()

    # Generate files
    generate_catalog_sql(manifest, output_dir / 'iso_catalog_populate.sql')
    generate_methodology_sql(manifest, methodology_text,
                             output_dir / 'iso_methodology_populate.sql')
    generate_coding_schemes_dataset_sql(manifest, output_dir / 'iso_coding_schemes_dataset_populate.sql')
    generate_coding_schemes_artefact_sql(manifest, output_dir / 'iso_coding_schemes_artefact_populate.sql')
    generate_dataset_sql(manifest, output_dir / 'iso_dataset_populate.sql')
    generate_dataset_tag_sql(manifest, output_dir / 'iso_dataset_tag_populate.sql')
    generate_dataset_dependency_sql(manifest, output_dir / 'iso_dataset_dependency_populate.sql')
    generate_master_sql(output_dir / 'iso_populate.sql')

    print()
    print("Generation complete!")


if __name__ == '__main__':
    main()
