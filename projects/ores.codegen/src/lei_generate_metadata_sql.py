#!/usr/bin/env python3
"""
Generates SQL populate scripts for GLEIF LEI metadata and artefact data.

Reads the manifest.json from external/lei/ and CSV subset files to generate:
  - lei_catalog_populate.sql
  - lei_methodology_populate.sql
  - lei_dataset_populate.sql
  - lei_dataset_dependency_populate.sql
  - lei_entities_small_artefact_populate.sql
  - lei_entities_large_artefact_populate.sql
  - lei_relationships_small_artefact_populate.sql
  - lei_relationships_large_artefact_populate.sql
  - lei_populate.sql (master include file)

Usage:
    python3 lei_generate_metadata_sql.py
    python3 lei_generate_metadata_sql.py --manifest-dir /path/to/external/lei
    python3 lei_generate_metadata_sql.py --output-dir /path/to/output
"""

import argparse
import csv
import glob
import json
import re
import sys
from pathlib import Path
from datetime import datetime


# CSV column mappings: CSV header -> artefact column name
LEI2_COLUMN_MAP = {
    'LEI': 'lei',
    'Entity.LegalName': 'entity_legal_name',
    'Entity.EntityCategory': 'entity_entity_category',
    'Entity.EntitySubCategory': 'entity_entity_sub_category',
    'Entity.EntityStatus': 'entity_entity_status',
    'Entity.LegalForm.EntityLegalFormCode': 'entity_legal_form_entity_legal_form_code',
    'Entity.LegalForm.OtherLegalForm': 'entity_legal_form_other_legal_form',
    'Entity.LegalJurisdiction': 'entity_legal_jurisdiction',
    'Entity.LegalAddress.FirstAddressLine': 'entity_legal_address_first_address_line',
    'Entity.LegalAddress.City': 'entity_legal_address_city',
    'Entity.LegalAddress.Region': 'entity_legal_address_region',
    'Entity.LegalAddress.Country': 'entity_legal_address_country',
    'Entity.LegalAddress.PostalCode': 'entity_legal_address_postal_code',
    'Entity.HeadquartersAddress.FirstAddressLine': 'entity_headquarters_address_first_address_line',
    'Entity.HeadquartersAddress.City': 'entity_headquarters_address_city',
    'Entity.HeadquartersAddress.Region': 'entity_headquarters_address_region',
    'Entity.HeadquartersAddress.Country': 'entity_headquarters_address_country',
    'Entity.HeadquartersAddress.PostalCode': 'entity_headquarters_address_postal_code',
    'Entity.EntityCreationDate': 'entity_entity_creation_date',
    'Registration.InitialRegistrationDate': 'registration_initial_registration_date',
    'Registration.LastUpdateDate': 'registration_last_update_date',
    'Registration.NextRenewalDate': 'registration_next_renewal_date',
    'Registration.RegistrationStatus': 'registration_registration_status',
}

RR_COLUMN_MAP = {
    'Relationship.StartNode.NodeID': 'relationship_start_node_node_id',
    'Relationship.StartNode.NodeIDType': 'relationship_start_node_node_id_type',
    'Relationship.EndNode.NodeID': 'relationship_end_node_node_id',
    'Relationship.EndNode.NodeIDType': 'relationship_end_node_node_id_type',
    'Relationship.RelationshipType': 'relationship_relationship_type',
    'Relationship.RelationshipStatus': 'relationship_relationship_status',
    'Relationship.Period.1.startDate': 'relationship_period_1_start_date',
    'Relationship.Period.1.endDate': 'relationship_period_1_end_date',
    'Registration.InitialRegistrationDate': 'registration_initial_registration_date',
    'Registration.LastUpdateDate': 'registration_last_update_date',
    'Registration.RegistrationStatus': 'registration_registration_status',
    'Registration.ValidationSources': 'registration_validation_sources',
}

# Timestamp columns (need ::timestamptz cast in SQL)
LEI2_TIMESTAMP_COLS = {
    'entity_entity_creation_date',
    'registration_initial_registration_date',
    'registration_last_update_date',
    'registration_next_renewal_date',
}

RR_TIMESTAMP_COLS = {
    'relationship_period_1_start_date',
    'relationship_period_1_end_date',
    'registration_initial_registration_date',
    'registration_last_update_date',
}


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


def escape_sql(s: str) -> str:
    """Escape single quotes for SQL strings."""
    if s is None:
        return ''
    return s.replace("'", "''")


def parse_as_of_date(filename: str) -> str:
    """Extract YYYYMMDD date from CSV filename and return as YYYY-MM-DD."""
    match = re.search(r'(\d{8})', filename)
    if match:
        d = match.group(1)
        return f"{d[:4]}-{d[4:6]}-{d[6:8]}"
    return datetime.now().strftime('%Y-%m-%d')


def find_csv_files(lei_dir: Path) -> dict:
    """Find LEI CSV subset files and return them grouped by type and size."""
    files = {}
    for csv_file in sorted(lei_dir.glob('*.csv')):
        name = csv_file.name
        if 'lei2' in name.lower() and 'subset-small' in name:
            files['lei2_small'] = csv_file
        elif 'lei2' in name.lower() and 'subset-large' in name:
            files['lei2_large'] = csv_file
        elif 'rr' in name.lower() and 'subset-small' in name:
            files['rr_small'] = csv_file
        elif 'rr' in name.lower() and 'subset-large' in name:
            files['rr_large'] = csv_file
    return files


def sql_value(val: str, is_timestamp: bool) -> str:
    """Format a CSV value as a SQL literal."""
    if val is None or val.strip() == '':
        return 'null'
    val = val.strip()
    if is_timestamp:
        return f"'{escape_sql(val)}'::timestamptz"
    return f"'{escape_sql(val)}'"


def read_csv_rows(csv_file: Path, column_map: dict, timestamp_cols: set) -> list:
    """Read CSV file and extract mapped columns as SQL value tuples."""
    rows = []
    with open(csv_file, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        # Verify all expected headers exist
        missing = set(column_map.keys()) - set(reader.fieldnames)
        if missing:
            print(f"  Warning: Missing CSV headers: {missing}", file=sys.stderr)

        for row in reader:
            values = []
            for csv_header, sql_col in column_map.items():
                val = row.get(csv_header, '')
                is_ts = sql_col in timestamp_cols
                values.append(sql_value(val, is_ts))
            rows.append(values)

    return rows


def generate_catalog_sql(manifest: dict, output_file: Path):
    """Generate the catalog populate SQL file."""
    print(f"Generating {output_file.name}...")

    catalog = manifest.get('catalog')
    if not catalog:
        print("  No catalog defined in manifest, skipping")
        return

    name = escape_sql(catalog['name'])
    description = escape_sql(catalog['description'])
    owner = escape_sql(catalog['owner'])

    with open(output_file, 'w', encoding='utf-8', newline='\n') as f:
        f.write(get_header())
        f.write(f"""
/**
 * GLEIF LEI Catalog Population Script
 *
 * Auto-generated from external/lei/manifest.json
 * This script is idempotent.
 */

-- =============================================================================
-- GLEIF LEI Catalog
-- =============================================================================

\\echo '--- GLEIF LEI Catalog ---'

select ores_dq_catalogs_upsert_fn(ores_iam_system_tenant_id_fn(),
    '{name}',
    '{description}',
    '{owner}'
);
""")

    print(f"  Generated catalog: {catalog['name']}")


def generate_methodology_sql(manifest: dict, output_file: Path):
    """Generate the methodology populate SQL file."""
    print(f"Generating {output_file.name}...")

    methodology = manifest.get('methodology')
    if not methodology:
        print("  No methodology defined in manifest, skipping")
        return

    name = escape_sql(methodology['name'])
    description = escape_sql(methodology['description'])
    url = methodology['url']
    methodology_steps = escape_sql(
        f"Data sourced from {url}\n\nSee methodology documentation for detailed steps."
    )

    with open(output_file, 'w', encoding='utf-8', newline='\n') as f:
        f.write(get_header())
        f.write(f"""
/**
 * GLEIF LEI Methodology Population Script
 *
 * Auto-generated from external/lei/manifest.json
 * This script is idempotent.
 */

-- =============================================================================
-- GLEIF LEI Methodology
-- =============================================================================

\\echo '--- GLEIF LEI Methodology ---'

select ores_dq_methodologies_upsert_fn(ores_iam_system_tenant_id_fn(),
    '{name}',
    '{description}',
    '{url}',
    '{methodology_steps}'
);
""")

    print(f"  Generated methodology: {methodology['name']}")


def generate_dataset_sql(manifest: dict, as_of_date: str, output_file: Path):
    """Generate the dataset populate SQL file."""
    print(f"Generating {output_file.name}...")

    datasets = manifest.get('datasets', [])
    catalog = manifest.get('catalog', {})
    catalog_name = escape_sql(catalog.get('name', ''))
    methodology = manifest.get('methodology', {})
    methodology_name = escape_sql(methodology.get('name', ''))

    with open(output_file, 'w', encoding='utf-8', newline='\n') as f:
        f.write(get_header())
        f.write("""
/**
 * GLEIF LEI Dataset Population Script
 *
 * Auto-generated from external/lei/manifest.json
 * This script is idempotent.
 */

-- =============================================================================
-- GLEIF LEI Datasets
-- =============================================================================

\\echo '--- GLEIF LEI Datasets ---'

""")

        for dataset in datasets:
            name = escape_sql(dataset['name'])
            code = dataset['code']
            subject_area = escape_sql(dataset['subject_area'])
            domain = escape_sql(dataset['domain'])
            coding_scheme = dataset.get('coding_scheme', 'NONE')
            artefact_type = dataset['artefact_type']

            f.write(f"""-- {name}
select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    '{code}',
    '{catalog_name}',
    '{subject_area}',
    '{domain}',
    '{coding_scheme}',
    'Primary',
    'Actual',
    'Raw',
    '{methodology_name}',
    '{name}',
    '{escape_sql(dataset.get("description", ""))}',
    'GLEIF',
    '{escape_sql(dataset.get("business_context", "GLEIF golden copy data"))}',
    '{as_of_date}'::date,
    'Open Data',
    '{artefact_type}'
);

""")

    print(f"  Generated {len(datasets)} dataset entries")


def generate_dataset_dependency_sql(manifest: dict, output_file: Path):
    """Generate the dataset dependency populate SQL file."""
    print(f"Generating {output_file.name}...")

    dependencies = manifest.get('dependencies', [])

    with open(output_file, 'w', encoding='utf-8', newline='\n') as f:
        f.write(get_header())
        f.write("""
/**
 * GLEIF LEI Dataset Dependencies
 *
 * Auto-generated from external/lei/manifest.json
 * This script is idempotent.
 */

-- =============================================================================
-- GLEIF LEI Dataset Dependencies
-- =============================================================================

\\echo '--- GLEIF LEI Dataset Dependencies ---'

""")

        for dep in dependencies:
            dataset_code = dep['dataset_code']
            dependency_code = dep['dependency_code']
            role = dep['role']

            f.write(f"""select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    '{dataset_code}',
    '{dependency_code}',
    '{role}'
);

""")

    print(f"  Generated {len(dependencies)} dataset dependency entries")


def generate_artefact_populate_sql(
    csv_file: Path,
    dataset_name: str,
    dataset_code: str,
    table_name: str,
    column_map: dict,
    timestamp_cols: set,
    output_file: Path,
):
    """Generate an artefact populate SQL file from a CSV subset."""
    print(f"Generating {output_file.name} from {csv_file.name}...")

    rows = read_csv_rows(csv_file, column_map, timestamp_cols)
    sql_columns = list(column_map.values())

    with open(output_file, 'w', encoding='utf-8', newline='\n') as f:
        f.write(get_header())
        f.write(f"""
/**
 * GLEIF LEI Artefact Population Script - {dataset_name}
 *
 * Auto-generated from {csv_file.name}
 * This script is idempotent.
 *
 * Row count: {len(rows)}
 */

-- =============================================================================
-- {dataset_name}
-- =============================================================================

\\echo '--- {dataset_name} ---'

DO $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = '{dataset_code}'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: {dataset_code}';
    end if;

    -- Clear existing artefacts for this dataset (idempotency)
    delete from ores_{table_name}
    where dataset_id = v_dataset_id;

    raise notice 'Populating artefacts for dataset: {dataset_name}';

    -- Insert artefact data
    insert into ores_{table_name} (
        dataset_id, tenant_id, {sql_columns[0]}, version,
        {', '.join(sql_columns[1:])}
    )
    select
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        v.{sql_columns[0]},
        0,
        {', '.join(f'v.{c}' for c in sql_columns[1:])}
    from (values
""")

        # Write VALUES rows
        for i, row_values in enumerate(rows):
            separator = ',' if i < len(rows) - 1 else ''
            values_str = ', '.join(row_values)
            f.write(f"        ({values_str}){separator}\n")

        f.write(f"""    ) as v({', '.join(sql_columns)});

    get diagnostics v_count = row_count;

    raise notice 'Successfully populated % artefacts for dataset: {dataset_name}', v_count;
end $$;
""")

    print(f"  Generated {len(rows)} rows")


def generate_master_sql(output_file: Path):
    """Generate the lei_populate.sql master include file."""
    print(f"Generating {output_file.name}...")

    with open(output_file, 'w', encoding='utf-8', newline='\n') as f:
        f.write("""/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * GLEIF LEI Reference Data Master Include File
 *
 * Auto-generated by lei_generate_metadata_sql.py
 * Includes all GLEIF LEI SQL files in the correct dependency order.
 */

-- =============================================================================
-- GLEIF LEI Catalog (must come first)
-- =============================================================================

\\echo '--- GLEIF LEI Catalog ---'
\\ir lei_catalog_populate.sql

-- =============================================================================
-- GLEIF LEI Methodology
-- =============================================================================

\\echo '--- GLEIF LEI Methodology ---'
\\ir lei_methodology_populate.sql

-- =============================================================================
-- GLEIF LEI Datasets
-- =============================================================================

\\echo '--- GLEIF LEI Datasets ---'
\\ir lei_dataset_populate.sql

-- =============================================================================
-- GLEIF LEI Dataset Dependencies
-- =============================================================================

\\echo '--- GLEIF LEI Dataset Dependencies ---'
\\ir lei_dataset_dependency_populate.sql

-- =============================================================================
-- GLEIF LEI Entity Artefacts (Small)
-- =============================================================================

\\echo '--- GLEIF LEI Entity Artefacts (Small) ---'
\\ir lei_entities_small_artefact_populate.sql

-- =============================================================================
-- GLEIF LEI Entity Artefacts (Large)
-- =============================================================================

\\echo '--- GLEIF LEI Entity Artefacts (Large) ---'
\\ir lei_entities_large_artefact_populate.sql

-- =============================================================================
-- GLEIF LEI Relationship Artefacts (Small)
-- =============================================================================

\\echo '--- GLEIF LEI Relationship Artefacts (Small) ---'
\\ir lei_relationships_small_artefact_populate.sql

-- =============================================================================
-- GLEIF LEI Relationship Artefacts (Large)
-- =============================================================================

\\echo '--- GLEIF LEI Relationship Artefacts (Large) ---'
\\ir lei_relationships_large_artefact_populate.sql
""")


def main():
    parser = argparse.ArgumentParser(
        description='Generate SQL populate scripts for GLEIF LEI metadata.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument('--manifest-dir', '-m',
                        help='Directory containing manifest.json and CSV files')
    parser.add_argument('--output-dir', '-o',
                        help='Output directory for generated SQL files')

    args = parser.parse_args()

    # Determine paths
    repo_root = get_repo_root()

    if args.manifest_dir:
        manifest_dir = Path(args.manifest_dir)
    else:
        manifest_dir = repo_root / 'external' / 'lei'

    if args.output_dir:
        output_dir = Path(args.output_dir)
    else:
        output_dir = repo_root / 'projects' / 'ores.sql' / 'populate' / 'lei'

    # Validate paths
    manifest_file = manifest_dir / 'manifest.json'

    if not manifest_file.exists():
        print(f"Error: Manifest file not found: {manifest_file}", file=sys.stderr)
        sys.exit(1)

    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"GLEIF LEI Metadata SQL Generator")
    print(f"================================")
    print(f"Manifest: {manifest_file}")
    print(f"Output:   {output_dir}")
    print()

    # Load manifest
    with open(manifest_file, 'r', encoding='utf-8') as f:
        manifest = json.load(f)

    # Find CSV files
    csv_files = find_csv_files(manifest_dir)
    required_files = ['lei2_small', 'lei2_large', 'rr_small', 'rr_large']
    missing = [k for k in required_files if k not in csv_files]
    if missing:
        print(f"Error: Missing CSV subset files: {missing}", file=sys.stderr)
        print(f"  Expected in: {manifest_dir}", file=sys.stderr)
        print(f"  Run generate_lei_subsets.sh first.", file=sys.stderr)
        sys.exit(1)

    for key, path in csv_files.items():
        print(f"  {key}: {path.name}")
    print()

    # Parse as_of_date from first CSV filename
    as_of_date = parse_as_of_date(csv_files['lei2_small'].name)
    print(f"As-of date: {as_of_date}")
    print()

    # Generate metadata files
    generate_catalog_sql(manifest, output_dir / 'lei_catalog_populate.sql')
    generate_methodology_sql(manifest, output_dir / 'lei_methodology_populate.sql')
    generate_dataset_sql(manifest, as_of_date, output_dir / 'lei_dataset_populate.sql')
    generate_dataset_dependency_sql(manifest, output_dir / 'lei_dataset_dependency_populate.sql')

    # Generate artefact populate files
    datasets = manifest.get('datasets', [])
    dataset_map = {d['code']: d for d in datasets}

    generate_artefact_populate_sql(
        csv_file=csv_files['lei2_small'],
        dataset_name='GLEIF LEI Entities (Small)',
        dataset_code='gleif.lei_entities.small',
        table_name='dq_lei_entities_artefact_tbl',
        column_map=LEI2_COLUMN_MAP,
        timestamp_cols=LEI2_TIMESTAMP_COLS,
        output_file=output_dir / 'lei_entities_small_artefact_populate.sql',
    )

    generate_artefact_populate_sql(
        csv_file=csv_files['lei2_large'],
        dataset_name='GLEIF LEI Entities (Large)',
        dataset_code='gleif.lei_entities.large',
        table_name='dq_lei_entities_artefact_tbl',
        column_map=LEI2_COLUMN_MAP,
        timestamp_cols=LEI2_TIMESTAMP_COLS,
        output_file=output_dir / 'lei_entities_large_artefact_populate.sql',
    )

    generate_artefact_populate_sql(
        csv_file=csv_files['rr_small'],
        dataset_name='GLEIF LEI Relationships (Small)',
        dataset_code='gleif.lei_relationships.small',
        table_name='dq_lei_relationships_artefact_tbl',
        column_map=RR_COLUMN_MAP,
        timestamp_cols=RR_TIMESTAMP_COLS,
        output_file=output_dir / 'lei_relationships_small_artefact_populate.sql',
    )

    generate_artefact_populate_sql(
        csv_file=csv_files['rr_large'],
        dataset_name='GLEIF LEI Relationships (Large)',
        dataset_code='gleif.lei_relationships.large',
        table_name='dq_lei_relationships_artefact_tbl',
        column_map=RR_COLUMN_MAP,
        timestamp_cols=RR_TIMESTAMP_COLS,
        output_file=output_dir / 'lei_relationships_large_artefact_populate.sql',
    )

    # Generate master include
    generate_master_sql(output_dir / 'lei_populate.sql')

    print()
    print("Generation complete!")


if __name__ == '__main__':
    main()
