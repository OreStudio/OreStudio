#!/usr/bin/env python3
"""
Generates SQL populate scripts for DQ image artefacts.

This is a generalized script that can be used for any image dataset
(flags, crypto icons, etc.). It reads SVG files from a source directory
and generates a SQL script to populate the dq_images_artefact_tbl.

Usage:
    python3 generate_dq_images_sql.py --config flags
    python3 generate_dq_images_sql.py --config crypto
    python3 generate_dq_images_sql.py \\
        --dataset-name "My Dataset" \\
        --subject-area "My Subject Area" \\
        --domain "Reference Data" \\
        --source-dir "path/to/svgs" \\
        --output-file "output.sql" \\
        --description-template "Icon for {key}"
"""

import argparse
import os
import glob
import sys

# Predefined configurations for common datasets
CONFIGS = {
    'flags': {
        'dataset_name': 'Country Flag Images',
        'subject_area_name': 'Countries',
        'domain_name': 'Reference Data',
        'source_dir': 'projects/ores.sql/populate/data/flags',
        'output_file': 'projects/ores.sql/populate/dq_flags_images_artefact_populate.sql',
        'description_template': 'Flag of {key}',
    },
    'crypto': {
        'dataset_name': 'Cryptocurrency Icon Images',
        'subject_area_name': 'Cryptocurrencies',
        'domain_name': 'Reference Data',
        'source_dir': 'projects/ores.sql/populate/data/cryptocurrency-icons',
        'output_file': 'projects/ores.sql/populate/dq_crypto_images_artefact_populate.sql',
        'description_template': 'Icon for {key}',
    },
}


def get_header(dataset_name: str, subject_area_name: str, domain_name: str,
               source_dir: str, script_name: str) -> str:
    return f"""/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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

-- Script to populate DQ SVG images into the database
-- Dataset: {dataset_name}
-- Subject Area: {subject_area_name}
-- Domain: {domain_name}
--
-- This file was auto-generated from the SVG files in {source_dir}
-- by {script_name}
--
-- To regenerate, run:
--   python3 {script_name} --config <config_name>
-- or with explicit parameters:
--   python3 {script_name} --dataset-name "..." --subject-area "..." --domain "..." ...

set schema 'ores';

DO $$
declare
    v_dataset_id uuid;
begin
    -- Get the dataset ID using (name, subject_area_name, domain_name)
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where name = '{dataset_name}'
      and subject_area_name = '{subject_area_name}'
      and domain_name = '{domain_name}'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: name="{dataset_name}", subject_area="{subject_area_name}", domain="{domain_name}"';
    end if;

    -- Clear existing images for this dataset (idempotency)
    delete from ores.dq_images_artefact_tbl
    where dataset_id = v_dataset_id;

    raise notice 'Populating images for dataset: %', '{dataset_name}';

    -- Insert images
"""


def get_footer(dataset_name: str, count: int) -> str:
    return f"""
    raise notice 'Successfully populated % images for dataset: %', {count}, '{dataset_name}';
end $$;
"""


def generate_insert(key: str, description: str, svg_content: str) -> str:
    # Escape single quotes in description
    safe_description = description.replace("'", "''")
    # Use dollar quoting for SVG content to avoid escaping issues
    return f"""    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, '{key}', '{safe_description}', $svg${svg_content}$svg$
    );
"""


def main():
    parser = argparse.ArgumentParser(
        description='Generate SQL populate script for DQ image artefacts.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Predefined configurations:
  flags   - Country flags from lipis/flag-icons
  crypto  - Cryptocurrency icons

Examples:
  %(prog)s --config flags
  %(prog)s --config crypto
  %(prog)s --dataset-name "My Icons" --subject-area "Icons" --domain "Reference Data" \\
           --source-dir "./icons" --output-file "icons.sql"
        """
    )

    parser.add_argument('--config', '-c', choices=CONFIGS.keys(),
                        help='Use a predefined configuration')
    parser.add_argument('--dataset-name', '-n',
                        help='Name of the dataset in dq_datasets_tbl')
    parser.add_argument('--subject-area', '-s',
                        help='Subject area name')
    parser.add_argument('--domain', '-d',
                        help='Domain name')
    parser.add_argument('--source-dir', '-i',
                        help='Directory containing SVG files')
    parser.add_argument('--output-file', '-o',
                        help='Output SQL file path')
    parser.add_argument('--description-template', '-t',
                        default='Image for {key}',
                        help='Template for image descriptions (use {key} as placeholder)')

    args = parser.parse_args()

    # Determine configuration
    if args.config:
        config = CONFIGS[args.config]
        dataset_name = args.dataset_name or config['dataset_name']
        subject_area_name = args.subject_area or config['subject_area_name']
        domain_name = args.domain or config['domain_name']
        source_dir = args.source_dir or config['source_dir']
        output_file = args.output_file or config['output_file']
        description_template = args.description_template if args.description_template != 'Image for {key}' else config['description_template']
    else:
        # All parameters must be provided
        if not all([args.dataset_name, args.subject_area, args.domain,
                    args.source_dir, args.output_file]):
            parser.error('Either --config or all of --dataset-name, --subject-area, '
                         '--domain, --source-dir, and --output-file must be provided')
        dataset_name = args.dataset_name
        subject_area_name = args.subject_area
        domain_name = args.domain
        source_dir = args.source_dir
        output_file = args.output_file
        description_template = args.description_template

    # Validate source directory
    if not os.path.exists(source_dir):
        print(f"Error: Source directory '{source_dir}' does not exist.", file=sys.stderr)
        sys.exit(1)

    # Find SVG files
    svg_files = sorted(glob.glob(os.path.join(source_dir, '*.svg')))

    if not svg_files:
        print(f"Error: No SVG files found in '{source_dir}'.", file=sys.stderr)
        sys.exit(1)

    print(f"Configuration:")
    print(f"  Dataset:      {dataset_name}")
    print(f"  Subject Area: {subject_area_name}")
    print(f"  Domain:       {domain_name}")
    print(f"  Source:       {source_dir}")
    print(f"  Output:       {output_file}")
    print(f"  Found {len(svg_files)} SVG files.")
    print()

    # Generate SQL
    script_name = os.path.basename(__file__)
    with open(output_file, 'w') as f:
        f.write(get_header(dataset_name, subject_area_name, domain_name,
                           source_dir, script_name))

        for file_path in svg_files:
            filename = os.path.basename(file_path)
            key = os.path.splitext(filename)[0]
            description = description_template.format(key=key)

            with open(file_path, 'r') as svg_file:
                svg_content = svg_file.read().strip()

            f.write(generate_insert(key, description, svg_content))

        f.write(get_footer(dataset_name, len(svg_files)))

    print(f"Successfully generated {output_file}")
    print(f"  Total images: {len(svg_files)}")


if __name__ == '__main__':
    main()
