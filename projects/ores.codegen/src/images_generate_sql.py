#!/usr/bin/env python3
"""
Generates SQL populate scripts for DQ image artefacts.

This is a generalized script that can be used for any image dataset
(flags, crypto icons, system avatars, etc.). It reads image files (SVG,
PNG, JPEG) from a source directory and generates a SQL script that
populates the dq_images_artefact_tbl with each file's MIME type and
base64-encoded data.

Usage:
    python3 images_generate_sql.py --config flags
    python3 images_generate_sql.py --config crypto
    python3 images_generate_sql.py --config system_avatars
    python3 images_generate_sql.py \\
        --dataset-name "My Dataset" \\
        --subject-area "My Subject Area" \\
        --domain "Reference Data" \\
        --source-dir "path/to/images" \\
        --output-file "output.sql" \\
        --description-template "Icon for {key}"
"""

import argparse
import base64
import os
import glob
import sys

# Media types by file extension. The staging table carries the live
# table's own (mime_type, data) shape, so all of these travel one pipeline.
MIME_TYPES = {
    '.svg': 'image/svg+xml',
    '.png': 'image/png',
    '.jpg': 'image/jpeg',
    '.jpeg': 'image/jpeg',
}

# Predefined configurations for common datasets
CONFIGS = {
    'flags': {
        'dataset_name': 'Country Flag Images',
        'subject_area_name': 'Country Flags',
        'domain_name': 'Reference Data',
        'source_dir': 'external/flags/flag-icons',
        'output_file': 'projects/ores.sql/populate/flags/flags_images_artefact_populate.sql',
        'description_template': 'Flag of {key}',
    },
    'crypto': {
        'dataset_name': 'Cryptocurrency Icon Images',
        'subject_area_name': 'Cryptocurrencies',
        'domain_name': 'Reference Data',
        'source_dir': 'external/crypto/cryptocurrency-icons',
        'output_file': 'projects/ores.sql/populate/crypto/crypto_images_artefact_populate.sql',
        'description_template': 'Icon for {key}',
    },
    'system_avatars': {
        'dataset_name': 'System Avatar Images',
        'subject_area_name': 'System Avatars',
        'domain_name': 'Reference Data',
        'source_dir': 'external/avatars',
        'output_file': 'projects/ores.sql/populate/assets/system_avatars_images_artefact_populate.sql',
        'description_template': 'Avatar of {key}',
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

-- Script to populate DQ images into the database
-- Dataset: {dataset_name}
-- Subject Area: {subject_area_name}
-- Domain: {domain_name}
--
-- This file was auto-generated from the image files in {source_dir}
-- by {script_name}
--
-- To regenerate, run:
--   python3 {script_name} --config <config_name>
-- or with explicit parameters:
--   python3 {script_name} --dataset-name "..." --subject-area "..." --domain "..." ...


DO $$
declare
    v_dataset_id uuid;
begin
    -- Get the dataset ID using (name, subject_area_name, domain_name)
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where name = '{dataset_name}'
      and subject_area_name = '{subject_area_name}'
      and domain_name = '{domain_name}'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: name="{dataset_name}", subject_area="{subject_area_name}", domain="{domain_name}"';
    end if;

    -- Clear existing images for this dataset (idempotency)
    delete from ores_dq_images_artefact_tbl
    where dataset_id = v_dataset_id;

    raise debug 'Populating images for dataset: %', '{dataset_name}';

    -- Insert images
"""


def get_footer(dataset_name: str, count: int) -> str:
    return f"""
    raise debug 'Successfully populated % images for dataset: %', {count}, '{dataset_name}';
end $$;
"""


def generate_insert(key: str, description: str, mime_type: str,
                    data_base64: str) -> str:
    # Escape single quotes in description
    safe_description = description.replace("'", "''")
    # Dollar-quote the base64 payload; its alphabet holds no dollar sign
    return f"""    insert into ores_dq_images_artefact_tbl (
        dataset_id, tenant_id, image_id, version, key, description, mime_type, data
    ) values (
        v_dataset_id, ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0, '{key}', '{safe_description}', '{mime_type}', $b64${data_base64}$b64$
    );
"""


def main():
    parser = argparse.ArgumentParser(
        description='Generate SQL populate script for DQ image artefacts.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Predefined configurations:
  flags          - Country flags from lipis/flag-icons
  crypto         - Cryptocurrency icons
  system_avatars - Default system avatars (PNG)

Examples:
  %(prog)s --config flags
  %(prog)s --config system_avatars
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
                        help='Directory containing image files')
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

    # Find image files
    image_files = sorted(
        path for path in glob.glob(os.path.join(source_dir, '*'))
        if os.path.splitext(path)[1].lower() in MIME_TYPES
    )

    if not image_files:
        print(f"Error: No image files found in '{source_dir}'.", file=sys.stderr)
        sys.exit(1)

    print(f"Configuration:")
    print(f"  Dataset:      {dataset_name}")
    print(f"  Subject Area: {subject_area_name}")
    print(f"  Domain:       {domain_name}")
    print(f"  Source:       {source_dir}")
    print(f"  Output:       {output_file}")
    print(f"  Found {len(image_files)} image files.")
    print()

    # Generate SQL
    script_name = os.path.basename(__file__)
    with open(output_file, 'w') as f:
        f.write(get_header(dataset_name, subject_area_name, domain_name,
                           source_dir, script_name))

        for file_path in image_files:
            filename = os.path.basename(file_path)
            key, extension = os.path.splitext(filename)
            description = description_template.format(key=key)

            with open(file_path, 'rb') as image_file:
                data_base64 = base64.b64encode(image_file.read()).decode('ascii')

            f.write(generate_insert(key, description,
                                    MIME_TYPES[extension.lower()], data_base64))

        f.write(get_footer(dataset_name, len(image_files)))

    print(f"Successfully generated {output_file}")
    print(f"  Total images: {len(image_files)}")


if __name__ == '__main__':
    main()
