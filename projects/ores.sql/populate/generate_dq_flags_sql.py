#!/usr/bin/env python3
"""
Generates the SQL populate script for DQ flag images.
Reads SVG files from projects/ores.sql/populate/data/flags/
and generates projects/ores.sql/populate/dq_images_artefact_populate.sql.
"""

import os
import glob

# Configuration
SOURCE_DIR = 'projects/ores.sql/populate/data/flags'
OUTPUT_FILE = 'projects/ores.sql/populate/dq_images_artefact_populate.sql'
DATASET_NAME = 'Country Flags from lipis/flag-icons'

def get_header():
    return """/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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

-- Script to populate DQ SVG flag images into the database
-- This file was auto-generated from the SVG files in projects/ores.sql/populate/data/flags/
-- by projects/ores.sql/populate/generate_dq_flags_sql.py

set schema 'ores';

DO $$
declare
    v_dataset_id uuid;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_dataset_tbl
    where name = '{}'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset "{}" not found.';
    end if;

    -- Clear existing data for this dataset (Idempotency)
    delete from ores.dq_images_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert images
""".format(DATASET_NAME, DATASET_NAME)

def get_footer():
    return """
    raise notice 'Populated dq_images_artefact_tbl for dataset: %', '{}';
end $$;
""".format(DATASET_NAME)

def generate_insert(key, svg_content):
    description = f"Flag of {key}"
    # Use dollar quoting for SVG content to avoid escaping issues
    return f"""    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, '{key}', '{description}', $svg${svg_content}$svg$
    );
"""

def main():
    if not os.path.exists(SOURCE_DIR):
        print(f"Error: Source directory {SOURCE_DIR} does not exist.")
        return

    svg_files = sorted(glob.glob(os.path.join(SOURCE_DIR, '*.svg')))
    
    if not svg_files:
        print(f"Error: No SVG files found in {SOURCE_DIR}.")
        return

    print(f"Found {len(svg_files)} SVG files.")

    with open(OUTPUT_FILE, 'w') as f:
        f.write(get_header())
        
        for file_path in svg_files:
            filename = os.path.basename(file_path)
            key = os.path.splitext(filename)[0]
            with open(file_path, 'r') as svg_file:
                svg_content = svg_file.read().strip()
                f.write(generate_insert(key, svg_content))
        
        f.write(get_footer())

    print(f"Successfully generated {OUTPUT_FILE}")

if __name__ == '__main__':
    main()
