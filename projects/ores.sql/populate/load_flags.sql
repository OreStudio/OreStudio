-- -*- mode: sql; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
--
-- Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
--
-- This program is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.
--
-- Script to load SVG flag images into the database
--
-- Prerequisites:
--   - The images, tags, and image_tags tables must exist
--   - This script must be run from the project root directory
--
-- Usage:
--   psql -h localhost -U oresadmin -d ores -f projects/ores.sql/load_flags.sql
--

-- Create the 'flag' tag if it doesn't exist
-- This tag is used to categorize flag images loaded by flags_populate.sql
INSERT INTO ores.tags (tag_id, version, name, description, modified_by,
    change_reason_code, change_commentary, valid_from, valid_to)
SELECT
    gen_random_uuid(),
    0,
    'flag',
    'Country and region flag images',
    'system',
    'system.new_record',
    'System seed data',
    CURRENT_TIMESTAMP,
    ores.infinity_timestamp()
WHERE NOT EXISTS (
    SELECT 1 FROM ores.tags
    WHERE name = 'flag' AND valid_to = ores.infinity_timestamp()
);

-- Note: The load_flag() function is defined in schema/images_functions_create.sql
-- and is available as part of the database schema.

-- Note: The actual flag loading requires reading SVG files from the filesystem.
-- This can be done using psql's \lo_import or by generating INSERT statements
-- from the flag files using a script.
--
-- Example usage of the load_flag function:
--
-- SELECT ores.load_flag(
--     'ro',
--     'Flag for country code RO (Romania)',
--     '<svg xmlns="http://www.w3.org/2000/svg">...</svg>'
-- );
--
-- To generate INSERT statements from the flag files, you can use:
--
-- for f in projects/ores.sql/populate/data/flags/*.svg; do
--     key=$(basename "$f" .svg)
--     desc="Flag for country code ${key^^}"
--     svg=$(cat "$f" | sed "s/'/''/g")
--     echo "SELECT ores.load_flag('$key', '$desc', '$svg');"
-- done > projects/ores.sql/load_flags_data.sql
-- echo "SELECT 'Loaded ' || COUNT(*) || ' flags' AS summary FROM ores.image_tags it JOIN ores.tags t ON it.tag_id = t.tag_id WHERE t.name = 'flag' AND t.valid_to = ores.infinity_timestamp() AND it.valid_to = ores.infinity_timestamp();" >> projects/ores.sql/load_flags_data.sql
--
-- Then run: psql -h localhost -U oresadmin -d ores -f projects/ores.sql/load_flags_data.sql
