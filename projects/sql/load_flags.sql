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
--   psql -h localhost -U oresadmin -d oresdb -f projects/sql/load_flags.sql
--

-- Create the 'flag' tag if it doesn't exist
INSERT INTO oresdb.tags (tag_id, version, name, description, modified_by, valid_from, valid_to)
SELECT
    gen_random_uuid(),
    0,
    'flag',
    'Country and region flag images',
    'system',
    CURRENT_TIMESTAMP,
    oresdb.infinity_timestamp()
WHERE NOT EXISTS (
    SELECT 1 FROM oresdb.tags
    WHERE name = 'flag' AND valid_to = oresdb.infinity_timestamp()
);

-- Function to load a single flag SVG file
-- This function reads an SVG file and inserts it into the images table
-- Returns void to suppress per-call output when loading many flags
-- The function is idempotent: if the image already exists, it creates a new version
CREATE OR REPLACE FUNCTION oresdb.load_flag(
    p_key text,
    p_description text,
    p_svg_data text
) RETURNS void AS $$
DECLARE
    v_image_id uuid;
BEGIN
    -- Check if image with this key already exists and get its ID
    SELECT image_id INTO v_image_id
    FROM oresdb.images
    WHERE key = p_key AND valid_to = oresdb.infinity_timestamp();

    -- If it's a new image, generate a new UUID
    IF v_image_id IS NULL THEN
        v_image_id := gen_random_uuid();
    END IF;

    -- Insert the image. The 'update_images' trigger handles versioning.
    INSERT INTO oresdb.images (
        image_id, version, key, description, svg_data,
        modified_by, valid_from, valid_to
    ) VALUES (
        v_image_id, 0, p_key, p_description, p_svg_data,
        'system', CURRENT_TIMESTAMP, oresdb.infinity_timestamp()
    );

    -- Link image to flag tag (skip if already linked)
    INSERT INTO oresdb.image_tags (
        image_id, tag_id, assigned_by, assigned_at, valid_from, valid_to
    )
    SELECT
        v_image_id, tag_id, 'system', CURRENT_TIMESTAMP,
        CURRENT_TIMESTAMP, oresdb.infinity_timestamp()
    FROM oresdb.tags
    WHERE name = 'flag' AND valid_to = oresdb.infinity_timestamp()
      AND NOT EXISTS (
          SELECT 1 FROM oresdb.image_tags it
          WHERE it.image_id = v_image_id
            AND it.tag_id = tags.tag_id
            AND it.valid_to = oresdb.infinity_timestamp()
      );

    RETURN;
END;
$$ LANGUAGE plpgsql;

-- Note: The actual flag loading requires reading SVG files from the filesystem.
-- This can be done using psql's \lo_import or by generating INSERT statements
-- from the flag files using a script.
--
-- Example usage of the load_flag function:
--
-- SELECT oresdb.load_flag(
--     'ro',
--     'Flag for country code RO (Romania)',
--     '<svg xmlns="http://www.w3.org/2000/svg">...</svg>'
-- );
--
-- To generate INSERT statements from the flag files, you can use:
--
-- for f in projects/sql/data/flags/*.svg; do
--     key=$(basename "$f" .svg)
--     desc="Flag for country code ${key^^}"
--     svg=$(cat "$f" | sed "s/'/''/g")
--     echo "SELECT oresdb.load_flag('$key', '$desc', '$svg');"
-- done > projects/sql/load_flags_data.sql
-- echo "SELECT 'Loaded ' || COUNT(*) || ' flags' AS summary FROM oresdb.image_tags it JOIN oresdb.tags t ON it.tag_id = t.tag_id WHERE t.name = 'flag' AND t.valid_to = oresdb.infinity_timestamp() AND it.valid_to = oresdb.infinity_timestamp();" >> projects/sql/load_flags_data.sql
--
-- Then run: psql -h localhost -U oresadmin -d oresdb -f projects/sql/load_flags_data.sql
