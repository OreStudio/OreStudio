/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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

--
-- Functions for working with images and flags
--

SET search_path TO ores;

-- Function to load a single flag SVG into the images table.
-- This function is idempotent: if the image already exists, it creates a new version.
-- Returns void to suppress per-call output when loading many flags.
CREATE OR REPLACE FUNCTION ores.load_flag(
    p_key text,
    p_description text,
    p_svg_data text
) RETURNS void AS $$
DECLARE
    v_image_id uuid;
BEGIN
    -- Check if image with this key already exists and get its ID
    SELECT image_id INTO v_image_id
    FROM ores.images
    WHERE key = p_key AND valid_to = ores.infinity_timestamp();

    -- If it's a new image, generate a new UUID
    IF v_image_id IS NULL THEN
        v_image_id := gen_random_uuid();
    END IF;

    -- Insert the image. The 'update_images' trigger handles versioning.
    INSERT INTO ores.images (
        image_id, version, key, description, svg_data,
        modified_by, valid_from, valid_to
    ) VALUES (
        v_image_id, 0, p_key, p_description, p_svg_data,
        'system', CURRENT_TIMESTAMP, ores.infinity_timestamp()
    );

    -- Link image to flag tag (skip if already linked)
    INSERT INTO ores.image_tags (
        image_id, tag_id, assigned_by, assigned_at, valid_from, valid_to
    )
    SELECT
        v_image_id, tag_id, 'system', CURRENT_TIMESTAMP,
        CURRENT_TIMESTAMP, ores.infinity_timestamp()
    FROM ores.tags
    WHERE name = 'flag' AND valid_to = ores.infinity_timestamp()
      AND NOT EXISTS (
          SELECT 1 FROM ores.image_tags it
          WHERE it.image_id = v_image_id
            AND it.tag_id = tags.tag_id
            AND it.valid_to = ores.infinity_timestamp()
      );

    RETURN;
END;
$$ LANGUAGE plpgsql;
