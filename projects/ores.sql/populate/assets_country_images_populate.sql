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
-- Populates the image_id column on countries with references to their flag images.
-- Each country is mapped to its flag using the lowercase alpha2_code as the image key.
--
-- Prerequisites:
--   - flags_populate.sql must be run first (to populate images table)
--   - countries_populate.sql must be run first (to populate countries table)
--

SET search_path TO ores;

--
-- Update countries to link to their flag images
-- The flag key in the images table matches the lowercase alpha2_code
--
UPDATE refdata_countries_tbl c
SET image_id = i.image_id
FROM assets_images_tbl i
WHERE i.key = lower(c.alpha2_code)
  AND i.valid_to = ores.utility_infinity_timestamp_fn()
  AND c.valid_to = ores.utility_infinity_timestamp_fn();

--
-- Assign placeholder "no-flag" image to countries without a specific flag
--
UPDATE refdata_countries_tbl c
SET image_id = i.image_id
FROM assets_images_tbl i
WHERE i.key = 'no-flag'
  AND i.valid_to = ores.utility_infinity_timestamp_fn()
  AND c.valid_to = ores.utility_infinity_timestamp_fn()
  AND c.image_id IS NULL;
