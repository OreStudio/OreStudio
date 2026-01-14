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
set search_path to ores;

create or replace function ores.assets_load_flag_fn(
    p_key text,
    p_description text,
    p_svg_data text
) returns void as $$
declare
    v_image_id uuid;
begin
    select image_id into v_image_id
    from ores.assets_images_tbl
    where key = p_key and valid_to = ores.utility_infinity_timestamp_fn();

    if v_image_id is null then
        v_image_id := gen_random_uuid();
    end if;

    insert into ores.assets_images_tbl (
        image_id, version, key, description, svg_data,
        modified_by, change_reason_code, change_commentary,
        valid_from, valid_to
    ) values (
        v_image_id, 0, p_key, p_description, p_svg_data,
        'system', 'system.new_record', 'System seed data',
        current_timestamp, ores.utility_infinity_timestamp_fn()
    );

    insert into ores.assets_image_tags_tbl (
        image_id, tag_id, assigned_by, assigned_at, valid_from, valid_to
    )
    select
        v_image_id, tag_id, 'system', current_timestamp,
        current_timestamp, ores.utility_infinity_timestamp_fn()
    from ores.assets_tags_tbl
    where name = 'flag' and valid_to = ores.utility_infinity_timestamp_fn()
      and not exists (
          select 1 from ores.assets_image_tags_tbl it
          where it.image_id = v_image_id
            and it.tag_id = assets_tags_tbl.tag_id
            and it.valid_to = ores.utility_infinity_timestamp_fn()
      );

    return;
end;
$$ language plpgsql;
