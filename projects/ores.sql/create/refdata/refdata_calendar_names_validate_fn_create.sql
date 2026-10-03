/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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

/**
 * Calendar expression validation
 *
 * A curve names its calendar as ORE does: one name, or several joined, as in
 * TARGET,UK or JoinHolidays(TARGET, US settlement). This function splits the
 * expression and checks each calendar in it against the names ORE's schema
 * lists, held in calendar_names, or against one of the two open forms the
 * schema allows: a four-letter exchange code, and a name beginning CUSTOM_.
 *
 * An empty calendar is accepted, because 110 corpus entries write one and the
 * export must write it back.
 */

create or replace function ores_refdata_validate_calendar_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
declare
    v_body text;
    v_part text;
begin
    if p_value is null or p_value = '' then
        return p_value;
    end if;

    v_body := regexp_replace(p_value, '^(JoinHolidays|JoinBusinessDays)\(', '');
    if v_body <> p_value then
        v_body := regexp_replace(v_body, '\)$', '');
    end if;

    foreach v_part in array regexp_split_to_array(v_body, '\s*,\s*') loop
        v_part := btrim(v_part);
        if v_part ~ '^[A-Z]{4}$' or v_part ~ '^CUSTOM_' then
            continue;
        end if;
        if not exists (
            select 1 from ores_refdata_calendar_names_tbl
            where tenant_id = p_tenant_id
              and code = v_part
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid calendar: %. % is not a calendar ORE accepts.', p_value, v_part
                using errcode = '23503';
        end if;
    end loop;

    return p_value;
end;
$$ language plpgsql;
