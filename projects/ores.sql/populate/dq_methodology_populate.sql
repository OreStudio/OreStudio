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

set schema 'ores';

-- =============================================================================
-- Helper Function
-- =============================================================================

create or replace function ores.upsert_dq_methodology(
    p_name text,
    p_description text,
    p_logic_reference text default null,
    p_implementation_details text default null
) returns void as $$
declare
    v_id uuid;
begin
    if not exists (
        select 1 from ores.dq_methodology_tbl
        where name = p_name
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_methodology_tbl (
            id, version, name, description, logic_reference, implementation_details,
            modified_by, change_reason_code, change_commentary,
            valid_from, valid_to
        )
        values (
            gen_random_uuid(), 0, p_name, p_description, p_logic_reference, p_implementation_details,
            'system', 'system.new_record', 'System seed data',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created dq_methodology: %', p_name;
    else
        raise notice 'dq_methodology already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Seed Data
-- =============================================================================

select ores.upsert_dq_methodology(
    'Wikipedia ISO 3166 Extraction',
    'Data extracted from Wikipedia page listing ISO 3166 country codes',
    'https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes',
    'Manual extraction of ISO 3166-1 alpha-2, alpha-3, and numeric codes with country names'
);

select ores.upsert_dq_methodology(
    'GitHub Flag Icons Download',
    'SVG images downloaded from lipis/flag-icons GitHub repository',
    'https://github.com/lipis/flag-icons',
    'Bulk download of SVG flag icons'
);

select ores.upsert_dq_methodology(
    'GitHub Cryptocurrency Icons Download',
    'SVG images downloaded from spothq/cryptocurrency-icons GitHub repository',
    'https://github.com/spothq/cryptocurrency-icons',
    'Bulk download of SVG cryptocurrency icons at commit 1a63530be6e374711a8554f31b17e4cb92c25fa5'
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_methodology(text, text, text, text);

-- =============================================================================
-- Summary
-- =============================================================================

select 'dq_methodology' as entity, count(*) as count
from ores.dq_methodology_tbl
where valid_to = ores.utility_infinity_timestamp_fn();
