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

create or replace function ores.upsert_dq_tags_artefact(
    p_dataset_name text
) returns void as $$
declare
    v_dataset_id uuid;
    v_count integer;
begin
    -- 1. Get the dataset ID
    select id into v_dataset_id
    from ores.dq_dataset_tbl
    where name = p_dataset_name
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset "%" not found.', p_dataset_name;
    end if;

    -- 2. Clear existing data for this dataset
    delete from ores.dq_tags_artefact_tbl
    where dataset_id = v_dataset_id;

    -- 3. Insert data from assets_tags_tbl
    insert into ores.dq_tags_artefact_tbl (
        dataset_id,
        tag_id,
        version,
        name,
        description
    )
    select
        v_dataset_id,
        tag_id,
        version,
        name,
        description
    from ores.assets_tags_tbl
    where valid_to = ores.utility_infinity_timestamp_fn();

    get diagnostics v_count = row_count;
    raise notice 'Inserted % records into dq_tags_artefact_tbl for dataset %', v_count, p_dataset_name;
end;
$$ language plpgsql;

-- =============================================================================
-- Seed Data
-- =============================================================================

select ores.upsert_dq_tags_artefact('Country Flags from lipis/flag-icons');

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_tags_artefact(text);

-- =============================================================================
-- Summary
-- =============================================================================

select 'dq_tags_artefact' as entity, count(*) as count
from ores.dq_tags_artefact_tbl;
