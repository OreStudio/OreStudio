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

/**
 * DQ IP to Country Artefact Population Script
 *
 * Imports IPv4 to country mapping data from iptoasn.com TSV file
 * into the DQ staging table.
 *
 * Source: https://iptoasn.com/
 * License: PDDL v1.0 (Public Domain)
 * Format: TSV with columns (range_start, range_end, country_code)
 *
 * Usage:
 *   psql -d your_database -f dq_ip2country_artefact_populate.sql
 */

set schema 'metadata';

\echo '--- IP to Country Artefact Population ---'

DO $$
declare
    v_dataset_id uuid;
    v_data_file text := '../../external/ip2country/ip2country-v4-u32.tsv';
begin
    -- Get dataset ID
    select id into v_dataset_id
    from metadata.dq_datasets_tbl
    where name = 'IP to Country IPv4 Ranges'
      and subject_area_name = 'IP Address to Country maps'
      and domain_name = 'Reference Data'
      and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: IP to Country IPv4 Ranges';
    end if;

    raise notice 'Dataset ID: %', v_dataset_id;

    -- Clear existing artefact data for this dataset
    delete from metadata.dq_ip2country_artefact_tbl where dataset_id = v_dataset_id;
    raise notice 'Cleared existing artefact data';

    -- Create temporary staging table for TSV import
    create temp table staging_ip2country (
        range_start bigint,
        range_end bigint,
        country_code text
    ) on commit drop;

    -- Note: The actual COPY command needs to run outside the DO block
    -- We'll use a workaround with dynamic execution

    -- Store dataset_id for use in the main script
    perform set_config('app.ip2country_dataset_id', v_dataset_id::text, false);
end $$;

-- Import TSV file using COPY (must be outside DO block)
\echo 'Importing TSV data...'

create temp table staging_ip2country_import (
    range_start bigint,
    range_end bigint,
    country_code text
);

\copy staging_ip2country_import from '../../external/ip2country/ip2country-v4-u32.tsv' with (format text, delimiter E'\t')

-- Insert into artefact table with dataset_id
insert into metadata.dq_ip2country_artefact_tbl (dataset_id, range_start, range_end, country_code)
select
    (current_setting('app.ip2country_dataset_id'))::uuid,
    range_start,
    range_end,
    country_code
from staging_ip2country_import;

drop table staging_ip2country_import;

\echo 'Import complete.'

-- Summary
\echo ''
\echo '--- Summary ---'

select
    count(*) as total_ranges,
    count(distinct country_code) as unique_countries,
    count(*) filter (where country_code = 'None') as unrouted_ranges
from metadata.dq_ip2country_artefact_tbl
where dataset_id = (current_setting('app.ip2country_dataset_id'))::uuid;
