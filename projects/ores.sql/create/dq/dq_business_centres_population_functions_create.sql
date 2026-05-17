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
 * FPML business_centres Population Functions
 *
 * Functions to publish business_centres from DQ artefact table to production.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores_dq_business_centre_preview_fn(dataset_id);
 *
 *   -- Publish to production
 *   SELECT * FROM ores_dq_business_centres_publish_fn(dataset_id, 'upsert');
 */

-- =============================================================================
-- Preview Function
-- =============================================================================

/**
 * Preview what business_centres would be copied from a DQ dataset.
 */
create or replace function ores_dq_business_centre_preview_fn(p_dataset_id uuid)
returns table (
    action text,
    code text,
    coding_scheme_code text,
    description text,
    has_image boolean,
    derived_country_alpha2 text,
    reason text
) as $$
begin
    return query
    select
        case
            when existing.code is not null then 'update'
            else 'insert'
        end as action,
        dq.code,
        dq.coding_scheme_code,
        dq.description,
        dq.image_id is not null as has_image,
        left(dq.code, 2) as derived_country_alpha2,
        case
            when existing.code is not null then 'Record already exists'
            else 'New record'
        end as reason
    from ores_dq_business_centres_artefact_tbl dq
    left join ores_refdata_business_centres_tbl existing
        on existing.code = dq.code
        and existing.coding_scheme_code = dq.coding_scheme_code
        and existing.valid_to = ores_utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.coding_scheme_code, dq.code;
end;
$$ language plpgsql;

