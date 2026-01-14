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
 * Change Reasons Population Script
 *
 * Seeds the database with change reason categories and change reasons.
 * This script is idempotent.
 *
 * Taxonomy is aligned with regulatory standards:
 * - BCBS 239 (Risk Data Aggregation)
 * - FRTB (Fundamental Review of the Trading Book)
 * - FINRA (Financial Industry Regulatory Authority)
 * - MiFID II (Markets in Financial Instruments Directive)
 *
 * Categories:
 * - system: Auto-assigned reasons (not user-selectable)
 *   - initial_load: System bootstrap or migration
 *   - new_record: Normal operational record creation (human or machine)
 *   - external_data_import: External vendor/file import (requires data lineage)
 * - common: Universal data quality reasons (BCBS 239 / FRTB aligned)
 * - trade: Trade lifecycle reasons (FINRA / MiFID II aligned)
 *
 * Reason code format: "category.reason_name"
 */

set schema 'ores';

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- Helper function to insert a change reason category if it doesn't exist
create or replace function ores.upsert_change_reason_category(
    p_code text,
    p_description text
) returns void as $$
begin
    if not exists (
        select 1 from ores.dq_change_reason_categories_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_change_reason_categories_tbl (
            code, description, modified_by, change_commentary,
            valid_from, valid_to
        )
        values (
            p_code, p_description, 'system',
            'System seed data - standard regulatory taxonomy',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created change reason category: %', p_code;
    else
        raise notice 'Change reason category already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

-- Helper function to insert a change reason if it doesn't exist
create or replace function ores.upsert_change_reason(
    p_code text,
    p_description text,
    p_category_code text,
    p_applies_to_amend boolean,
    p_applies_to_delete boolean,
    p_requires_commentary boolean,
    p_display_order integer
) returns void as $$
begin
    if not exists (
        select 1 from ores.dq_change_reasons_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_change_reasons_tbl (
            code, description, category_code,
            applies_to_amend, applies_to_delete, requires_commentary, display_order,
            modified_by, change_commentary, valid_from, valid_to
        )
        values (
            p_code, p_description, p_category_code,
            p_applies_to_amend, p_applies_to_delete, p_requires_commentary, p_display_order,
            'system', 'System seed data - standard regulatory taxonomy',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created change reason: %', p_code;
    else
        raise notice 'Change reason already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Change Reason Categories
-- =============================================================================

\echo '--- Change Reason Categories ---'

select ores.upsert_change_reason_category(
    'system',
    'System-generated reasons for automatic operations (not user-selectable)'
);

select ores.upsert_change_reason_category(
    'common',
    'Universal data quality reasons aligned with BCBS 239 and FRTB standards'
);

select ores.upsert_change_reason_category(
    'trade',
    'Trade lifecycle reasons aligned with FINRA and MiFID II standards'
);

-- =============================================================================
-- Change Reasons: System Category
-- =============================================================================

\echo ''
\echo '--- Change Reasons: System ---'

-- System reasons (auto-assigned, not user-selectable)
select ores.upsert_change_reason(
    'system.initial_load',
    'Initial system bootstrap or migration',
    'system',
    false,  -- not for amend
    false,  -- not for delete
    false,  -- no commentary required
    0       -- display order
);

select ores.upsert_change_reason(
    'system.new_record',
    'New record created during normal operations',
    'system',
    false,  -- not for amend
    false,  -- not for delete
    false,  -- no commentary required
    10      -- display order
);

select ores.upsert_change_reason(
    'system.external_data_import',
    'External data import (requires data lineage)',
    'system',
    true,   -- applies to amend (imports can update existing records)
    false,  -- not for delete
    true,   -- commentary REQUIRED (data lineage)
    20      -- display order
);

-- =============================================================================
-- Change Reasons: Common Category (BCBS 239 / FRTB aligned)
-- =============================================================================

\echo ''
\echo '--- Change Reasons: Common (BCBS 239 / FRTB) ---'

select ores.upsert_change_reason(
    'common.non_material_update',
    'Non-material update (Touch)',
    'common',
    true,   -- applies to amend
    false,  -- not for delete
    false,  -- commentary optional
    10
);

select ores.upsert_change_reason(
    'common.rectification',
    'User/Booking Error',
    'common',
    true,   -- applies to amend
    true,   -- applies to delete
    false,  -- commentary optional
    20
);

select ores.upsert_change_reason(
    'common.duplicate',
    'Duplicate Record',
    'common',
    false,  -- not for amend
    true,   -- applies to delete
    false,  -- commentary optional
    30
);

select ores.upsert_change_reason(
    'common.stale_data',
    'Data not updated within required liquidity horizon',
    'common',
    true,   -- applies to amend
    true,   -- applies to delete
    false,  -- commentary optional
    40
);

select ores.upsert_change_reason(
    'common.outlier_correction',
    'Manual override after plausibility check failure',
    'common',
    true,   -- applies to amend
    true,   -- applies to delete
    true,   -- commentary REQUIRED
    50
);

select ores.upsert_change_reason(
    'common.feed_failure',
    'Upstream vendor/API data issue',
    'common',
    true,   -- applies to amend
    true,   -- applies to delete
    true,   -- commentary REQUIRED
    60
);

select ores.upsert_change_reason(
    'common.mapping_error',
    'Incorrect ID translation (e.g., ISIN to FIGI)',
    'common',
    true,   -- applies to amend
    true,   -- applies to delete
    true,   -- commentary REQUIRED
    70
);

select ores.upsert_change_reason(
    'common.judgmental_override',
    'Expert judgment when market prices unavailable',
    'common',
    true,   -- applies to amend
    true,   -- applies to delete
    true,   -- commentary REQUIRED
    80
);

select ores.upsert_change_reason(
    'common.regulatory',
    'Mandatory compliance adjustment',
    'common',
    true,   -- applies to amend
    true,   -- applies to delete
    true,   -- commentary REQUIRED
    90
);

select ores.upsert_change_reason(
    'common.other',
    'Exceptional (requires audit note)',
    'common',
    true,   -- applies to amend
    true,   -- applies to delete
    true,   -- commentary REQUIRED
    1000    -- display last
);

-- =============================================================================
-- Change Reasons: Trade Category (FINRA / MiFID II aligned)
-- =============================================================================

\echo ''
\echo '--- Change Reasons: Trade (FINRA / MiFID II) ---'

select ores.upsert_change_reason(
    'trade.fat_finger',
    'Erroneous execution (wrong quantity/price)',
    'trade',
    true,   -- applies to amend
    true,   -- applies to delete
    false,  -- commentary optional
    10
);

select ores.upsert_change_reason(
    'trade.system_malfunction',
    'Technical glitch or algorithm issue',
    'trade',
    true,   -- applies to amend
    true,   -- applies to delete
    true,   -- commentary REQUIRED
    20
);

select ores.upsert_change_reason(
    'trade.corporate_action',
    'Stock split, dividend, or merger adjustment',
    'trade',
    true,   -- applies to amend
    true,   -- applies to delete
    false,  -- commentary optional
    30
);

select ores.upsert_change_reason(
    'trade.allocation_swap',
    'House to client sub-account reallocation',
    'trade',
    true,   -- applies to amend
    true,   -- applies to delete
    false,  -- commentary optional
    40
);

select ores.upsert_change_reason(
    'trade.re_booking',
    'Wrong legal entity correction',
    'trade',
    true,   -- applies to amend
    true,   -- applies to delete
    true,   -- commentary REQUIRED
    50
);

select ores.upsert_change_reason(
    'trade.other',
    'Exceptional (requires audit note)',
    'trade',
    true,   -- applies to amend
    true,   -- applies to delete
    true,   -- commentary REQUIRED
    1000    -- display last
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_change_reason_category(text, text);
drop function ores.upsert_change_reason(text, text, text, boolean, boolean, boolean, integer);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Change Reason Categories' as entity, count(*) as count
from ores.dq_change_reason_categories_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Change Reasons (system)', count(*)
from ores.dq_change_reasons_tbl where category_code = 'system' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Change Reasons (common)', count(*)
from ores.dq_change_reasons_tbl where category_code = 'common' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Change Reasons (trade)', count(*)
from ores.dq_change_reasons_tbl where category_code = 'trade' and valid_to = ores.utility_infinity_timestamp_fn()
order by entity;
