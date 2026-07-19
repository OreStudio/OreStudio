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
 * Currency Pair Convention Calendars Seed Population Script
 *
 * Registers the refdata.currency_pair_convention_calendars dataset and
 * seeds its DQ artefact table by deriving, for every pair in
 * refdata.currency_pair_conventions, the union of both legs' own
 * calendars from the already-populated refdata.currency_calendars
 * artefact -- e.g. EUR/USD's calendars are exactly EUR's own
 * (TARGET) union USD's own (UnitedStates.Settlement), not an
 * independently-curated list. A pair whose leg has no calendar in
 * refdata.currency_calendars simply contributes no row for that leg,
 * rather than forcing one.
 *
 * Execution order: depends on refdata.currency_pair_conventions and
 * refdata.currency_calendars both being populated first in the same
 * script run (this reads their artefact tables directly).
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_pair_convention_calendars',
        'Calendar Reference Data',
        'Calendars',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Currency Pair Convention Calendars',
        'Currency-pair-convention-to-calendar advance-calendar assignments, derived as the union of both legs'' own refdata.currency_calendars assignments.',
        'ORESTUDIO',
        'Seed data for the currency pair convention calendars Librarian bundle',
        current_date,
        'Internal Use Only',
        'currency_pair_convention_calendars'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

DO $$
declare
    v_dataset_id uuid;
    v_conventions_dataset_id uuid;
    v_currency_calendars_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.currency_pair_convention_calendars'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_pair_convention_calendars';
    end if;

    select id into v_conventions_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.currency_pair_conventions'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_conventions_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_pair_conventions';
    end if;

    select id into v_currency_calendars_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.currency_calendars'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_currency_calendars_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_calendars';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_currency_pair_convention_calendars_artefact_tbl
    where dataset_id = v_dataset_id;

    with pairs as (
        select
            pair_code,
            split_part(pair_code, '/', 1) as base_currency,
            split_part(pair_code, '/', 2) as quote_currency
        from ores_dq_currency_pair_conventions_artefact_tbl
        where dataset_id = v_conventions_dataset_id
          and tenant_id = v_tenant_id
    ),
    leg_calendars as (
        select p.pair_code, cc.calendar_code
        from pairs p
        join ores_dq_currency_calendars_artefact_tbl cc
            on cc.currency_iso_code = p.base_currency
        where cc.dataset_id = v_currency_calendars_dataset_id
          and cc.tenant_id = v_tenant_id
        union
        select p.pair_code, cc.calendar_code
        from pairs p
        join ores_dq_currency_calendars_artefact_tbl cc
            on cc.currency_iso_code = p.quote_currency
        where cc.dataset_id = v_currency_calendars_dataset_id
          and cc.tenant_id = v_tenant_id
    )
    insert into ores_dq_currency_pair_convention_calendars_artefact_tbl (
        dataset_id, tenant_id, pair_code, calendar_code, version
    )
    select v_dataset_id, v_tenant_id, pair_code, calendar_code, 0
    from leg_calendars;

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % currency pair convention calendars for dataset: refdata.currency_pair_convention_calendars', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Currency Pair Convention Calendars Summary ---'

select 'Total DQ Currency Pair Convention Calendars' as metric, count(*) as count
from ores_dq_currency_pair_convention_calendars_artefact_tbl;
