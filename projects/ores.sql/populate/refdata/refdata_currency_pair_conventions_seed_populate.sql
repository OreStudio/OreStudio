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
 * Currency Pair Conventions Seed Population Script
 *
 * Registers the refdata.currency_pair_conventions dataset and derives one
 * convention row per currency pair in refdata.currency_pairs: pip_factor
 * and decimal_places follow the standard FX convention (JPY-quoted pairs
 * use 2 decimal places / 0.01 pip; everything else uses 4 decimal places /
 * 0.0001 pip), tick_size defaults to 1.0 (one pip, per
 * doc/knowledge/domain/fx_pip_tick_and_pip_factor.org — tick_size is a pip
 * count, not an absolute rate move), advance_calendar comma-joins both
 * legs' currency.holiday_calendar (only the known ones — falls back to a
 * single calendar, or null, when one or both legs have none set), and
 * business_day_convention/spot_relative/end_of_month use the standard FX
 * spot defaults (Modified Following, spot-relative, no end-of-month roll).
 *
 * Execution order: depends on refdata.currency_pairs and iso.currencies
 * already being populated in the same database (reads both artefact
 * tables directly; not a hard dependency on publish order, since this
 * only reads DQ artefact data, not the published refdata tables).
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_pair_conventions',
        'FX Market Conventions',
        'Currency Pairs',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Standard Currency Pair Conventions',
        'Quoting and date conventions (pip factor, tick size, calendars) for every pair in the Standard Currency Pairs dataset.',
        'ORESTUDIO',
        'Seed data for the currency pair conventions Librarian bundle',
        current_date,
        'Internal Use Only',
        'currency_pair_conventions'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

DO $$
declare
    v_dataset_id uuid;
    v_pairs_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.currency_pair_conventions'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_pair_conventions';
    end if;

    select id into v_pairs_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.currency_pairs'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_pairs_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_pairs (must be populated first)';
    end if;

    -- Clear existing conventions for this dataset (idempotency)
    delete from ores_dq_currency_pair_conventions_artefact_tbl
    where dataset_id = v_dataset_id;

    raise debug 'Populating currency pair conventions for dataset: refdata.currency_pair_conventions';

    insert into ores_dq_currency_pair_conventions_artefact_tbl (
        dataset_id, tenant_id, pair_code, version,
        pip_factor, tick_size, decimal_places,
        advance_calendar, business_day_convention, spot_relative, end_of_month
    )
    select
        v_dataset_id,
        v_tenant_id,
        p.pair_code,
        0,
        case when p.quote_currency = 'JPY' then 0.01 else 0.0001 end,
        1.0,
        case when p.quote_currency = 'JPY' then 2 else 4 end,
        nullif(concat_ws(',', base.holiday_calendar, quote.holiday_calendar), ''),
        'ModifiedFollowing',
        true,
        false
    from ores_dq_currency_pairs_artefact_tbl p
    join ores_dq_datasets_tbl iso_ds
        on iso_ds.name = 'ISO 4217 Currency Codes'
        and iso_ds.subject_area_name = 'Currencies'
        and iso_ds.domain_name = 'Reference Data'
    left join ores_dq_currencies_artefact_tbl base
        on base.dataset_id = iso_ds.id
        and base.tenant_id = v_tenant_id
        and base.iso_code = p.base_currency
    left join ores_dq_currencies_artefact_tbl quote
        on quote.dataset_id = iso_ds.id
        and quote.tenant_id = v_tenant_id
        and quote.iso_code = p.quote_currency
    where p.dataset_id = v_pairs_dataset_id
      and p.tenant_id = v_tenant_id;

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % currency pair conventions for dataset: refdata.currency_pair_conventions', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Currency Pair Conventions Summary ---'

select 'Total DQ Currency Pair Conventions' as metric, count(*) as count
from ores_dq_currency_pair_conventions_artefact_tbl
union all
select 'JPY-Quoted (2dp)', count(*)
from ores_dq_currency_pair_conventions_artefact_tbl
where decimal_places = 2
union all
select 'With Advance Calendar', count(*)
from ores_dq_currency_pair_conventions_artefact_tbl
where advance_calendar is not null;
