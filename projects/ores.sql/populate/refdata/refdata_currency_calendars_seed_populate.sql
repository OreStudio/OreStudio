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
 * Currency Calendars Seed Population Script
 *
 * Registers the refdata.currency_calendars dataset and seeds its DQ
 * artefact table with a currency -> calendar mapping for every currency
 * we can derive one for, from two sources:
 *
 * 1. The ~19 currencies with an explicit holiday_calendar already set on
 *    the iso.currencies dataset (curated by hand where the mapping isn't
 *    a simple country match -- e.g. EUR -> TARGET, a supranational
 *    calendar with no single owning country).
 * 2. Every other currency, matched mechanically against
 *    refdata.calendars by country, via the refdata.currency_countries
 *    artefact rows. Where a currency has multiple countries (e.g. EUR)
 *    or a country has multiple calendars (e.g. the US has 7), prefer
 *    the "<Country>.Settlement" one, since that's the general-purpose
 *    spot/settlement calendar the other market-specific ones (NYSE,
 *    SOFR, GovernmentBond, ...) are variants of; falling back to the
 *    bare country-name calendar (no dot) for countries without a
 *    ".Settlement" row, then to any remaining match.
 *
 * One currency also holds a second calendar that no country match finds:
 * USD carries the FOMC meeting calendar beside its settlement calendar,
 * because the short end of the dollar curve steps on FOMC dates. It is
 * stated below as a curated row.
 *
 * Every currency with no country in refdata.currency_countries, or whose
 * country has no calendar in the 41-country coverage of refdata.calendars,
 * is left unassigned rather than forcing an arbitrary mapping -- see
 * refdata.calendars' and refdata.currency_countries' own seed scripts to
 * grow coverage, which grows this join for free next time this runs.
 *
 * Execution order: depends on the refdata.calendars,
 * refdata.currency_countries and iso.currencies seeds having run, because
 * it reads their artefact rows, and the refdata layer runs the
 * currency_countries seed before this one.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_calendars',
        'Calendar Reference Data',
        'Calendars',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Currency Calendars',
        'Currency-to-calendar spot/settlement calendar assignments, curated where a currency has no single owning country (e.g. EUR) and derived by country match against refdata.calendars everywhere else.',
        'ORESTUDIO',
        'Seed data for the currency calendars Librarian bundle',
        current_date,
        'Internal Use Only',
        'currency_calendars'
    );

    -- Dependency edges: currency_calendars rows reference published
    -- currencies and calendars, so iso.currencies and refdata.calendars must
    -- publish first; without these edges dependency-graph resolution may
    -- order currency_calendars either way.
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_calendars',
        'refdata.calendars',
        'calendar_reference'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_calendars',
        'iso.currencies',
        'currency_reference'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

DO $$
declare
    v_dataset_id uuid;
    v_currencies_dataset_id uuid;
    v_calendars_dataset_id uuid;
    v_countries_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.currency_calendars'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_calendars';
    end if;

    select id into v_currencies_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'iso.currencies'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_currencies_dataset_id is null then
        raise exception 'Dataset not found: iso.currencies';
    end if;

    select id into v_calendars_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.calendars'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_calendars_dataset_id is null then
        raise exception 'Dataset not found: refdata.calendars';
    end if;

    select id into v_countries_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.currency_countries'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_countries_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_countries';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_currency_calendars_artefact_tbl
    where dataset_id = v_dataset_id;

    with ranked_country_calendars as (
        -- One best calendar per country: prefer "<Country>.Settlement",
        -- then a bare (dot-free) code, then whatever's left, alphabetically.
        select
            country_code,
            code as calendar_code,
            row_number() over (
                partition by country_code
                order by
                    case when code like '%.Settlement' then 0
                         when code not like '%.%' then 1
                         else 2
                    end,
                    code
            ) as rnk
        from ores_dq_calendars_artefact_tbl
        where dataset_id = v_calendars_dataset_id
          and tenant_id = v_tenant_id
          and country_code <> 'ZZ'
    ),
    -- A second calendar a currency holds beside the one its holiday_calendar
    -- names, and that no country match finds. USD carries the FOMC meeting
    -- calendar because the short end of the dollar curve steps on FOMC dates.
    -- A row is written only where the currency does not already carry that
    -- calendar, so the union below cannot produce a duplicate.
    second_calendars as (
        select v.currency_iso_code, v.calendar_code
        from (values ('USD', 'US.FOMC')) as v(currency_iso_code, calendar_code)
        where not exists (
            select 1
            from ores_dq_currencies_artefact_tbl
            where dataset_id = v_currencies_dataset_id
              and tenant_id = v_tenant_id
              and iso_code = v.currency_iso_code
              and holiday_calendar = v.calendar_code
        )
    ),
    curated as (
        -- 1. Currencies with an explicit holiday_calendar already set.
        select iso_code as currency_iso_code, holiday_calendar as calendar_code
        from ores_dq_currencies_artefact_tbl
        where dataset_id = v_currencies_dataset_id
          and tenant_id = v_tenant_id
          and holiday_calendar is not null
        union all
        select * from second_calendars
    ),
    ranked_currency_countries as (
        -- A currency with several countries (e.g. EUR) picks one
        -- deterministically -- alphabetically first -- purely so the
        -- derivation is stable across reruns; irrelevant for EUR itself
        -- since it's already covered by the curated CTE above.
        select
            currency_iso_code,
            country_alpha2_code,
            row_number() over (
                partition by currency_iso_code
                order by country_alpha2_code
            ) as rnk
        from ores_dq_currency_countries_artefact_tbl
        where dataset_id = v_countries_dataset_id
          and tenant_id = v_tenant_id
    ),
    derived as (
        -- 2. Everyone else, matched by country.
        select c.iso_code as currency_iso_code, rc.calendar_code
        from ores_dq_currencies_artefact_tbl c
        join ranked_currency_countries rcc
            on rcc.currency_iso_code = c.iso_code and rcc.rnk = 1
        join ranked_country_calendars rc
            on rc.country_code = rcc.country_alpha2_code and rc.rnk = 1
        where c.dataset_id = v_currencies_dataset_id
          and c.tenant_id = v_tenant_id
          and c.holiday_calendar is null
    )
    insert into ores_dq_currency_calendars_artefact_tbl (
        dataset_id, tenant_id, currency_iso_code, calendar_code, version
    )
    select v_dataset_id, v_tenant_id, currency_iso_code, calendar_code, 0
    from (
        select * from curated
        union all
        select * from derived
    ) combined;

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % currency calendars for dataset: refdata.currency_calendars', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Currency Calendars Summary ---'

select 'Total DQ Currency Calendars' as metric, count(*) as count
from ores_dq_currency_calendars_artefact_tbl;
