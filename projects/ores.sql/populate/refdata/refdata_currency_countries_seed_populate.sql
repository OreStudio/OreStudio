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
 * Currency Countries Seed Population Script
 *
 * Registers the refdata.currency_countries dataset and seeds its DQ
 * artefact table with the currency-to-issuing-country mapping: the six
 * countries that issue the euro, and one country for each of the other
 * currencies the platform carries.
 *
 * The mapping is the dataset's own data and is not derived from a live
 * table. The live table has one writer, this dataset's publish function,
 * so a derivation read there would find nothing until a publish had run.
 *
 * Execution order: no dependency on another seed. Every row names a
 * currency and a country the publish validates against the target
 * tenant, so iso.currencies and iso.countries publish first; the
 * dependency edges below order the two.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_countries',
        'Calendar Reference Data',
        'Calendars',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Currency Countries',
        'Currency-to-country issuance assignments (a currency can be issued by more than one country, e.g. EUR).',
        'ORESTUDIO',
        'Seed data for the currency countries Librarian bundle',
        current_date,
        'Internal Use Only',
        'currency_countries'
    );

    -- Dependency edges: currency_countries rows reference published
    -- currencies and countries, so iso.currencies and iso.countries must
    -- publish first; without these edges dependency-graph resolution may
    -- order currency_countries either way.
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_countries',
        'iso.countries',
        'country_reference'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_countries',
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
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.currency_countries'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_countries';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_currency_countries_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_currency_countries_artefact_tbl (
        dataset_id, tenant_id, currency_iso_code, country_alpha2_code, version
    )
    values
        (v_dataset_id, v_tenant_id, 'EUR', 'DE', 0),
        (v_dataset_id, v_tenant_id, 'EUR', 'FR', 0),
        (v_dataset_id, v_tenant_id, 'EUR', 'IT', 0),
        (v_dataset_id, v_tenant_id, 'EUR', 'ES', 0),
        (v_dataset_id, v_tenant_id, 'EUR', 'AT', 0),
        (v_dataset_id, v_tenant_id, 'EUR', 'FI', 0),
        (v_dataset_id, v_tenant_id, 'USD', 'US', 0),
        (v_dataset_id, v_tenant_id, 'GBP', 'GB', 0),
        (v_dataset_id, v_tenant_id, 'JPY', 'JP', 0),
        (v_dataset_id, v_tenant_id, 'CHF', 'CH', 0),
        (v_dataset_id, v_tenant_id, 'CAD', 'CA', 0),
        (v_dataset_id, v_tenant_id, 'AUD', 'AU', 0),
        (v_dataset_id, v_tenant_id, 'SEK', 'SE', 0),
        (v_dataset_id, v_tenant_id, 'NOK', 'NO', 0),
        (v_dataset_id, v_tenant_id, 'DKK', 'DK', 0),
        (v_dataset_id, v_tenant_id, 'CNY', 'CN', 0),
        (v_dataset_id, v_tenant_id, 'HKD', 'HK', 0),
        (v_dataset_id, v_tenant_id, 'SGD', 'SG', 0),
        (v_dataset_id, v_tenant_id, 'ZAR', 'ZA', 0),
        (v_dataset_id, v_tenant_id, 'INR', 'IN', 0),
        (v_dataset_id, v_tenant_id, 'KRW', 'KR', 0),
        (v_dataset_id, v_tenant_id, 'MXN', 'MX', 0),
        (v_dataset_id, v_tenant_id, 'BRL', 'BR', 0),
        (v_dataset_id, v_tenant_id, 'ARS', 'AR', 0),
        (v_dataset_id, v_tenant_id, 'CLP', 'CL', 0),
        (v_dataset_id, v_tenant_id, 'CZK', 'CZ', 0),
        (v_dataset_id, v_tenant_id, 'HUF', 'HU', 0),
        (v_dataset_id, v_tenant_id, 'ISK', 'IS', 0),
        (v_dataset_id, v_tenant_id, 'PLN', 'PL', 0),
        (v_dataset_id, v_tenant_id, 'RON', 'RO', 0),
        (v_dataset_id, v_tenant_id, 'RUB', 'RU', 0),
        (v_dataset_id, v_tenant_id, 'SAR', 'SA', 0),
        (v_dataset_id, v_tenant_id, 'THB', 'TH', 0),
        (v_dataset_id, v_tenant_id, 'TRY', 'TR', 0),
        (v_dataset_id, v_tenant_id, 'TWD', 'TW', 0),
        (v_dataset_id, v_tenant_id, 'UAH', 'UA', 0),
        (v_dataset_id, v_tenant_id, 'ILS', 'IL', 0),
        (v_dataset_id, v_tenant_id, 'IDR', 'ID', 0),
        (v_dataset_id, v_tenant_id, 'BWP', 'BW', 0);

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % currency countries for dataset: refdata.currency_countries', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Currency Countries Summary ---'

select 'Total DQ Currency Countries' as metric, count(*) as count
from ores_dq_currency_countries_artefact_tbl;
