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
 * Data Quality FpML Non-ISO Currency Codes Artefact Population Script
 *
 * Populates the dq_currencies_artefact_tbl with FpML non-ISO currency data.
 * Source: http://www.fpml.org/coding-scheme/non-iso-currency-1-1.xml
 *
 * These currencies extend ISO 4217 for derivatives trading:
 * - Offshore currencies: CNH (Hong Kong), CNT (Taiwan)
 * - Crown Dependencies: GGP (Guernsey), IMP (Isle of Man), JEP (Jersey)
 * - Pacific islands: KID (Kiribati), TVD (Tuvalu)
 * - Historical: MCF (Monaco), SML (San Marino), VAL (Vatican)
 *
 * This script is idempotent.
 */

set schema 'ores';

DO $$
declare
    v_dataset_id uuid;
    v_flags_dataset_id uuid;
    v_placeholder_image_id uuid;
    v_count integer := 0;
begin
    -- Get the FpML Non-ISO Currency Codes dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where name = 'FpML Non-ISO Currency Codes'
      and subject_area_name = 'Currencies'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: FpML Non-ISO Currency Codes';
    end if;

    -- Get the flags dataset ID (for linking images)
    select id into v_flags_dataset_id
    from ores.dq_datasets_tbl
    where name = 'Country Flag Images'
      and subject_area_name = 'Country Flags'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_flags_dataset_id is null then
        raise exception 'Dataset not found: Country Flag Images';
    end if;

    -- Get the placeholder image (xx.svg = "no flag available")
    select image_id into v_placeholder_image_id
    from ores.dq_images_artefact_tbl
    where dataset_id = v_flags_dataset_id
      and key = 'xx';

    if v_placeholder_image_id is null then
        raise warning 'Placeholder image (xx) not found - currencies without flags will have NULL image_id';
    end if;

    -- Clear existing currencies for this dataset (idempotency)
    delete from ores.dq_currencies_artefact_tbl
    where dataset_id = v_dataset_id;

    raise notice 'Populating currencies for dataset: FpML Non-ISO Currency Codes';

    -- Insert FpML non-ISO currencies with flag image links
    -- Data sourced from: http://www.fpml.org/coding-scheme/non-iso-currency-1-1.xml
    insert into ores.dq_currencies_artefact_tbl (
        dataset_id, iso_code, version, name, numeric_code, symbol, fraction_symbol,
        fractions_per_unit, rounding_type, rounding_precision, format, currency_type, image_id
    )
    select
        v_dataset_id,
        c.iso_code,
        0,
        c.name,
        c.numeric_code,
        c.symbol,
        c.fraction_symbol,
        c.fractions_per_unit,
        c.rounding_type,
        c.rounding_precision,
        c.format,
        c.currency_type,
        coalesce(i.image_id, v_placeholder_image_id)
    from (values
        -- Offshore Chinese Yuan variants
        ('CNH', 'Offshore Chinese Yuan (Hong Kong)', '', '¥', '分', 100, 'standard', 2, '¥#,##0.00', 'fiat.offshore', 'hk'),
        ('CNT', 'Offshore Chinese Yuan (Taiwan)', '', '¥', '分', 100, 'standard', 2, '¥#,##0.00', 'fiat.offshore', 'tw'),
        -- British Crown Dependencies (pegged to GBP)
        ('GGP', 'Guernsey Pound', '', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat.emerging', 'gg'),
        ('IMP', 'Isle of Man Pound', '', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat.emerging', 'im'),
        ('JEP', 'Jersey Pound', '', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat.emerging', 'je'),
        -- Pacific Island currencies (pegged to AUD)
        ('KID', 'Kiribati Dollar', '', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'ki'),
        ('TVD', 'Tuvalu Dollar', '', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'tv'),
        -- Historical European currencies
        ('MCF', 'Monegasque Franc', '', '₣', 'c', 100, 'standard', 2, '₣#,##0.00', 'fiat.historical', 'mc'),
        ('SML', 'Sammarinese Lira', '', '₤', 'c', 100, 'standard', 2, '₤#,##0.00', 'fiat.historical', 'sm'),
        ('VAL', 'Vatican Lira', '', '₤', 'c', 100, 'standard', 2, '₤#,##0.00', 'fiat.historical', 'va')
    ) as c(iso_code, name, numeric_code, symbol, fraction_symbol, fractions_per_unit, rounding_type, rounding_precision, format, currency_type, flag_key)
    left join ores.dq_images_artefact_tbl i
        on i.dataset_id = v_flags_dataset_id
        and i.key = c.flag_key;

    get diagnostics v_count = row_count;

    raise notice 'Successfully populated % currencies for dataset: FpML Non-ISO Currency Codes', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ FpML Non-ISO Currency Codes Summary ---'

select 'Total FpML Non-ISO Currency Codes' as metric, count(*) as count
from ores.dq_currencies_artefact_tbl c
join ores.dq_datasets_tbl d on c.dataset_id = d.id
where d.name = 'FpML Non-ISO Currency Codes'
  and d.valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Offshore Currencies (fiat.offshore)', count(*)
from ores.dq_currencies_artefact_tbl c
join ores.dq_datasets_tbl d on c.dataset_id = d.id
where d.name = 'FpML Non-ISO Currency Codes'
  and d.valid_to = ores.utility_infinity_timestamp_fn()
  and c.currency_type = 'fiat.offshore'
union all
select 'Emerging Currencies (fiat.emerging)', count(*)
from ores.dq_currencies_artefact_tbl c
join ores.dq_datasets_tbl d on c.dataset_id = d.id
where d.name = 'FpML Non-ISO Currency Codes'
  and d.valid_to = ores.utility_infinity_timestamp_fn()
  and c.currency_type = 'fiat.emerging'
union all
select 'Historical Currencies (fiat.historical)', count(*)
from ores.dq_currencies_artefact_tbl c
join ores.dq_datasets_tbl d on c.dataset_id = d.id
where d.name = 'FpML Non-ISO Currency Codes'
  and d.valid_to = ores.utility_infinity_timestamp_fn()
  and c.currency_type = 'fiat.historical';
