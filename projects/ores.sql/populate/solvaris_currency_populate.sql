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
 * Data Quality Solvaris Currencies Artefact Population Script
 *
 * Populates the dq_currencies_artefact_tbl with Solvaris currency data.
 * Source: country_currency.json model
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
    -- Get the currencies dataset ID from the datasets model
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where name = 'Solvaris Currencies'
      and subject_area_name = 'Currencies'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found for currencies';
    end if;

    -- Get the flags dataset ID (for linking images)
    select id into v_flags_dataset_id
    from ores.dq_datasets_tbl
    where name = 'Solvaris Country Flag Images'
      and subject_area_name = 'Country Flags'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_flags_dataset_id is null then
        raise exception 'Dataset not found for flag images';
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

    raise notice 'Populating currencies for dataset: %', (select name from ores.dq_datasets_tbl where id = v_dataset_id);

    -- Insert Solvaris currencies with flag image links
    insert into ores.dq_currencies_artefact_tbl (
        dataset_id, iso_code, version, name, numeric_code, symbol, fraction_symbol,
        fractions_per_unit, rounding_type, rounding_precision, format, currency_type, image_id
    )
    select
        v_dataset_id,
        c.iso_code,
        0,
        c.name,
        c.numeric_code::text,
        c.symbol,
        c.fraction_symbol,
        c.fractions_per_unit,
        c.rounding_type,
        c.rounding_precision,
        c.format,
        c.currency_type,
        coalesce(i.image_id, v_placeholder_image_id)
    from (values
        ('ALD', 'Aerilonian Ducat', 10001, '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'al'),
        ('ARA', 'Arcturian Aurum', 10002, '€', 'c', 100, 'standard', 2, '€#,##0.00', 'fiat.major', 'ar'),
        ('BAF', 'Balthorian Florin', 10003, '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat.major', 'ba'),
        ('BEB', 'Bellorian Bezant', 10004, '¥', '', 0, 'standard', 0, '¥#,##0', 'fiat.emerging', 'be'),
        ('CAC', 'Calandrian Crown', 10005, 'kr', 'ø', 100, 'standard', 2, 'kr #,##0.00', 'fiat.major', 'ca'),
        ('CDC', 'Caledonian Credit', 10006, 'zł', 'gr', 100, 'standard', 2, '#,##0.00 zł', 'fiat.emerging', 'cd'),
        ('DAD', 'Daelorian Dinar', 10007, '₹', 'p', 100, 'standard', 2, '₹#,##0.00', 'fiat.emerging', 'da'),
        ('DED', 'Delvadian Drachma', 10008, 'د.إ', 'ف', 100, 'standard', 2, 'د.إ#,##0.00', 'fiat.emerging', 'de'),
        ('ERE', 'Eriadoran Ecu', 10009, 'R', 'c', 100, 'standard', 2, 'R#,##0.00', 'fiat.emerging', 'er'),
        ('ESE', 'Esterian Escudo', 10010, 'ƒ', '¢', 100, 'standard', 2, 'ƒ#,##0.00', 'fiat.emerging', 'es'),
        ('FEF', 'Felorian Franc', 10011, 'د.ك', 'ف', 1000, 'standard', 3, 'د.ك#,##0.000', 'fiat.emerging', 'fe'),
        ('FNF', 'Fendarian Farthing', 10012, 'S/', '¢', 100, 'standard', 2, 'S/#,##0.00', 'fiat.emerging', 'fn'),
        ('GAG', 'Galdorian Galleon', 10013, '฿', 'ส', 100, 'standard', 2, '฿#,##0.00', 'fiat.emerging', 'ga'),
        ('GRG', 'Grendorian Guilder', 10014, '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'gr'),
        ('HEF', 'Helvetian Florin', 10015, '€', 'c', 100, 'standard', 2, '€#,##0.00', 'fiat.major', 'he'),
        ('HYH', 'Hydronian Helix', 10016, '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat.major', 'hy'),
        ('IRD', 'Iridian Ducat', 10017, '¥', '', 0, 'standard', 0, '¥#,##0', 'fiat.emerging', 'ir'),
        ('ITI', 'Ithacan Ion', 10018, 'kr', 'ø', 100, 'standard', 2, 'kr #,##0.00', 'fiat.major', 'it'),
        ('JEJ', 'Jethronian Jewel', 10019, 'zł', 'gr', 100, 'standard', 2, '#,##0.00 zł', 'fiat.emerging', 'je'),
        ('JOK', 'Jorvikian Krona', 10020, '₹', 'p', 100, 'standard', 2, '₹#,##0.00', 'fiat.emerging', 'jo'),
        ('KAK', 'Kaelorian Krone', 10021, 'د.إ', 'ف', 100, 'standard', 2, 'د.إ#,##0.00', 'fiat.emerging', 'ka'),
        ('KRK', 'Krynnish Kredit', 10022, 'R', 'c', 100, 'standard', 2, 'R#,##0.00', 'fiat.emerging', 'kr'),
        ('LUL', 'Luminian Lumen', 10023, 'ƒ', '¢', 100, 'standard', 2, 'ƒ#,##0.00', 'fiat.emerging', 'lu'),
        ('LYL', 'Lysandrian Lira', 10024, 'د.ك', 'ف', 1000, 'standard', 3, 'د.ك#,##0.000', 'fiat.emerging', 'ly'),
        ('MAM', 'Maldorian Mark', 10025, 'S/', '¢', 100, 'standard', 2, 'S/#,##0.00', 'fiat.emerging', 'ma'),
        ('MRP', 'Mariposan Peso', 10026, '฿', 'ส', 100, 'standard', 2, '฿#,##0.00', 'fiat.emerging', 'mr'),
        ('NEN', 'Nektonian Nexus', 10027, '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'ne'),
        ('NTN', 'Netharian Naira', 10028, '€', 'c', 100, 'standard', 2, '€#,##0.00', 'fiat.major', 'nt'),
        ('ORB', 'Orinocan Bolivar', 10029, '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat.major', 'or'),
        ('OLO', 'Orlanthian Orb', 10030, '¥', '', 0, 'standard', 0, '¥#,##0', 'fiat.emerging', 'ol'),
        ('PAP', 'Paldorian Pound', 10031, 'kr', 'ø', 100, 'standard', 2, 'kr #,##0.00', 'fiat.major', 'pa'),
        ('PYP', 'Pyrrhian Pyre', 10032, 'zł', 'gr', 100, 'standard', 2, '#,##0.00 zł', 'fiat.emerging', 'py'),
        ('QUQ', 'Quentarian Quill', 10033, '₹', 'p', 100, 'standard', 2, '₹#,##0.00', 'fiat.emerging', 'qu'),
        ('QNQ', 'Quinarian Quetzal', 10034, 'د.إ', 'ف', 100, 'standard', 2, 'د.إ#,##0.00', 'fiat.emerging', 'qn'),
        ('RER', 'Rendellian Real', 10035, 'R', 'c', 100, 'standard', 2, 'R#,##0.00', 'fiat.emerging', 're'),
        ('RIR', 'Rivenian Ruble', 10036, 'ƒ', '¢', 100, 'standard', 2, 'ƒ#,##0.00', 'fiat.emerging', 'ri'),
        ('SES', 'Serendian Shilling', 10037, 'د.ك', 'ف', 1000, 'standard', 3, 'د.ك#,##0.000', 'fiat.emerging', 'se'),
        ('SIS', 'Sildorian Sovereign', 10038, 'S/', '¢', 100, 'standard', 2, 'S/#,##0.00', 'fiat.emerging', 'si'),
        ('TAT', 'Tandorian Taka', 10039, '฿', 'ส', 100, 'standard', 2, '฿#,##0.00', 'fiat.emerging', 'ta'),
        ('TET', 'Tenebrian Talon', 10040, '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'te'),
        ('ULU', 'Uldorian Unit', 10041, '€', 'c', 100, 'standard', 2, '€#,##0.00', 'fiat.major', 'ul'),
        ('UTU', 'Utopian Utopia', 10042, '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat.major', 'ut'),
        ('VAV', 'Valorian Valor', 10043, '¥', '', 0, 'standard', 0, '¥#,##0', 'fiat.emerging', 'va'),
        ('VLV', 'Valtarian Vault', 10044, 'kr', 'ø', 100, 'standard', 2, 'kr #,##0.00', 'fiat.major', 'vl'),
        ('WIW', 'Wintervalean Win', 10045, 'zł', 'gr', 100, 'standard', 2, '#,##0.00 zł', 'fiat.emerging', 'wi'),
        ('WYW', 'Wysterian Wyre', 10046, '₹', 'p', 100, 'standard', 2, '₹#,##0.00', 'fiat.emerging', 'wy'),
        ('XAX', 'Xandrian Xenon', 10047, 'د.إ', 'ف', 100, 'standard', 2, 'د.إ#,##0.00', 'fiat.emerging', 'xa'),
        ('XEX', 'Xenorian Xerxes', 10048, 'R', 'c', 100, 'standard', 2, 'R#,##0.00', 'fiat.emerging', 'xe'),
        ('YSY', 'Yslandian Ysol', 10049, 'ƒ', '¢', 100, 'standard', 2, 'ƒ#,##0.00', 'fiat.emerging', 'ys'),
        ('ZEZ', 'Zephyrian Zephyr', 10050, 'د.ك', 'ف', 1000, 'standard', 3, 'د.ك#,##0.000', 'fiat.emerging', 'ze'),
        ('FAF', 'Faelandian Fae', 10100, 'S/', '¢', 100, 'standard', 2, 'S/#,##0.00', 'fiat.emerging', 'fa')
    ) as c(iso_code, name, numeric_code, symbol, fraction_symbol, fractions_per_unit, rounding_type, rounding_precision, format, currency_type, flag_key)
    left join ores.dq_images_artefact_tbl i
        on i.dataset_id = v_flags_dataset_id
        and i.key = c.flag_key;

    get diagnostics v_count = row_count;

    raise notice 'Successfully populated % currencies for dataset: %', v_count, (select name from ores.dq_datasets_tbl where id = v_dataset_id);
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Solvaris Currencies Summary ---'

with currencies as (
    select c.currency_type
    from ores.dq_currencies_artefact_tbl c
    join ores.dq_datasets_tbl d on c.dataset_id = d.id
    where d.subject_area_name = 'Currencies'
      and d.domain_name = 'Reference Data'
      and d.valid_to = ores.utility_infinity_timestamp_fn()
)
select 'Total Solvaris Currencies' as metric, count(*) as count from currencies
union all
select 'Fiat Currencies', count(*) from currencies where currency_type = 'fiat.standard'
union all
select 'Synthetic Currencies', count(*) from currencies where currency_type = 'synthetic';
