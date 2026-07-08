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
 * Data Quality Currencies Artefact Population Script
 *
 * Populates the dq_currencies_artefact_tbl with ISO 4217 currency data.
 * Links currencies to their flag images from the flags dataset.
 * This script is idempotent.
 */


DO $$
declare
    v_currencies_dataset_id uuid;
    v_flags_dataset_id uuid;
    v_placeholder_image_id uuid;
    v_count integer := 0;
begin
    -- Get the currencies dataset ID
    select id into v_currencies_dataset_id
    from ores_dq_datasets_tbl
    where name = 'ISO 4217 Currency Codes'
      and subject_area_name = 'Currencies'
      and domain_name = 'Reference Data'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_currencies_dataset_id is null then
        raise exception 'Dataset not found: ISO 4217 Currency Codes';
    end if;

    -- Get the flags dataset ID (for linking images)
    select id into v_flags_dataset_id
    from ores_dq_datasets_tbl
    where name = 'Country Flag Images'
      and subject_area_name = 'Country Flags'
      and domain_name = 'Reference Data'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_flags_dataset_id is null then
        raise exception 'Dataset not found: Country Flag Images';
    end if;

    -- Get the placeholder image (xx.svg = "no flag available")
    select image_id into v_placeholder_image_id
    from ores_dq_images_artefact_tbl
    where dataset_id = v_flags_dataset_id
      and key = 'xx';

    if v_placeholder_image_id is null then
        raise warning 'Placeholder image (xx) not found - currencies without flags will have NULL image_id';
    end if;

    -- Clear existing currencies for this dataset (idempotency)
    delete from ores_dq_currencies_artefact_tbl
    where dataset_id = v_currencies_dataset_id;

    raise debug 'Populating currencies for dataset: ISO 4217 Currency Codes';

    -- Insert currencies with flag image links
    -- Currency-to-flag mapping: each currency maps to its issuing country's flag
    -- Special cases: EUR -> 'eu', XAU -> 'xau', XDR -> 'xdr', etc.
    --
    -- spot_days: business days to settlement for a spot trade (USD is 1, everyone else is 2).
    -- deliverable: false for non-deliverable (NDF) currencies that settle cash-only in a
    --   third currency due to capital controls.
    -- day_basis: money-market day-count convention (mostly ACT/360; Commonwealth-convention
    --   currencies use ACT/365).
    -- base_precedence: base-currency precedence hierarchy for canonical pair ordering
    --   (lower = more senior as base leg), per the standard FX market quoting convention;
    --   100 = unranked.
    -- holiday_calendar: named ORE calendar for spot-date computation; null where no
    --   dedicated calendar exists yet.
    insert into ores_dq_currencies_artefact_tbl (
        dataset_id, tenant_id, iso_code, version, name, numeric_code, symbol, fraction_symbol,
        fractions_per_unit, rounding_type, rounding_precision, format, monetary_nature, market_tier, image_id,
        spot_days, deliverable, day_basis, base_precedence, holiday_calendar
    )
    select
        v_currencies_dataset_id,
        ores_utility_system_tenant_id_fn(),
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
        split_part(c.currency_type, '.', 1),
        split_part(c.currency_type, '.', 2),
        coalesce(i.image_id, v_placeholder_image_id),
        c.spot_days,
        c.deliverable,
        c.day_basis,
        c.base_precedence,
        c.holiday_calendar
    from (values
        -- Americas
        ('USD', 'US Dollar', '840', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.g10', 'us', 1, true, 'ACT/360', 5, 'US'),
        ('CAD', 'Canadian Dollar', '124', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.g10', 'ca', 2, true, 'ACT/365', 6, 'Canada'),
        ('MXN', 'Mexican Peso', '484', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'mx', 2, true, 'ACT/360', 100, 'Mexico'),
        ('BRL', 'Brazilian Real', '986', 'R$', '¢', 100, 'Closest', 2, 'R$#,##0.00', 'fiat.emerging', 'br', 2, true, 'ACT/360', 100, 'Brazil'),
        ('ARS', 'Argentine Peso', '032', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'ar', 2, false, 'ACT/360', 100, null),
        ('CLP', 'Chilean Peso', '152', '$', '', 0, 'Closest', 0, '$#,##0', 'fiat.emerging', 'cl', 2, true, 'ACT/360', 100, null),
        ('COP', 'Colombian Peso', '170', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'co', 2, true, 'ACT/360', 100, null),
        ('PEN', 'Peruvian Sol', '604', 'S/', '¢', 100, 'Closest', 2, 'S/#,##0.00', 'fiat.emerging', 'pe', 2, true, 'ACT/360', 100, null),
        ('UYU', 'Uruguayan Peso', '858', '$U', '¢', 100, 'Closest', 2, '$U#,##0.00', 'fiat.emerging', 'uy', 2, true, 'ACT/360', 100, null),
        ('PYG', 'Paraguayan Guarani', '600', '₲', '', 0, 'Closest', 0, '₲#,##0', 'fiat.emerging', 'py', 2, true, 'ACT/360', 100, null),
        ('BOB', 'Bolivian Boliviano', '068', 'Bs', '¢', 100, 'Closest', 2, 'Bs#,##0.00', 'fiat.emerging', 'bo', 2, true, 'ACT/360', 100, null),
        ('VES', 'Venezuelan Bolivar', '928', 'Bs', '¢', 100, 'Closest', 2, 'Bs#,##0.00', 'fiat.emerging', 've', 2, false, 'ACT/360', 100, null),
        ('CRC', 'Costa Rican Colon', '188', '₡', '¢', 100, 'Closest', 2, '₡#,##0.00', 'fiat.emerging', 'cr', 2, true, 'ACT/360', 100, null),
        ('PAB', 'Panamanian Balboa', '590', 'B/.', '¢', 100, 'Closest', 2, 'B/.#,##0.00', 'fiat.emerging', 'pa', 2, true, 'ACT/360', 100, null),
        ('GTQ', 'Guatemalan Quetzal', '320', 'Q', '¢', 100, 'Closest', 2, 'Q#,##0.00', 'fiat.emerging', 'gt', 2, true, 'ACT/360', 100, null),
        ('HNL', 'Honduran Lempira', '340', 'L', '¢', 100, 'Closest', 2, 'L#,##0.00', 'fiat.emerging', 'hn', 2, true, 'ACT/360', 100, null),
        ('NIO', 'Nicaraguan Cordoba', '558', 'C$', '¢', 100, 'Closest', 2, 'C$#,##0.00', 'fiat.emerging', 'ni', 2, true, 'ACT/360', 100, null),
        ('DOP', 'Dominican Peso', '214', 'RD$', '¢', 100, 'Closest', 2, 'RD$#,##0.00', 'fiat.emerging', 'do', 2, true, 'ACT/360', 100, null),
        ('CUP', 'Cuban Peso', '192', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'cu', 2, true, 'ACT/360', 100, null),
        ('JMD', 'Jamaican Dollar', '388', 'J$', '¢', 100, 'Closest', 2, 'J$#,##0.00', 'fiat.emerging', 'jm', 2, true, 'ACT/360', 100, null),
        ('TTD', 'Trinidad and Tobago Dollar', '780', 'TT$', '¢', 100, 'Closest', 2, 'TT$#,##0.00', 'fiat.emerging', 'tt', 2, true, 'ACT/360', 100, null),
        ('BBD', 'Barbadian Dollar', '052', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'bb', 2, true, 'ACT/360', 100, null),
        ('BSD', 'Bahamian Dollar', '044', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'bs', 2, true, 'ACT/360', 100, null),
        ('HTG', 'Haitian Gourde', '332', 'G', '¢', 100, 'Closest', 2, 'G#,##0.00', 'fiat.emerging', 'ht', 2, true, 'ACT/360', 100, null),
        ('SRD', 'Surinamese Dollar', '968', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'sr', 2, true, 'ACT/360', 100, null),
        ('GYD', 'Guyanese Dollar', '328', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'gy', 2, true, 'ACT/360', 100, null),
        ('BZD', 'Belize Dollar', '084', 'BZ$', '¢', 100, 'Closest', 2, 'BZ$#,##0.00', 'fiat.emerging', 'bz', 2, true, 'ACT/360', 100, null),
        ('AWG', 'Aruban Florin', '533', 'ƒ', '¢', 100, 'Closest', 2, 'ƒ#,##0.00', 'fiat.emerging', 'aw', 2, true, 'ACT/360', 100, null),
        ('ANG', 'Netherlands Antillean Guilder', '532', 'ƒ', '¢', 100, 'Closest', 2, 'ƒ#,##0.00', 'fiat.emerging', 'cw', 2, true, 'ACT/360', 100, null),
        ('XCD', 'East Caribbean Dollar', '951', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'ag', 2, true, 'ACT/360', 100, null),
        ('KYD', 'Cayman Islands Dollar', '136', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'ky', 2, true, 'ACT/360', 100, null),
        ('BMD', 'Bermudian Dollar', '060', '$', '¢', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'bm', 2, true, 'ACT/360', 100, null),
        ('FKP', 'Falkland Islands Pound', '238', '£', 'p', 100, 'Closest', 2, '£#,##0.00', 'fiat.emerging', 'fk', 2, true, 'ACT/365', 100, null),
        -- Europe
        ('EUR', 'Euro', '978', '€', 'c', 100, 'Closest', 2, '€#,##0.00', 'fiat.g10', 'eu', 2, true, 'ACT/360', 1, 'TARGET'),
        ('GBP', 'British Pound Sterling', '826', '£', 'p', 100, 'Closest', 2, '£#,##0.00', 'fiat.g10', 'gb', 2, true, 'ACT/365', 2, 'UK'),
        ('CHF', 'Swiss Franc', '756', 'CHF', 'c', 100, 'Closest', 2, 'CHF #,##0.00', 'fiat.g10', 'ch', 2, true, 'ACT/360', 7, 'Switzerland'),
        ('NOK', 'Norwegian Krone', '578', 'kr', 'ø', 100, 'Closest', 2, 'kr #,##0.00', 'fiat.g10', 'no', 2, true, 'ACT/360', 9, 'Norway'),
        ('SEK', 'Swedish Krona', '752', 'kr', 'ö', 100, 'Closest', 2, '#,##0.00 kr', 'fiat.g10', 'se', 2, true, 'ACT/360', 10, 'Sweden'),
        ('DKK', 'Danish Krone', '208', 'kr', 'ø', 100, 'Closest', 2, 'kr #,##0.00', 'fiat.g10', 'dk', 2, true, 'ACT/360', 11, 'Denmark'),
        ('ISK', 'Icelandic Krona', '352', 'kr', '', 0, 'Closest', 0, '#,##0 kr', 'fiat.emerging', 'is', 2, true, 'ACT/360', 100, null),
        ('PLN', 'Polish Zloty', '985', 'zł', 'gr', 100, 'Closest', 2, '#,##0.00 zł', 'fiat.emerging', 'pl', 2, true, 'ACT/360', 100, null),
        ('CZK', 'Czech Koruna', '203', 'Kč', 'h', 100, 'Closest', 2, '#,##0.00 Kč', 'fiat.emerging', 'cz', 2, true, 'ACT/360', 100, null),
        ('HUF', 'Hungarian Forint', '348', 'Ft', 'f', 100, 'Closest', 2, '#,##0.00 Ft', 'fiat.emerging', 'hu', 2, true, 'ACT/360', 100, null),
        ('RON', 'Romanian Leu', '946', 'lei', 'b', 100, 'Closest', 2, '#,##0.00 lei', 'fiat.emerging', 'ro', 2, true, 'ACT/360', 100, null),
        ('BGN', 'Bulgarian Lev', '975', 'лв', 'ст', 100, 'Closest', 2, '#,##0.00 лв', 'fiat.emerging', 'bg', 2, true, 'ACT/360', 100, null),
        ('HRK', 'Croatian Kuna', '191', 'kn', 'lp', 100, 'Closest', 2, '#,##0.00 kn', 'fiat.emerging', 'hr', 2, true, 'ACT/360', 100, null),
        ('RSD', 'Serbian Dinar', '941', 'дин', 'п', 100, 'Closest', 2, '#,##0.00 дин', 'fiat.emerging', 'rs', 2, true, 'ACT/360', 100, null),
        ('BAM', 'Bosnia and Herzegovina Convertible Mark', '977', 'KM', 'pf', 100, 'Closest', 2, '#,##0.00 KM', 'fiat.emerging', 'ba', 2, true, 'ACT/360', 100, null),
        ('MKD', 'Macedonian Denar', '807', 'ден', 'д', 100, 'Closest', 2, '#,##0.00 ден', 'fiat.emerging', 'mk', 2, true, 'ACT/360', 100, null),
        ('ALL', 'Albanian Lek', '008', 'L', 'q', 100, 'Closest', 2, 'L #,##0.00', 'fiat.emerging', 'al', 2, true, 'ACT/360', 100, null),
        ('MDL', 'Moldovan Leu', '498', 'L', 'b', 100, 'Closest', 2, '#,##0.00 L', 'fiat.emerging', 'md', 2, true, 'ACT/360', 100, null),
        ('UAH', 'Ukrainian Hryvnia', '980', '₴', 'к', 100, 'Closest', 2, '#,##0.00 ₴', 'fiat.emerging', 'ua', 2, true, 'ACT/360', 100, null),
        ('BYN', 'Belarusian Ruble', '933', 'Br', 'к', 100, 'Closest', 2, 'Br #,##0.00', 'fiat.emerging', 'by', 2, true, 'ACT/360', 100, null),
        ('RUB', 'Russian Ruble', '643', '₽', 'к', 100, 'Closest', 2, '#,##0.00 ₽', 'fiat.emerging', 'ru', 2, false, 'ACT/360', 100, null),
        ('GEL', 'Georgian Lari', '981', '₾', 'თ', 100, 'Closest', 2, '#,##0.00 ₾', 'fiat.emerging', 'ge', 2, true, 'ACT/360', 100, null),
        ('AMD', 'Armenian Dram', '051', '֏', 'լ', 100, 'Closest', 2, '#,##0.00 ֏', 'fiat.emerging', 'am', 2, true, 'ACT/360', 100, null),
        ('AZN', 'Azerbaijani Manat', '944', '₼', 'q', 100, 'Closest', 2, '#,##0.00 ₼', 'fiat.emerging', 'az', 2, true, 'ACT/360', 100, null),
        ('TRY', 'Turkish Lira', '949', '₺', 'kr', 100, 'Closest', 2, '₺#,##0.00', 'fiat.emerging', 'tr', 2, true, 'ACT/360', 100, null),
        ('GIP', 'Gibraltar Pound', '292', '£', 'p', 100, 'Closest', 2, '£#,##0.00', 'fiat.emerging', 'gi', 2, true, 'ACT/365', 100, null),
        -- Asia-Pacific
        ('JPY', 'Japanese Yen', '392', '¥', '', 0, 'Closest', 0, '¥#,##0', 'fiat.g10', 'jp', 2, true, 'ACT/360', 8, 'Japan'),
        ('CNY', 'Chinese Yuan', '156', '¥', '分', 100, 'Closest', 2, '¥#,##0.00', 'fiat.emerging', 'cn', 2, false, 'ACT/360', 100, 'China'),
        ('HKD', 'Hong Kong Dollar', '344', 'HK$', '¢', 100, 'Closest', 2, 'HK$#,##0.00', 'fiat.emerging', 'hk', 2, true, 'ACT/365', 100, 'HongKong'),
        ('TWD', 'New Taiwan Dollar', '901', 'NT$', '¢', 100, 'Closest', 2, 'NT$#,##0.00', 'fiat.emerging', 'tw', 2, false, 'ACT/360', 100, null),
        ('KRW', 'South Korean Won', '410', '₩', '', 0, 'Closest', 0, '₩#,##0', 'fiat.emerging', 'kr', 2, false, 'ACT/360', 100, 'SouthKorea'),
        ('KPW', 'North Korean Won', '408', '₩', '전', 100, 'Closest', 2, '₩#,##0.00', 'fiat.emerging', 'kp', 2, true, 'ACT/360', 100, null),
        ('SGD', 'Singapore Dollar', '702', 'S$', '¢', 100, 'Closest', 2, 'S$#,##0.00', 'fiat.emerging', 'sg', 2, true, 'ACT/365', 100, 'Singapore'),
        ('MYR', 'Malaysian Ringgit', '458', 'RM', 'sen', 100, 'Closest', 2, 'RM#,##0.00', 'fiat.emerging', 'my', 2, false, 'ACT/365', 100, null),
        ('IDR', 'Indonesian Rupiah', '360', 'Rp', 'sen', 100, 'Closest', 2, 'Rp#,##0.00', 'fiat.emerging', 'id', 2, false, 'ACT/360', 100, null),
        ('THB', 'Thai Baht', '764', '฿', 'ส', 100, 'Closest', 2, '฿#,##0.00', 'fiat.emerging', 'th', 2, true, 'ACT/360', 100, null),
        ('VND', 'Vietnamese Dong', '704', '₫', '', 0, 'Closest', 0, '#,##0 ₫', 'fiat.emerging', 'vn', 2, false, 'ACT/360', 100, null),
        ('PHP', 'Philippine Peso', '608', '₱', '¢', 100, 'Closest', 2, '₱#,##0.00', 'fiat.emerging', 'ph', 2, false, 'ACT/360', 100, null),
        ('INR', 'Indian Rupee', '356', '₹', 'p', 100, 'Closest', 2, '₹#,##0.00', 'fiat.emerging', 'in', 2, false, 'ACT/365', 100, 'India'),
        ('PKR', 'Pakistani Rupee', '586', 'Rs', 'p', 100, 'Closest', 2, 'Rs#,##0.00', 'fiat.emerging', 'pk', 2, false, 'ACT/365', 100, null),
        ('BDT', 'Bangladeshi Taka', '050', '৳', 'p', 100, 'Closest', 2, '৳#,##0.00', 'fiat.emerging', 'bd', 2, true, 'ACT/360', 100, null),
        ('LKR', 'Sri Lankan Rupee', '144', 'Rs', '¢', 100, 'Closest', 2, 'Rs#,##0.00', 'fiat.emerging', 'lk', 2, true, 'ACT/360', 100, null),
        ('NPR', 'Nepalese Rupee', '524', 'Rs', 'p', 100, 'Closest', 2, 'Rs#,##0.00', 'fiat.emerging', 'np', 2, true, 'ACT/360', 100, null),
        ('MMK', 'Myanmar Kyat', '104', 'K', 'p', 100, 'Closest', 2, 'K#,##0.00', 'fiat.emerging', 'mm', 2, true, 'ACT/360', 100, null),
        ('KHR', 'Cambodian Riel', '116', '៛', 'សេន', 100, 'Closest', 2, '៛#,##0.00', 'fiat.emerging', 'kh', 2, true, 'ACT/360', 100, null),
        ('LAK', 'Lao Kip', '418', '₭', 'ອັດ', 100, 'Closest', 2, '₭#,##0.00', 'fiat.emerging', 'la', 2, true, 'ACT/360', 100, null),
        ('MNT', 'Mongolian Tugrik', '496', '₮', 'м', 100, 'Closest', 2, '₮#,##0.00', 'fiat.emerging', 'mn', 2, true, 'ACT/360', 100, null),
        ('KZT', 'Kazakhstani Tenge', '398', '₸', 'т', 100, 'Closest', 2, '₸#,##0.00', 'fiat.emerging', 'kz', 2, false, 'ACT/360', 100, null),
        ('UZS', 'Uzbekistani Som', '860', 'сўм', 'т', 100, 'Closest', 2, '#,##0.00 сўм', 'fiat.emerging', 'uz', 2, true, 'ACT/360', 100, null),
        ('KGS', 'Kyrgyzstani Som', '417', 'с', 'т', 100, 'Closest', 2, '#,##0.00 с', 'fiat.emerging', 'kg', 2, true, 'ACT/360', 100, null),
        ('TJS', 'Tajikistani Somoni', '972', 'ЅМ', 'д', 100, 'Closest', 2, '#,##0.00 ЅМ', 'fiat.emerging', 'tj', 2, true, 'ACT/360', 100, null),
        ('TMT', 'Turkmenistani Manat', '934', 'm', 't', 100, 'Closest', 2, '#,##0.00 m', 'fiat.emerging', 'tm', 2, true, 'ACT/360', 100, null),
        ('AFN', 'Afghan Afghani', '971', '؋', 'پ', 100, 'Closest', 2, '؋#,##0.00', 'fiat.emerging', 'af', 2, true, 'ACT/360', 100, null),
        ('AUD', 'Australian Dollar', '036', 'A$', '¢', 100, 'Closest', 2, 'A$#,##0.00', 'fiat.g10', 'au', 2, true, 'ACT/365', 3, 'Australia'),
        ('NZD', 'New Zealand Dollar', '554', 'NZ$', '¢', 100, 'Closest', 2, 'NZ$#,##0.00', 'fiat.g10', 'nz', 2, true, 'ACT/365', 4, 'NewZealand'),
        ('FJD', 'Fijian Dollar', '242', 'FJ$', '¢', 100, 'Closest', 2, 'FJ$#,##0.00', 'fiat.emerging', 'fj', 2, true, 'ACT/365', 100, null),
        ('PGK', 'Papua New Guinean Kina', '598', 'K', 't', 100, 'Closest', 2, 'K#,##0.00', 'fiat.emerging', 'pg', 2, true, 'ACT/365', 100, null),
        ('SBD', 'Solomon Islands Dollar', '090', 'SI$', '¢', 100, 'Closest', 2, 'SI$#,##0.00', 'fiat.emerging', 'sb', 2, true, 'ACT/365', 100, null),
        ('VUV', 'Vanuatu Vatu', '548', 'VT', '', 0, 'Closest', 0, 'VT#,##0', 'fiat.emerging', 'vu', 2, true, 'ACT/365', 100, null),
        ('WST', 'Samoan Tala', '882', 'WS$', 's', 100, 'Closest', 2, 'WS$#,##0.00', 'fiat.emerging', 'ws', 2, true, 'ACT/365', 100, null),
        ('TOP', 'Tongan Paanga', '776', 'T$', 's', 100, 'Closest', 2, 'T$#,##0.00', 'fiat.emerging', 'to', 2, true, 'ACT/365', 100, null),
        ('MOP', 'Macanese Pataca', '446', 'MOP$', 'a', 100, 'Closest', 2, 'MOP$#,##0.00', 'fiat.emerging', 'mo', 2, true, 'ACT/360', 100, null),
        ('BND', 'Brunei Dollar', '096', 'B$', 'sen', 100, 'Closest', 2, 'B$#,##0.00', 'fiat.emerging', 'bn', 2, true, 'ACT/365', 100, null),
        ('BTN', 'Bhutanese Ngultrum', '064', 'Nu.', 'ch', 100, 'Closest', 2, 'Nu.#,##0.00', 'fiat.emerging', 'bt', 2, true, 'ACT/365', 100, null),
        ('MVR', 'Maldivian Rufiyaa', '462', 'Rf', 'l', 100, 'Closest', 2, 'Rf#,##0.00', 'fiat.emerging', 'mv', 2, true, 'ACT/360', 100, null),
        -- Middle East
        ('SAR', 'Saudi Riyal', '682', '﷼', 'h', 100, 'Closest', 2, '﷼#,##0.00', 'fiat.emerging', 'sa', 2, true, 'ACT/360', 100, null),
        ('AED', 'UAE Dirham', '784', 'د.إ', 'ف', 100, 'Closest', 2, 'د.إ#,##0.00', 'fiat.emerging', 'ae', 2, true, 'ACT/360', 100, null),
        ('QAR', 'Qatari Riyal', '634', '﷼', 'd', 100, 'Closest', 2, '﷼#,##0.00', 'fiat.emerging', 'qa', 2, true, 'ACT/360', 100, null),
        ('KWD', 'Kuwaiti Dinar', '414', 'د.ك', 'ف', 1000, 'Closest', 3, 'د.ك#,##0.000', 'fiat.emerging', 'kw', 2, true, 'ACT/360', 100, null),
        ('BHD', 'Bahraini Dinar', '048', 'BD', 'f', 1000, 'Closest', 3, 'BD#,##0.000', 'fiat.emerging', 'bh', 2, true, 'ACT/360', 100, null),
        ('OMR', 'Omani Rial', '512', '﷼', 'b', 1000, 'Closest', 3, '﷼#,##0.000', 'fiat.emerging', 'om', 2, true, 'ACT/360', 100, null),
        ('JOD', 'Jordanian Dinar', '400', 'د.ا', 'ف', 1000, 'Closest', 3, 'د.ا#,##0.000', 'fiat.emerging', 'jo', 2, true, 'ACT/360', 100, null),
        ('ILS', 'Israeli New Shekel', '376', '₪', 'a', 100, 'Closest', 2, '₪#,##0.00', 'fiat.emerging', 'il', 2, true, 'ACT/360', 100, null),
        ('LBP', 'Lebanese Pound', '422', 'ل.ل', 'ق', 100, 'Closest', 2, 'ل.ل#,##0.00', 'fiat.emerging', 'lb', 2, true, 'ACT/360', 100, null),
        ('SYP', 'Syrian Pound', '760', '£', 'ق', 100, 'Closest', 2, '£#,##0.00', 'fiat.emerging', 'sy', 2, true, 'ACT/360', 100, null),
        ('IQD', 'Iraqi Dinar', '368', 'ع.د', 'ف', 1000, 'Closest', 3, 'ع.د#,##0.000', 'fiat.emerging', 'iq', 2, true, 'ACT/360', 100, null),
        ('IRR', 'Iranian Rial', '364', '﷼', 'd', 100, 'Closest', 2, '﷼#,##0.00', 'fiat.emerging', 'ir', 2, true, 'ACT/360', 100, null),
        ('YER', 'Yemeni Rial', '886', '﷼', 'f', 100, 'Closest', 2, '﷼#,##0.00', 'fiat.emerging', 'ye', 2, true, 'ACT/360', 100, null),
        -- Africa
        ('ZAR', 'South African Rand', '710', 'R', 'c', 100, 'Closest', 2, 'R#,##0.00', 'fiat.emerging', 'za', 2, true, 'ACT/365', 100, 'SouthAfrica'),
        ('EGP', 'Egyptian Pound', '818', '£', 'pt', 100, 'Closest', 2, '£#,##0.00', 'fiat.emerging', 'eg', 2, false, 'ACT/360', 100, null),
        ('NGN', 'Nigerian Naira', '566', '₦', 'k', 100, 'Closest', 2, '₦#,##0.00', 'fiat.emerging', 'ng', 2, false, 'ACT/360', 100, null),
        ('KES', 'Kenyan Shilling', '404', 'KSh', 'c', 100, 'Closest', 2, 'KSh#,##0.00', 'fiat.emerging', 'ke', 2, true, 'ACT/360', 100, null),
        ('GHS', 'Ghanaian Cedi', '936', 'GH₵', 'p', 100, 'Closest', 2, 'GH₵#,##0.00', 'fiat.emerging', 'gh', 2, true, 'ACT/360', 100, null),
        ('TZS', 'Tanzanian Shilling', '834', 'TSh', 'c', 100, 'Closest', 2, 'TSh#,##0.00', 'fiat.emerging', 'tz', 2, true, 'ACT/360', 100, null),
        ('UGX', 'Ugandan Shilling', '800', 'USh', '', 0, 'Closest', 0, 'USh#,##0', 'fiat.emerging', 'ug', 2, true, 'ACT/360', 100, null),
        ('ETB', 'Ethiopian Birr', '230', 'Br', 's', 100, 'Closest', 2, 'Br#,##0.00', 'fiat.emerging', 'et', 2, true, 'ACT/360', 100, null),
        ('MAD', 'Moroccan Dirham', '504', 'د.م.', 's', 100, 'Closest', 2, 'د.م.#,##0.00', 'fiat.emerging', 'ma', 2, true, 'ACT/360', 100, null),
        ('TND', 'Tunisian Dinar', '788', 'د.ت', 'm', 1000, 'Closest', 3, 'د.ت#,##0.000', 'fiat.emerging', 'tn', 2, true, 'ACT/360', 100, null),
        ('DZD', 'Algerian Dinar', '012', 'د.ج', 's', 100, 'Closest', 2, 'د.ج#,##0.00', 'fiat.emerging', 'dz', 2, true, 'ACT/360', 100, null),
        ('LYD', 'Libyan Dinar', '434', 'ل.د', 'd', 1000, 'Closest', 3, 'ل.د#,##0.000', 'fiat.emerging', 'ly', 2, true, 'ACT/360', 100, null),
        ('SDG', 'Sudanese Pound', '938', 'ج.س', 'q', 100, 'Closest', 2, 'ج.س#,##0.00', 'fiat.emerging', 'sd', 2, true, 'ACT/360', 100, null),
        ('SSP', 'South Sudanese Pound', '728', '£', 'p', 100, 'Closest', 2, '£#,##0.00', 'fiat.emerging', 'ss', 2, true, 'ACT/360', 100, null),
        ('AOA', 'Angolan Kwanza', '973', 'Kz', 'c', 100, 'Closest', 2, 'Kz#,##0.00', 'fiat.emerging', 'ao', 2, true, 'ACT/360', 100, null),
        ('XAF', 'Central African CFA Franc', '950', 'FCFA', 'c', 100, 'Closest', 2, 'FCFA#,##0.00', 'fiat.emerging', 'cm', 2, true, 'ACT/360', 100, null),
        ('XOF', 'West African CFA Franc', '952', 'CFA', 'c', 100, 'Closest', 2, 'CFA#,##0.00', 'fiat.emerging', 'sn', 2, true, 'ACT/360', 100, null),
        ('ZMW', 'Zambian Kwacha', '967', 'ZK', 'n', 100, 'Closest', 2, 'ZK#,##0.00', 'fiat.emerging', 'zm', 2, true, 'ACT/360', 100, null),
        ('MZN', 'Mozambican Metical', '943', 'MT', 'c', 100, 'Closest', 2, 'MT#,##0.00', 'fiat.emerging', 'mz', 2, true, 'ACT/360', 100, null),
        ('BWP', 'Botswana Pula', '072', 'P', 't', 100, 'Closest', 2, 'P#,##0.00', 'fiat.emerging', 'bw', 2, true, 'ACT/360', 100, null),
        ('NAD', 'Namibian Dollar', '516', '$', 'c', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'na', 2, true, 'ACT/365', 100, null),
        ('SZL', 'Swazi Lilangeni', '748', 'L', 'c', 100, 'Closest', 2, 'L#,##0.00', 'fiat.emerging', 'sz', 2, true, 'ACT/365', 100, null),
        ('LSL', 'Lesotho Loti', '426', 'L', 's', 100, 'Closest', 2, 'L#,##0.00', 'fiat.emerging', 'ls', 2, true, 'ACT/365', 100, null),
        ('MWK', 'Malawian Kwacha', '454', 'MK', 't', 100, 'Closest', 2, 'MK#,##0.00', 'fiat.emerging', 'mw', 2, true, 'ACT/360', 100, null),
        ('ZWL', 'Zimbabwean Dollar', '932', '$', 'c', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'zw', 2, false, 'ACT/360', 100, null),
        ('RWF', 'Rwandan Franc', '646', 'FRw', 'c', 100, 'Closest', 2, 'FRw#,##0.00', 'fiat.emerging', 'rw', 2, true, 'ACT/360', 100, null),
        ('BIF', 'Burundian Franc', '108', 'FBu', 'c', 100, 'Closest', 2, 'FBu#,##0.00', 'fiat.emerging', 'bi', 2, true, 'ACT/360', 100, null),
        ('CDF', 'Congolese Franc', '976', 'FC', 'c', 100, 'Closest', 2, 'FC#,##0.00', 'fiat.emerging', 'cd', 2, true, 'ACT/360', 100, null),
        ('DJF', 'Djiboutian Franc', '262', 'Fdj', 'c', 100, 'Closest', 2, 'Fdj#,##0.00', 'fiat.emerging', 'dj', 2, true, 'ACT/360', 100, null),
        ('ERN', 'Eritrean Nakfa', '232', 'Nfk', 'c', 100, 'Closest', 2, 'Nfk#,##0.00', 'fiat.emerging', 'er', 2, true, 'ACT/360', 100, null),
        ('SOS', 'Somali Shilling', '706', 'Sh', 'c', 100, 'Closest', 2, 'Sh#,##0.00', 'fiat.emerging', 'so', 2, true, 'ACT/360', 100, null),
        ('GMD', 'Gambian Dalasi', '270', 'D', 'b', 100, 'Closest', 2, 'D#,##0.00', 'fiat.emerging', 'gm', 2, true, 'ACT/360', 100, null),
        ('GNF', 'Guinean Franc', '324', 'FG', 'c', 100, 'Closest', 2, 'FG#,##0.00', 'fiat.emerging', 'gn', 2, true, 'ACT/360', 100, null),
        ('SLL', 'Sierra Leonean Leone', '694', 'Le', 'c', 100, 'Closest', 2, 'Le#,##0.00', 'fiat.emerging', 'sl', 2, true, 'ACT/360', 100, null),
        ('LRD', 'Liberian Dollar', '430', '$', 'c', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'lr', 2, true, 'ACT/360', 100, null),
        ('CVE', 'Cape Verdean Escudo', '132', '$', 'c', 100, 'Closest', 2, '$#,##0.00', 'fiat.emerging', 'cv', 2, true, 'ACT/360', 100, null),
        ('STN', 'Sao Tome and Principe Dobra', '930', 'Db', 'c', 100, 'Closest', 2, 'Db#,##0.00', 'fiat.emerging', 'st', 2, true, 'ACT/360', 100, null),
        ('MRU', 'Mauritanian Ouguiya', '929', 'UM', 'k', 5, 'Closest', 1, 'UM#,##0.0', 'fiat.emerging', 'mr', 2, true, 'ACT/360', 100, null),
        ('MGA', 'Malagasy Ariary', '969', 'Ar', 'ir', 5, 'Closest', 1, 'Ar#,##0.0', 'fiat.emerging', 'mg', 2, true, 'ACT/360', 100, null),
        ('MUR', 'Mauritian Rupee', '480', '₨', 'c', 100, 'Closest', 2, '₨#,##0.00', 'fiat.emerging', 'mu', 2, true, 'ACT/360', 100, null),
        ('SCR', 'Seychellois Rupee', '690', '₨', 'c', 100, 'Closest', 2, '₨#,##0.00', 'fiat.emerging', 'sc', 2, true, 'ACT/360', 100, null),
        ('KMF', 'Comorian Franc', '174', 'CF', 'c', 100, 'Closest', 2, 'CF#,##0.00', 'fiat.emerging', 'km', 2, true, 'ACT/360', 100, null),
        -- Precious Metals (commodity currencies)
        ('XAU', 'Gold (troy ounce)', '959', 'Au', '', 0, 'Down', 6, '#,##0.000000', 'commodity', 'xau', 2, true, 'ACT/360', 100, null),
        ('XAG', 'Silver (troy ounce)', '961', 'Ag', '', 0, 'Down', 6, '#,##0.000000', 'commodity', 'xag', 2, true, 'ACT/360', 100, null),
        ('XPT', 'Platinum (troy ounce)', '962', 'Pt', '', 0, 'Down', 6, '#,##0.000000', 'commodity', 'xpt', 2, true, 'ACT/360', 100, null),
        ('XPD', 'Palladium (troy ounce)', '964', 'Pd', '', 0, 'Down', 6, '#,##0.000000', 'commodity', 'xpd', 2, true, 'ACT/360', 100, null),
        -- Supranational
        ('XDR', 'Special Drawing Rights', '960', 'SDR', '', 0, 'Closest', 6, 'SDR #,##0.000000', 'supranational', 'xdr', 2, true, 'ACT/360', 100, null)
    ) as c(iso_code, name, numeric_code, symbol, fraction_symbol, fractions_per_unit, rounding_type, rounding_precision, format, currency_type, flag_key,
           spot_days, deliverable, day_basis, base_precedence, holiday_calendar)
    left join ores_dq_images_artefact_tbl i
        on i.dataset_id = v_flags_dataset_id
        and i.key = c.flag_key;

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % currencies for dataset: ISO 4217 Currency Codes', v_count;

    -- Report currencies using placeholder flag
    raise debug 'Currencies using placeholder flag (xx):';
    perform iso_code
    from ores_dq_currencies_artefact_tbl
    where dataset_id = v_currencies_dataset_id
      and image_id = v_placeholder_image_id;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Currencies Summary ---'

select 'Total DQ Currencies' as metric, count(*) as count
from ores_dq_currencies_artefact_tbl
union all
select 'G10 Fiat Currencies (fiat.g10)', count(*)
from ores_dq_currencies_artefact_tbl
where monetary_nature = 'fiat' and market_tier = 'g10'
union all
select 'Emerging Fiat Currencies (fiat.emerging)', count(*)
from ores_dq_currencies_artefact_tbl
where monetary_nature = 'fiat' and market_tier = 'emerging'
union all
select 'Commodity Currencies (commodity)', count(*)
from ores_dq_currencies_artefact_tbl
where monetary_nature = 'commodity'
union all
select 'Supranational Currencies (supranational)', count(*)
from ores_dq_currencies_artefact_tbl
where monetary_nature = 'supranational'
union all
select 'Currencies with Placeholder Flag', count(*)
from ores_dq_currencies_artefact_tbl c
join ores_dq_images_artefact_tbl i on c.image_id = i.image_id
where i.key = 'xx';
