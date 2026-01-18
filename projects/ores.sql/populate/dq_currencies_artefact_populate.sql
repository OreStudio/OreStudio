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

set schema 'ores';

DO $$
declare
    v_currencies_dataset_id uuid;
    v_flags_dataset_id uuid;
    v_placeholder_image_id uuid;
    v_count integer := 0;
begin
    -- Get the currencies dataset ID
    select id into v_currencies_dataset_id
    from ores.dq_datasets_tbl
    where name = 'ISO 4217 Currencies from Wikipedia'
      and subject_area_name = 'Currencies'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_currencies_dataset_id is null then
        raise exception 'Dataset not found: ISO 4217 Currencies from Wikipedia';
    end if;

    -- Get the flags dataset ID (for linking images)
    select id into v_flags_dataset_id
    from ores.dq_datasets_tbl
    where name = 'Country Flags from lipis/flag-icons'
      and subject_area_name = 'Countries'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_flags_dataset_id is null then
        raise exception 'Dataset not found: Country Flags from lipis/flag-icons';
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
    where dataset_id = v_currencies_dataset_id;

    raise notice 'Populating currencies for dataset: ISO 4217 Currencies from Wikipedia';

    -- Insert currencies with flag image links
    -- Currency-to-flag mapping: each currency maps to its issuing country's flag
    -- Special cases: EUR -> 'eu', XAU -> 'xau', XDR -> 'xdr', etc.
    insert into ores.dq_currencies_artefact_tbl (
        dataset_id, iso_code, version, name, numeric_code, symbol, fraction_symbol,
        fractions_per_unit, rounding_type, rounding_precision, format, currency_type, image_id
    )
    select
        v_currencies_dataset_id,
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
        -- Americas
        ('USD', 'US Dollar', '840', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.major', 'us'),
        ('CAD', 'Canadian Dollar', '124', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.major', 'ca'),
        ('MXN', 'Mexican Peso', '484', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'mx'),
        ('BRL', 'Brazilian Real', '986', 'R$', '¢', 100, 'standard', 2, 'R$#,##0.00', 'fiat.emerging', 'br'),
        ('ARS', 'Argentine Peso', '032', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'ar'),
        ('CLP', 'Chilean Peso', '152', '$', '', 0, 'standard', 0, '$#,##0', 'fiat.emerging', 'cl'),
        ('COP', 'Colombian Peso', '170', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'co'),
        ('PEN', 'Peruvian Sol', '604', 'S/', '¢', 100, 'standard', 2, 'S/#,##0.00', 'fiat.emerging', 'pe'),
        ('UYU', 'Uruguayan Peso', '858', '$U', '¢', 100, 'standard', 2, '$U#,##0.00', 'fiat.emerging', 'uy'),
        ('PYG', 'Paraguayan Guarani', '600', '₲', '', 0, 'standard', 0, '₲#,##0', 'fiat.emerging', 'py'),
        ('BOB', 'Bolivian Boliviano', '068', 'Bs', '¢', 100, 'standard', 2, 'Bs#,##0.00', 'fiat.emerging', 'bo'),
        ('VES', 'Venezuelan Bolivar', '928', 'Bs', '¢', 100, 'standard', 2, 'Bs#,##0.00', 'fiat.emerging', 've'),
        ('CRC', 'Costa Rican Colon', '188', '₡', '¢', 100, 'standard', 2, '₡#,##0.00', 'fiat.emerging', 'cr'),
        ('PAB', 'Panamanian Balboa', '590', 'B/.', '¢', 100, 'standard', 2, 'B/.#,##0.00', 'fiat.emerging', 'pa'),
        ('GTQ', 'Guatemalan Quetzal', '320', 'Q', '¢', 100, 'standard', 2, 'Q#,##0.00', 'fiat.emerging', 'gt'),
        ('HNL', 'Honduran Lempira', '340', 'L', '¢', 100, 'standard', 2, 'L#,##0.00', 'fiat.emerging', 'hn'),
        ('NIO', 'Nicaraguan Cordoba', '558', 'C$', '¢', 100, 'standard', 2, 'C$#,##0.00', 'fiat.emerging', 'ni'),
        ('DOP', 'Dominican Peso', '214', 'RD$', '¢', 100, 'standard', 2, 'RD$#,##0.00', 'fiat.emerging', 'do'),
        ('CUP', 'Cuban Peso', '192', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'cu'),
        ('JMD', 'Jamaican Dollar', '388', 'J$', '¢', 100, 'standard', 2, 'J$#,##0.00', 'fiat.emerging', 'jm'),
        ('TTD', 'Trinidad and Tobago Dollar', '780', 'TT$', '¢', 100, 'standard', 2, 'TT$#,##0.00', 'fiat.emerging', 'tt'),
        ('BBD', 'Barbadian Dollar', '052', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'bb'),
        ('BSD', 'Bahamian Dollar', '044', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'bs'),
        ('HTG', 'Haitian Gourde', '332', 'G', '¢', 100, 'standard', 2, 'G#,##0.00', 'fiat.emerging', 'ht'),
        ('SRD', 'Surinamese Dollar', '968', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'sr'),
        ('GYD', 'Guyanese Dollar', '328', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'gy'),
        ('BZD', 'Belize Dollar', '084', 'BZ$', '¢', 100, 'standard', 2, 'BZ$#,##0.00', 'fiat.emerging', 'bz'),
        ('AWG', 'Aruban Florin', '533', 'ƒ', '¢', 100, 'standard', 2, 'ƒ#,##0.00', 'fiat.emerging', 'aw'),
        ('ANG', 'Netherlands Antillean Guilder', '532', 'ƒ', '¢', 100, 'standard', 2, 'ƒ#,##0.00', 'fiat.emerging', 'cw'),
        ('XCD', 'East Caribbean Dollar', '951', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'ag'),
        ('KYD', 'Cayman Islands Dollar', '136', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'ky'),
        ('BMD', 'Bermudian Dollar', '060', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'bm'),
        ('FKP', 'Falkland Islands Pound', '238', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat.emerging', 'fk'),
        -- Europe
        ('EUR', 'Euro', '978', '€', 'c', 100, 'standard', 2, '€#,##0.00', 'fiat.major', 'eu'),
        ('GBP', 'British Pound Sterling', '826', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat.major', 'gb'),
        ('CHF', 'Swiss Franc', '756', 'CHF', 'c', 100, 'swedish', 2, 'CHF #,##0.00', 'fiat.major', 'ch'),
        ('NOK', 'Norwegian Krone', '578', 'kr', 'ø', 100, 'standard', 2, 'kr #,##0.00', 'fiat.major', 'no'),
        ('SEK', 'Swedish Krona', '752', 'kr', 'ö', 100, 'standard', 2, '#,##0.00 kr', 'fiat.major', 'se'),
        ('DKK', 'Danish Krone', '208', 'kr', 'ø', 100, 'standard', 2, 'kr #,##0.00', 'fiat.major', 'dk'),
        ('ISK', 'Icelandic Krona', '352', 'kr', '', 0, 'standard', 0, '#,##0 kr', 'fiat.emerging', 'is'),
        ('PLN', 'Polish Zloty', '985', 'zł', 'gr', 100, 'standard', 2, '#,##0.00 zł', 'fiat.emerging', 'pl'),
        ('CZK', 'Czech Koruna', '203', 'Kč', 'h', 100, 'standard', 2, '#,##0.00 Kč', 'fiat.emerging', 'cz'),
        ('HUF', 'Hungarian Forint', '348', 'Ft', 'f', 100, 'standard', 2, '#,##0.00 Ft', 'fiat.emerging', 'hu'),
        ('RON', 'Romanian Leu', '946', 'lei', 'b', 100, 'standard', 2, '#,##0.00 lei', 'fiat.emerging', 'ro'),
        ('BGN', 'Bulgarian Lev', '975', 'лв', 'ст', 100, 'standard', 2, '#,##0.00 лв', 'fiat.emerging', 'bg'),
        ('HRK', 'Croatian Kuna', '191', 'kn', 'lp', 100, 'standard', 2, '#,##0.00 kn', 'fiat.emerging', 'hr'),
        ('RSD', 'Serbian Dinar', '941', 'дин', 'п', 100, 'standard', 2, '#,##0.00 дин', 'fiat.emerging', 'rs'),
        ('BAM', 'Bosnia and Herzegovina Convertible Mark', '977', 'KM', 'pf', 100, 'standard', 2, '#,##0.00 KM', 'fiat.emerging', 'ba'),
        ('MKD', 'Macedonian Denar', '807', 'ден', 'д', 100, 'standard', 2, '#,##0.00 ден', 'fiat.emerging', 'mk'),
        ('ALL', 'Albanian Lek', '008', 'L', 'q', 100, 'standard', 2, 'L #,##0.00', 'fiat.emerging', 'al'),
        ('MDL', 'Moldovan Leu', '498', 'L', 'b', 100, 'standard', 2, '#,##0.00 L', 'fiat.emerging', 'md'),
        ('UAH', 'Ukrainian Hryvnia', '980', '₴', 'к', 100, 'standard', 2, '#,##0.00 ₴', 'fiat.emerging', 'ua'),
        ('BYN', 'Belarusian Ruble', '933', 'Br', 'к', 100, 'standard', 2, 'Br #,##0.00', 'fiat.emerging', 'by'),
        ('RUB', 'Russian Ruble', '643', '₽', 'к', 100, 'standard', 2, '#,##0.00 ₽', 'fiat.emerging', 'ru'),
        ('GEL', 'Georgian Lari', '981', '₾', 'თ', 100, 'standard', 2, '#,##0.00 ₾', 'fiat.emerging', 'ge'),
        ('AMD', 'Armenian Dram', '051', '֏', 'լ', 100, 'standard', 2, '#,##0.00 ֏', 'fiat.emerging', 'am'),
        ('AZN', 'Azerbaijani Manat', '944', '₼', 'q', 100, 'standard', 2, '#,##0.00 ₼', 'fiat.emerging', 'az'),
        ('TRY', 'Turkish Lira', '949', '₺', 'kr', 100, 'standard', 2, '₺#,##0.00', 'fiat.emerging', 'tr'),
        ('GIP', 'Gibraltar Pound', '292', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat.emerging', 'gi'),
        -- Asia-Pacific
        ('JPY', 'Japanese Yen', '392', '¥', '', 0, 'standard', 0, '¥#,##0', 'fiat.major', 'jp'),
        ('CNY', 'Chinese Yuan', '156', '¥', '分', 100, 'standard', 2, '¥#,##0.00', 'fiat.emerging', 'cn'),
        ('HKD', 'Hong Kong Dollar', '344', 'HK$', '¢', 100, 'standard', 2, 'HK$#,##0.00', 'fiat.emerging', 'hk'),
        ('TWD', 'New Taiwan Dollar', '901', 'NT$', '¢', 100, 'standard', 2, 'NT$#,##0.00', 'fiat.emerging', 'tw'),
        ('KRW', 'South Korean Won', '410', '₩', '', 0, 'standard', 0, '₩#,##0', 'fiat.emerging', 'kr'),
        ('KPW', 'North Korean Won', '408', '₩', '전', 100, 'standard', 2, '₩#,##0.00', 'fiat.emerging', 'kp'),
        ('SGD', 'Singapore Dollar', '702', 'S$', '¢', 100, 'standard', 2, 'S$#,##0.00', 'fiat.emerging', 'sg'),
        ('MYR', 'Malaysian Ringgit', '458', 'RM', 'sen', 100, 'standard', 2, 'RM#,##0.00', 'fiat.emerging', 'my'),
        ('IDR', 'Indonesian Rupiah', '360', 'Rp', 'sen', 100, 'standard', 2, 'Rp#,##0.00', 'fiat.emerging', 'id'),
        ('THB', 'Thai Baht', '764', '฿', 'ส', 100, 'standard', 2, '฿#,##0.00', 'fiat.emerging', 'th'),
        ('VND', 'Vietnamese Dong', '704', '₫', '', 0, 'standard', 0, '#,##0 ₫', 'fiat.emerging', 'vn'),
        ('PHP', 'Philippine Peso', '608', '₱', '¢', 100, 'standard', 2, '₱#,##0.00', 'fiat.emerging', 'ph'),
        ('INR', 'Indian Rupee', '356', '₹', 'p', 100, 'standard', 2, '₹#,##0.00', 'fiat.emerging', 'in'),
        ('PKR', 'Pakistani Rupee', '586', 'Rs', 'p', 100, 'standard', 2, 'Rs#,##0.00', 'fiat.emerging', 'pk'),
        ('BDT', 'Bangladeshi Taka', '050', '৳', 'p', 100, 'standard', 2, '৳#,##0.00', 'fiat.emerging', 'bd'),
        ('LKR', 'Sri Lankan Rupee', '144', 'Rs', '¢', 100, 'standard', 2, 'Rs#,##0.00', 'fiat.emerging', 'lk'),
        ('NPR', 'Nepalese Rupee', '524', 'Rs', 'p', 100, 'standard', 2, 'Rs#,##0.00', 'fiat.emerging', 'np'),
        ('MMK', 'Myanmar Kyat', '104', 'K', 'p', 100, 'standard', 2, 'K#,##0.00', 'fiat.emerging', 'mm'),
        ('KHR', 'Cambodian Riel', '116', '៛', 'សេន', 100, 'standard', 2, '៛#,##0.00', 'fiat.emerging', 'kh'),
        ('LAK', 'Lao Kip', '418', '₭', 'ອັດ', 100, 'standard', 2, '₭#,##0.00', 'fiat.emerging', 'la'),
        ('MNT', 'Mongolian Tugrik', '496', '₮', 'м', 100, 'standard', 2, '₮#,##0.00', 'fiat.emerging', 'mn'),
        ('KZT', 'Kazakhstani Tenge', '398', '₸', 'т', 100, 'standard', 2, '₸#,##0.00', 'fiat.emerging', 'kz'),
        ('UZS', 'Uzbekistani Som', '860', 'сўм', 'т', 100, 'standard', 2, '#,##0.00 сўм', 'fiat.emerging', 'uz'),
        ('KGS', 'Kyrgyzstani Som', '417', 'с', 'т', 100, 'standard', 2, '#,##0.00 с', 'fiat.emerging', 'kg'),
        ('TJS', 'Tajikistani Somoni', '972', 'ЅМ', 'д', 100, 'standard', 2, '#,##0.00 ЅМ', 'fiat.emerging', 'tj'),
        ('TMT', 'Turkmenistani Manat', '934', 'm', 't', 100, 'standard', 2, '#,##0.00 m', 'fiat.emerging', 'tm'),
        ('AFN', 'Afghan Afghani', '971', '؋', 'پ', 100, 'standard', 2, '؋#,##0.00', 'fiat.emerging', 'af'),
        ('AUD', 'Australian Dollar', '036', 'A$', '¢', 100, 'standard', 2, 'A$#,##0.00', 'fiat.major', 'au'),
        ('NZD', 'New Zealand Dollar', '554', 'NZ$', '¢', 100, 'standard', 2, 'NZ$#,##0.00', 'fiat.major', 'nz'),
        ('FJD', 'Fijian Dollar', '242', 'FJ$', '¢', 100, 'standard', 2, 'FJ$#,##0.00', 'fiat.emerging', 'fj'),
        ('PGK', 'Papua New Guinean Kina', '598', 'K', 't', 100, 'standard', 2, 'K#,##0.00', 'fiat.emerging', 'pg'),
        ('SBD', 'Solomon Islands Dollar', '090', 'SI$', '¢', 100, 'standard', 2, 'SI$#,##0.00', 'fiat.emerging', 'sb'),
        ('VUV', 'Vanuatu Vatu', '548', 'VT', '', 0, 'standard', 0, 'VT#,##0', 'fiat.emerging', 'vu'),
        ('WST', 'Samoan Tala', '882', 'WS$', 's', 100, 'standard', 2, 'WS$#,##0.00', 'fiat.emerging', 'ws'),
        ('TOP', 'Tongan Paanga', '776', 'T$', 's', 100, 'standard', 2, 'T$#,##0.00', 'fiat.emerging', 'to'),
        ('MOP', 'Macanese Pataca', '446', 'MOP$', 'a', 100, 'standard', 2, 'MOP$#,##0.00', 'fiat.emerging', 'mo'),
        ('BND', 'Brunei Dollar', '096', 'B$', 'sen', 100, 'standard', 2, 'B$#,##0.00', 'fiat.emerging', 'bn'),
        ('BTN', 'Bhutanese Ngultrum', '064', 'Nu.', 'ch', 100, 'standard', 2, 'Nu.#,##0.00', 'fiat.emerging', 'bt'),
        ('MVR', 'Maldivian Rufiyaa', '462', 'Rf', 'l', 100, 'standard', 2, 'Rf#,##0.00', 'fiat.emerging', 'mv'),
        -- Middle East
        ('SAR', 'Saudi Riyal', '682', '﷼', 'h', 100, 'standard', 2, '﷼#,##0.00', 'fiat.emerging', 'sa'),
        ('AED', 'UAE Dirham', '784', 'د.إ', 'ف', 100, 'standard', 2, 'د.إ#,##0.00', 'fiat.emerging', 'ae'),
        ('QAR', 'Qatari Riyal', '634', '﷼', 'd', 100, 'standard', 2, '﷼#,##0.00', 'fiat.emerging', 'qa'),
        ('KWD', 'Kuwaiti Dinar', '414', 'د.ك', 'ف', 1000, 'standard', 3, 'د.ك#,##0.000', 'fiat.emerging', 'kw'),
        ('BHD', 'Bahraini Dinar', '048', 'BD', 'f', 1000, 'standard', 3, 'BD#,##0.000', 'fiat.emerging', 'bh'),
        ('OMR', 'Omani Rial', '512', '﷼', 'b', 1000, 'standard', 3, '﷼#,##0.000', 'fiat.emerging', 'om'),
        ('JOD', 'Jordanian Dinar', '400', 'د.ا', 'ف', 1000, 'standard', 3, 'د.ا#,##0.000', 'fiat.emerging', 'jo'),
        ('ILS', 'Israeli New Shekel', '376', '₪', 'a', 100, 'standard', 2, '₪#,##0.00', 'fiat.emerging', 'il'),
        ('LBP', 'Lebanese Pound', '422', 'ل.ل', 'ق', 100, 'standard', 2, 'ل.ل#,##0.00', 'fiat.emerging', 'lb'),
        ('SYP', 'Syrian Pound', '760', '£', 'ق', 100, 'standard', 2, '£#,##0.00', 'fiat.emerging', 'sy'),
        ('IQD', 'Iraqi Dinar', '368', 'ع.د', 'ف', 1000, 'standard', 3, 'ع.د#,##0.000', 'fiat.emerging', 'iq'),
        ('IRR', 'Iranian Rial', '364', '﷼', 'd', 100, 'standard', 2, '﷼#,##0.00', 'fiat.emerging', 'ir'),
        ('YER', 'Yemeni Rial', '886', '﷼', 'f', 100, 'standard', 2, '﷼#,##0.00', 'fiat.emerging', 'ye'),
        -- Africa
        ('ZAR', 'South African Rand', '710', 'R', 'c', 100, 'standard', 2, 'R#,##0.00', 'fiat.emerging', 'za'),
        ('EGP', 'Egyptian Pound', '818', '£', 'pt', 100, 'standard', 2, '£#,##0.00', 'fiat.emerging', 'eg'),
        ('NGN', 'Nigerian Naira', '566', '₦', 'k', 100, 'standard', 2, '₦#,##0.00', 'fiat.emerging', 'ng'),
        ('KES', 'Kenyan Shilling', '404', 'KSh', 'c', 100, 'standard', 2, 'KSh#,##0.00', 'fiat.emerging', 'ke'),
        ('GHS', 'Ghanaian Cedi', '936', 'GH₵', 'p', 100, 'standard', 2, 'GH₵#,##0.00', 'fiat.emerging', 'gh'),
        ('TZS', 'Tanzanian Shilling', '834', 'TSh', 'c', 100, 'standard', 2, 'TSh#,##0.00', 'fiat.emerging', 'tz'),
        ('UGX', 'Ugandan Shilling', '800', 'USh', '', 0, 'standard', 0, 'USh#,##0', 'fiat.emerging', 'ug'),
        ('ETB', 'Ethiopian Birr', '230', 'Br', 's', 100, 'standard', 2, 'Br#,##0.00', 'fiat.emerging', 'et'),
        ('MAD', 'Moroccan Dirham', '504', 'د.م.', 's', 100, 'standard', 2, 'د.م.#,##0.00', 'fiat.emerging', 'ma'),
        ('TND', 'Tunisian Dinar', '788', 'د.ت', 'm', 1000, 'standard', 3, 'د.ت#,##0.000', 'fiat.emerging', 'tn'),
        ('DZD', 'Algerian Dinar', '012', 'د.ج', 's', 100, 'standard', 2, 'د.ج#,##0.00', 'fiat.emerging', 'dz'),
        ('LYD', 'Libyan Dinar', '434', 'ل.د', 'd', 1000, 'standard', 3, 'ل.د#,##0.000', 'fiat.emerging', 'ly'),
        ('SDG', 'Sudanese Pound', '938', 'ج.س', 'q', 100, 'standard', 2, 'ج.س#,##0.00', 'fiat.emerging', 'sd'),
        ('SSP', 'South Sudanese Pound', '728', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat.emerging', 'ss'),
        ('AOA', 'Angolan Kwanza', '973', 'Kz', 'c', 100, 'standard', 2, 'Kz#,##0.00', 'fiat.emerging', 'ao'),
        ('XAF', 'Central African CFA Franc', '950', 'FCFA', 'c', 100, 'standard', 2, 'FCFA#,##0.00', 'fiat.emerging', 'cm'),
        ('XOF', 'West African CFA Franc', '952', 'CFA', 'c', 100, 'standard', 2, 'CFA#,##0.00', 'fiat.emerging', 'sn'),
        ('ZMW', 'Zambian Kwacha', '967', 'ZK', 'n', 100, 'standard', 2, 'ZK#,##0.00', 'fiat.emerging', 'zm'),
        ('MZN', 'Mozambican Metical', '943', 'MT', 'c', 100, 'standard', 2, 'MT#,##0.00', 'fiat.emerging', 'mz'),
        ('BWP', 'Botswana Pula', '072', 'P', 't', 100, 'standard', 2, 'P#,##0.00', 'fiat.emerging', 'bw'),
        ('NAD', 'Namibian Dollar', '516', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'na'),
        ('SZL', 'Swazi Lilangeni', '748', 'L', 'c', 100, 'standard', 2, 'L#,##0.00', 'fiat.emerging', 'sz'),
        ('LSL', 'Lesotho Loti', '426', 'L', 's', 100, 'standard', 2, 'L#,##0.00', 'fiat.emerging', 'ls'),
        ('MWK', 'Malawian Kwacha', '454', 'MK', 't', 100, 'standard', 2, 'MK#,##0.00', 'fiat.emerging', 'mw'),
        ('ZWL', 'Zimbabwean Dollar', '932', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'zw'),
        ('RWF', 'Rwandan Franc', '646', 'FRw', 'c', 100, 'standard', 2, 'FRw#,##0.00', 'fiat.emerging', 'rw'),
        ('BIF', 'Burundian Franc', '108', 'FBu', 'c', 100, 'standard', 2, 'FBu#,##0.00', 'fiat.emerging', 'bi'),
        ('CDF', 'Congolese Franc', '976', 'FC', 'c', 100, 'standard', 2, 'FC#,##0.00', 'fiat.emerging', 'cd'),
        ('DJF', 'Djiboutian Franc', '262', 'Fdj', 'c', 100, 'standard', 2, 'Fdj#,##0.00', 'fiat.emerging', 'dj'),
        ('ERN', 'Eritrean Nakfa', '232', 'Nfk', 'c', 100, 'standard', 2, 'Nfk#,##0.00', 'fiat.emerging', 'er'),
        ('SOS', 'Somali Shilling', '706', 'Sh', 'c', 100, 'standard', 2, 'Sh#,##0.00', 'fiat.emerging', 'so'),
        ('GMD', 'Gambian Dalasi', '270', 'D', 'b', 100, 'standard', 2, 'D#,##0.00', 'fiat.emerging', 'gm'),
        ('GNF', 'Guinean Franc', '324', 'FG', 'c', 100, 'standard', 2, 'FG#,##0.00', 'fiat.emerging', 'gn'),
        ('SLL', 'Sierra Leonean Leone', '694', 'Le', 'c', 100, 'standard', 2, 'Le#,##0.00', 'fiat.emerging', 'sl'),
        ('LRD', 'Liberian Dollar', '430', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'lr'),
        ('CVE', 'Cape Verdean Escudo', '132', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat.emerging', 'cv'),
        ('STN', 'Sao Tome and Principe Dobra', '930', 'Db', 'c', 100, 'standard', 2, 'Db#,##0.00', 'fiat.emerging', 'st'),
        ('MRU', 'Mauritanian Ouguiya', '929', 'UM', 'k', 5, 'standard', 1, 'UM#,##0.0', 'fiat.emerging', 'mr'),
        ('MGA', 'Malagasy Ariary', '969', 'Ar', 'ir', 5, 'standard', 1, 'Ar#,##0.0', 'fiat.emerging', 'mg'),
        ('MUR', 'Mauritian Rupee', '480', '₨', 'c', 100, 'standard', 2, '₨#,##0.00', 'fiat.emerging', 'mu'),
        ('SCR', 'Seychellois Rupee', '690', '₨', 'c', 100, 'standard', 2, '₨#,##0.00', 'fiat.emerging', 'sc'),
        ('KMF', 'Comorian Franc', '174', 'CF', 'c', 100, 'standard', 2, 'CF#,##0.00', 'fiat.emerging', 'km'),
        -- Precious Metals (commodity currencies)
        ('XAU', 'Gold (troy ounce)', '959', 'Au', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'xau'),
        ('XAG', 'Silver (troy ounce)', '961', 'Ag', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'xag'),
        ('XPT', 'Platinum (troy ounce)', '962', 'Pt', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'xpt'),
        ('XPD', 'Palladium (troy ounce)', '964', 'Pd', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'xpd'),
        -- Supranational
        ('XDR', 'Special Drawing Rights', '960', 'SDR', '', 0, 'standard', 6, 'SDR #,##0.000000', 'supranational', 'xdr')
    ) as c(iso_code, name, numeric_code, symbol, fraction_symbol, fractions_per_unit, rounding_type, rounding_precision, format, currency_type, flag_key)
    left join ores.dq_images_artefact_tbl i
        on i.dataset_id = v_flags_dataset_id
        and i.key = c.flag_key;

    get diagnostics v_count = row_count;

    raise notice 'Successfully populated % currencies for dataset: ISO 4217 Currencies from Wikipedia', v_count;

    -- Report currencies using placeholder flag
    raise notice 'Currencies using placeholder flag (xx):';
    perform iso_code
    from ores.dq_currencies_artefact_tbl
    where dataset_id = v_currencies_dataset_id
      and image_id = v_placeholder_image_id;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Currencies Summary ---'

select 'Total DQ Currencies' as metric, count(*) as count
from ores.dq_currencies_artefact_tbl
union all
select 'Major Fiat Currencies (fiat.major)', count(*)
from ores.dq_currencies_artefact_tbl
where currency_type = 'fiat.major'
union all
select 'Emerging Fiat Currencies (fiat.emerging)', count(*)
from ores.dq_currencies_artefact_tbl
where currency_type = 'fiat.emerging'
union all
select 'Commodity Currencies (commodity)', count(*)
from ores.dq_currencies_artefact_tbl
where currency_type = 'commodity'
union all
select 'Supranational Currencies (supranational)', count(*)
from ores.dq_currencies_artefact_tbl
where currency_type = 'supranational'
union all
select 'Currencies with Placeholder Flag', count(*)
from ores.dq_currencies_artefact_tbl c
join ores.dq_images_artefact_tbl i on c.image_id = i.image_id
where i.key = 'xx';
