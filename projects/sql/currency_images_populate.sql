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

--
-- Populates the currency_images table with mappings from currencies to their flags.
-- Each currency is mapped to the flag of its primary issuing country/region.
-- Currencies without an available flag are not included.
--
-- Prerequisites:
--   - flags_populate.sql must be run first (to populate images table)
--   - currencies_populate.sql must be run first (to populate currencies table)
--

SET search_path TO oresdb;

--
-- Helper function to link a currency to a flag image by key.
-- Returns the image_id if successful, NULL otherwise.
--
CREATE OR REPLACE FUNCTION link_currency_to_flag(
    p_currency_code text,
    p_flag_key text
) RETURNS uuid AS $$
DECLARE
    v_image_id uuid;
BEGIN
    -- Look up the image_id for the flag
    SELECT image_id INTO v_image_id
    FROM images
    WHERE key = p_flag_key
    AND valid_to = '9999-12-31 23:59:59'::timestamptz;

    IF v_image_id IS NULL THEN
        RAISE NOTICE 'Flag not found for key: %', p_flag_key;
        RETURN NULL;
    END IF;

    -- Check if the currency exists
    IF NOT EXISTS (
        SELECT 1 FROM currencies
        WHERE iso_code = p_currency_code
        AND valid_to = '9999-12-31 23:59:59'::timestamptz
    ) THEN
        RAISE NOTICE 'Currency not found: %', p_currency_code;
        RETURN NULL;
    END IF;

    -- Insert the mapping
    INSERT INTO currency_images (iso_code, image_id, assigned_by)
    VALUES (p_currency_code, v_image_id, 'system');

    RETURN v_image_id;
END;
$$ LANGUAGE plpgsql;

--
-- Currency to flag mappings
-- Format: SELECT link_currency_to_flag('CURRENCY_CODE', 'flag_key');
--

-- Americas
SELECT link_currency_to_flag('USD', 'us');   -- US Dollar -> United States
SELECT link_currency_to_flag('CAD', 'ca');   -- Canadian Dollar -> Canada
SELECT link_currency_to_flag('MXN', 'mx');   -- Mexican Peso -> Mexico
SELECT link_currency_to_flag('BRL', 'br');   -- Brazilian Real -> Brazil
SELECT link_currency_to_flag('ARS', 'ar');   -- Argentine Peso -> Argentina
SELECT link_currency_to_flag('CLP', 'cl');   -- Chilean Peso -> Chile
SELECT link_currency_to_flag('COP', 'co');   -- Colombian Peso -> Colombia
SELECT link_currency_to_flag('PEN', 'pe');   -- Peruvian Sol -> Peru
SELECT link_currency_to_flag('UYU', 'uy');   -- Uruguayan Peso -> Uruguay
SELECT link_currency_to_flag('PYG', 'py');   -- Paraguayan Guarani -> Paraguay
SELECT link_currency_to_flag('BOB', 'bo');   -- Bolivian Boliviano -> Bolivia
SELECT link_currency_to_flag('VES', 've');   -- Venezuelan Bolivar -> Venezuela
SELECT link_currency_to_flag('CRC', 'cr');   -- Costa Rican Colon -> Costa Rica
SELECT link_currency_to_flag('PAB', 'pa');   -- Panamanian Balboa -> Panama
SELECT link_currency_to_flag('GTQ', 'gt');   -- Guatemalan Quetzal -> Guatemala
SELECT link_currency_to_flag('HNL', 'hn');   -- Honduran Lempira -> Honduras
SELECT link_currency_to_flag('NIO', 'ni');   -- Nicaraguan Cordoba -> Nicaragua
SELECT link_currency_to_flag('DOP', 'do');   -- Dominican Peso -> Dominican Republic
SELECT link_currency_to_flag('CUP', 'cu');   -- Cuban Peso -> Cuba
SELECT link_currency_to_flag('JMD', 'jm');   -- Jamaican Dollar -> Jamaica
SELECT link_currency_to_flag('TTD', 'tt');   -- Trinidad and Tobago Dollar -> Trinidad and Tobago
SELECT link_currency_to_flag('BBD', 'bb');   -- Barbadian Dollar -> Barbados
SELECT link_currency_to_flag('BSD', 'bs');   -- Bahamian Dollar -> Bahamas
SELECT link_currency_to_flag('HTG', 'ht');   -- Haitian Gourde -> Haiti
SELECT link_currency_to_flag('SRD', 'sr');   -- Surinamese Dollar -> Suriname
SELECT link_currency_to_flag('GYD', 'gy');   -- Guyanese Dollar -> Guyana
SELECT link_currency_to_flag('BZD', 'bz');   -- Belize Dollar -> Belize
SELECT link_currency_to_flag('AWG', 'aw');   -- Aruban Florin -> Aruba
SELECT link_currency_to_flag('ANG', 'cw');   -- Netherlands Antillean Guilder -> Curacao
SELECT link_currency_to_flag('KYD', 'ky');   -- Cayman Islands Dollar -> Cayman Islands
SELECT link_currency_to_flag('BMD', 'bm');   -- Bermudian Dollar -> Bermuda
SELECT link_currency_to_flag('FKP', 'fk');   -- Falkland Islands Pound -> Falkland Islands

-- Europe
SELECT link_currency_to_flag('EUR', 'eu');   -- Euro -> European Union
SELECT link_currency_to_flag('GBP', 'gb');   -- British Pound -> United Kingdom
SELECT link_currency_to_flag('CHF', 'ch');   -- Swiss Franc -> Switzerland
SELECT link_currency_to_flag('NOK', 'no');   -- Norwegian Krone -> Norway
SELECT link_currency_to_flag('SEK', 'se');   -- Swedish Krona -> Sweden
SELECT link_currency_to_flag('DKK', 'dk');   -- Danish Krone -> Denmark
SELECT link_currency_to_flag('ISK', 'is');   -- Icelandic Krona -> Iceland
SELECT link_currency_to_flag('PLN', 'pl');   -- Polish Zloty -> Poland
SELECT link_currency_to_flag('CZK', 'cz');   -- Czech Koruna -> Czech Republic
SELECT link_currency_to_flag('HUF', 'hu');   -- Hungarian Forint -> Hungary
SELECT link_currency_to_flag('RON', 'ro');   -- Romanian Leu -> Romania
SELECT link_currency_to_flag('BGN', 'bg');   -- Bulgarian Lev -> Bulgaria
SELECT link_currency_to_flag('HRK', 'hr');   -- Croatian Kuna -> Croatia
SELECT link_currency_to_flag('RSD', 'rs');   -- Serbian Dinar -> Serbia
SELECT link_currency_to_flag('BAM', 'ba');   -- Bosnia and Herzegovina Mark -> Bosnia and Herzegovina
SELECT link_currency_to_flag('MKD', 'mk');   -- Macedonian Denar -> North Macedonia
SELECT link_currency_to_flag('ALL', 'al');   -- Albanian Lek -> Albania
SELECT link_currency_to_flag('MDL', 'md');   -- Moldovan Leu -> Moldova
SELECT link_currency_to_flag('UAH', 'ua');   -- Ukrainian Hryvnia -> Ukraine
SELECT link_currency_to_flag('BYN', 'by');   -- Belarusian Ruble -> Belarus
SELECT link_currency_to_flag('RUB', 'ru');   -- Russian Ruble -> Russia
SELECT link_currency_to_flag('GEL', 'ge');   -- Georgian Lari -> Georgia
SELECT link_currency_to_flag('AMD', 'am');   -- Armenian Dram -> Armenia
SELECT link_currency_to_flag('AZN', 'az');   -- Azerbaijani Manat -> Azerbaijan
SELECT link_currency_to_flag('TRY', 'tr');   -- Turkish Lira -> Turkey
SELECT link_currency_to_flag('GIP', 'gi');   -- Gibraltar Pound -> Gibraltar

-- Asia-Pacific
SELECT link_currency_to_flag('JPY', 'jp');   -- Japanese Yen -> Japan
SELECT link_currency_to_flag('CNY', 'cn');   -- Chinese Yuan -> China
SELECT link_currency_to_flag('HKD', 'hk');   -- Hong Kong Dollar -> Hong Kong
SELECT link_currency_to_flag('TWD', 'tw');   -- New Taiwan Dollar -> Taiwan
SELECT link_currency_to_flag('KRW', 'kr');   -- South Korean Won -> South Korea
SELECT link_currency_to_flag('KPW', 'kp');   -- North Korean Won -> North Korea
SELECT link_currency_to_flag('SGD', 'sg');   -- Singapore Dollar -> Singapore
SELECT link_currency_to_flag('MYR', 'my');   -- Malaysian Ringgit -> Malaysia
SELECT link_currency_to_flag('IDR', 'id');   -- Indonesian Rupiah -> Indonesia
SELECT link_currency_to_flag('THB', 'th');   -- Thai Baht -> Thailand
SELECT link_currency_to_flag('VND', 'vn');   -- Vietnamese Dong -> Vietnam
SELECT link_currency_to_flag('PHP', 'ph');   -- Philippine Peso -> Philippines
SELECT link_currency_to_flag('INR', 'in');   -- Indian Rupee -> India
SELECT link_currency_to_flag('PKR', 'pk');   -- Pakistani Rupee -> Pakistan
SELECT link_currency_to_flag('BDT', 'bd');   -- Bangladeshi Taka -> Bangladesh
SELECT link_currency_to_flag('LKR', 'lk');   -- Sri Lankan Rupee -> Sri Lanka
SELECT link_currency_to_flag('NPR', 'np');   -- Nepalese Rupee -> Nepal
SELECT link_currency_to_flag('MMK', 'mm');   -- Myanmar Kyat -> Myanmar
SELECT link_currency_to_flag('KHR', 'kh');   -- Cambodian Riel -> Cambodia
SELECT link_currency_to_flag('LAK', 'la');   -- Lao Kip -> Laos
SELECT link_currency_to_flag('MNT', 'mn');   -- Mongolian Tugrik -> Mongolia
SELECT link_currency_to_flag('KZT', 'kz');   -- Kazakhstani Tenge -> Kazakhstan
SELECT link_currency_to_flag('UZS', 'uz');   -- Uzbekistani Som -> Uzbekistan
SELECT link_currency_to_flag('KGS', 'kg');   -- Kyrgyzstani Som -> Kyrgyzstan
SELECT link_currency_to_flag('TJS', 'tj');   -- Tajikistani Somoni -> Tajikistan
SELECT link_currency_to_flag('TMT', 'tm');   -- Turkmenistani Manat -> Turkmenistan
SELECT link_currency_to_flag('AFN', 'af');   -- Afghan Afghani -> Afghanistan
SELECT link_currency_to_flag('AUD', 'au');   -- Australian Dollar -> Australia
SELECT link_currency_to_flag('NZD', 'nz');   -- New Zealand Dollar -> New Zealand
SELECT link_currency_to_flag('FJD', 'fj');   -- Fijian Dollar -> Fiji
SELECT link_currency_to_flag('PGK', 'pg');   -- Papua New Guinean Kina -> Papua New Guinea
SELECT link_currency_to_flag('SBD', 'sb');   -- Solomon Islands Dollar -> Solomon Islands
SELECT link_currency_to_flag('VUV', 'vu');   -- Vanuatu Vatu -> Vanuatu
SELECT link_currency_to_flag('WST', 'ws');   -- Samoan Tala -> Samoa
SELECT link_currency_to_flag('TOP', 'to');   -- Tongan Paanga -> Tonga
SELECT link_currency_to_flag('MOP', 'mo');   -- Macanese Pataca -> Macau
SELECT link_currency_to_flag('BND', 'bn');   -- Brunei Dollar -> Brunei
SELECT link_currency_to_flag('BTN', 'bt');   -- Bhutanese Ngultrum -> Bhutan
SELECT link_currency_to_flag('MVR', 'mv');   -- Maldivian Rufiyaa -> Maldives

-- Middle East
SELECT link_currency_to_flag('SAR', 'sa');   -- Saudi Riyal -> Saudi Arabia
SELECT link_currency_to_flag('AED', 'ae');   -- UAE Dirham -> United Arab Emirates
SELECT link_currency_to_flag('QAR', 'qa');   -- Qatari Riyal -> Qatar
SELECT link_currency_to_flag('KWD', 'kw');   -- Kuwaiti Dinar -> Kuwait
SELECT link_currency_to_flag('BHD', 'bh');   -- Bahraini Dinar -> Bahrain
SELECT link_currency_to_flag('OMR', 'om');   -- Omani Rial -> Oman
SELECT link_currency_to_flag('JOD', 'jo');   -- Jordanian Dinar -> Jordan
SELECT link_currency_to_flag('ILS', 'il');   -- Israeli New Shekel -> Israel
SELECT link_currency_to_flag('LBP', 'lb');   -- Lebanese Pound -> Lebanon
SELECT link_currency_to_flag('SYP', 'sy');   -- Syrian Pound -> Syria
SELECT link_currency_to_flag('IQD', 'iq');   -- Iraqi Dinar -> Iraq
SELECT link_currency_to_flag('IRR', 'ir');   -- Iranian Rial -> Iran
SELECT link_currency_to_flag('YER', 'ye');   -- Yemeni Rial -> Yemen

-- Africa
SELECT link_currency_to_flag('ZAR', 'za');   -- South African Rand -> South Africa
SELECT link_currency_to_flag('EGP', 'eg');   -- Egyptian Pound -> Egypt
SELECT link_currency_to_flag('NGN', 'ng');   -- Nigerian Naira -> Nigeria
SELECT link_currency_to_flag('KES', 'ke');   -- Kenyan Shilling -> Kenya
SELECT link_currency_to_flag('GHS', 'gh');   -- Ghanaian Cedi -> Ghana
SELECT link_currency_to_flag('TZS', 'tz');   -- Tanzanian Shilling -> Tanzania
SELECT link_currency_to_flag('UGX', 'ug');   -- Ugandan Shilling -> Uganda
SELECT link_currency_to_flag('ETB', 'et');   -- Ethiopian Birr -> Ethiopia
SELECT link_currency_to_flag('MAD', 'ma');   -- Moroccan Dirham -> Morocco
SELECT link_currency_to_flag('TND', 'tn');   -- Tunisian Dinar -> Tunisia
SELECT link_currency_to_flag('DZD', 'dz');   -- Algerian Dinar -> Algeria
SELECT link_currency_to_flag('LYD', 'ly');   -- Libyan Dinar -> Libya
SELECT link_currency_to_flag('SDG', 'sd');   -- Sudanese Pound -> Sudan
SELECT link_currency_to_flag('SSP', 'ss');   -- South Sudanese Pound -> South Sudan
SELECT link_currency_to_flag('AOA', 'ao');   -- Angolan Kwanza -> Angola
SELECT link_currency_to_flag('XAF', 'cm');   -- Central African CFA Franc -> Cameroon (CEMAC HQ)
SELECT link_currency_to_flag('XOF', 'sn');   -- West African CFA Franc -> Senegal (BCEAO HQ)
SELECT link_currency_to_flag('ZMW', 'zm');   -- Zambian Kwacha -> Zambia
SELECT link_currency_to_flag('MZN', 'mz');   -- Mozambican Metical -> Mozambique
SELECT link_currency_to_flag('BWP', 'bw');   -- Botswana Pula -> Botswana
SELECT link_currency_to_flag('NAD', 'na');   -- Namibian Dollar -> Namibia
SELECT link_currency_to_flag('SZL', 'sz');   -- Swazi Lilangeni -> Eswatini
SELECT link_currency_to_flag('LSL', 'ls');   -- Lesotho Loti -> Lesotho
SELECT link_currency_to_flag('MWK', 'mw');   -- Malawian Kwacha -> Malawi
SELECT link_currency_to_flag('ZWL', 'zw');   -- Zimbabwean Dollar -> Zimbabwe
SELECT link_currency_to_flag('RWF', 'rw');   -- Rwandan Franc -> Rwanda
SELECT link_currency_to_flag('BIF', 'bi');   -- Burundian Franc -> Burundi
SELECT link_currency_to_flag('CDF', 'cd');   -- Congolese Franc -> DR Congo
SELECT link_currency_to_flag('DJF', 'dj');   -- Djiboutian Franc -> Djibouti
SELECT link_currency_to_flag('ERN', 'er');   -- Eritrean Nakfa -> Eritrea
SELECT link_currency_to_flag('SOS', 'so');   -- Somali Shilling -> Somalia
SELECT link_currency_to_flag('GMD', 'gm');   -- Gambian Dalasi -> Gambia
SELECT link_currency_to_flag('GNF', 'gn');   -- Guinean Franc -> Guinea
SELECT link_currency_to_flag('SLL', 'sl');   -- Sierra Leonean Leone -> Sierra Leone
SELECT link_currency_to_flag('LRD', 'lr');   -- Liberian Dollar -> Liberia
SELECT link_currency_to_flag('CVE', 'cv');   -- Cape Verdean Escudo -> Cape Verde
SELECT link_currency_to_flag('STN', 'st');   -- Sao Tome Dobra -> Sao Tome and Principe
SELECT link_currency_to_flag('MRU', 'mr');   -- Mauritanian Ouguiya -> Mauritania
SELECT link_currency_to_flag('MGA', 'mg');   -- Malagasy Ariary -> Madagascar
SELECT link_currency_to_flag('MUR', 'mu');   -- Mauritian Rupee -> Mauritius
SELECT link_currency_to_flag('SCR', 'sc');   -- Seychellois Rupee -> Seychelles
SELECT link_currency_to_flag('KMF', 'km');   -- Comorian Franc -> Comoros

-- Note: Commodity currencies (XAU, XAG, XPT, XPD) and supranational (XDR)
-- do not have associated country flags.

-- Clean up the helper function
DROP FUNCTION IF EXISTS link_currency_to_flag(text, text);
