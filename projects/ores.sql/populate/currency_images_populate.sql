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
-- Populates the image_id column on currencies with references to their flag images.
-- Each currency is mapped to the flag of its primary issuing country/region.
--
-- Prerequisites:
--   - flags_populate.sql must be run first (to populate images table)
--   - currencies_populate.sql must be run first (to populate currencies table)
--

SET search_path TO ores;

--
-- Currency to flag mappings
-- Uses UPDATE with a FROM clause to set image_id on currencies:
-- - Only currencies that exist in the currencies table are updated
-- - Only flags that exist in the images table are used
--
UPDATE currencies c
SET image_id = i.image_id
FROM (
    VALUES
        -- Americas
        ('USD', 'us'),   -- US Dollar -> United States
        ('CAD', 'ca'),   -- Canadian Dollar -> Canada
        ('MXN', 'mx'),   -- Mexican Peso -> Mexico
        ('BRL', 'br'),   -- Brazilian Real -> Brazil
        ('ARS', 'ar'),   -- Argentine Peso -> Argentina
        ('CLP', 'cl'),   -- Chilean Peso -> Chile
        ('COP', 'co'),   -- Colombian Peso -> Colombia
        ('PEN', 'pe'),   -- Peruvian Sol -> Peru
        ('UYU', 'uy'),   -- Uruguayan Peso -> Uruguay
        ('PYG', 'py'),   -- Paraguayan Guarani -> Paraguay
        ('BOB', 'bo'),   -- Bolivian Boliviano -> Bolivia
        ('VES', 've'),   -- Venezuelan Bolivar -> Venezuela
        ('CRC', 'cr'),   -- Costa Rican Colon -> Costa Rica
        ('PAB', 'pa'),   -- Panamanian Balboa -> Panama
        ('GTQ', 'gt'),   -- Guatemalan Quetzal -> Guatemala
        ('HNL', 'hn'),   -- Honduran Lempira -> Honduras
        ('NIO', 'ni'),   -- Nicaraguan Cordoba -> Nicaragua
        ('DOP', 'do'),   -- Dominican Peso -> Dominican Republic
        ('CUP', 'cu'),   -- Cuban Peso -> Cuba
        ('JMD', 'jm'),   -- Jamaican Dollar -> Jamaica
        ('TTD', 'tt'),   -- Trinidad and Tobago Dollar -> Trinidad and Tobago
        ('BBD', 'bb'),   -- Barbadian Dollar -> Barbados
        ('BSD', 'bs'),   -- Bahamian Dollar -> Bahamas
        ('HTG', 'ht'),   -- Haitian Gourde -> Haiti
        ('SRD', 'sr'),   -- Surinamese Dollar -> Suriname
        ('GYD', 'gy'),   -- Guyanese Dollar -> Guyana
        ('BZD', 'bz'),   -- Belize Dollar -> Belize
        ('AWG', 'aw'),   -- Aruban Florin -> Aruba
        ('ANG', 'cw'),   -- Netherlands Antillean Guilder -> Curacao
        ('KYD', 'ky'),   -- Cayman Islands Dollar -> Cayman Islands
        ('BMD', 'bm'),   -- Bermudian Dollar -> Bermuda
        ('FKP', 'fk'),   -- Falkland Islands Pound -> Falkland Islands
        ('XCD', 'ag'),   -- East Caribbean Dollar -> Antigua (ECCB HQ)

        -- Europe
        ('EUR', 'eu'),   -- Euro -> European Union
        ('GBP', 'gb'),   -- British Pound -> United Kingdom
        ('CHF', 'ch'),   -- Swiss Franc -> Switzerland
        ('NOK', 'no'),   -- Norwegian Krone -> Norway
        ('SEK', 'se'),   -- Swedish Krona -> Sweden
        ('DKK', 'dk'),   -- Danish Krone -> Denmark
        ('ISK', 'is'),   -- Icelandic Krona -> Iceland
        ('PLN', 'pl'),   -- Polish Zloty -> Poland
        ('CZK', 'cz'),   -- Czech Koruna -> Czech Republic
        ('HUF', 'hu'),   -- Hungarian Forint -> Hungary
        ('RON', 'ro'),   -- Romanian Leu -> Romania
        ('BGN', 'bg'),   -- Bulgarian Lev -> Bulgaria
        ('HRK', 'hr'),   -- Croatian Kuna -> Croatia
        ('RSD', 'rs'),   -- Serbian Dinar -> Serbia
        ('BAM', 'ba'),   -- Bosnia and Herzegovina Mark -> Bosnia and Herzegovina
        ('MKD', 'mk'),   -- Macedonian Denar -> North Macedonia
        ('ALL', 'al'),   -- Albanian Lek -> Albania
        ('MDL', 'md'),   -- Moldovan Leu -> Moldova
        ('UAH', 'ua'),   -- Ukrainian Hryvnia -> Ukraine
        ('BYN', 'by'),   -- Belarusian Ruble -> Belarus
        ('RUB', 'ru'),   -- Russian Ruble -> Russia
        ('GEL', 'ge'),   -- Georgian Lari -> Georgia
        ('AMD', 'am'),   -- Armenian Dram -> Armenia
        ('AZN', 'az'),   -- Azerbaijani Manat -> Azerbaijan
        ('TRY', 'tr'),   -- Turkish Lira -> Turkey
        ('GIP', 'gi'),   -- Gibraltar Pound -> Gibraltar

        -- Asia-Pacific
        ('JPY', 'jp'),   -- Japanese Yen -> Japan
        ('CNY', 'cn'),   -- Chinese Yuan -> China
        ('HKD', 'hk'),   -- Hong Kong Dollar -> Hong Kong
        ('TWD', 'tw'),   -- New Taiwan Dollar -> Taiwan
        ('KRW', 'kr'),   -- South Korean Won -> South Korea
        ('KPW', 'kp'),   -- North Korean Won -> North Korea
        ('SGD', 'sg'),   -- Singapore Dollar -> Singapore
        ('MYR', 'my'),   -- Malaysian Ringgit -> Malaysia
        ('IDR', 'id'),   -- Indonesian Rupiah -> Indonesia
        ('THB', 'th'),   -- Thai Baht -> Thailand
        ('VND', 'vn'),   -- Vietnamese Dong -> Vietnam
        ('PHP', 'ph'),   -- Philippine Peso -> Philippines
        ('INR', 'in'),   -- Indian Rupee -> India
        ('PKR', 'pk'),   -- Pakistani Rupee -> Pakistan
        ('BDT', 'bd'),   -- Bangladeshi Taka -> Bangladesh
        ('LKR', 'lk'),   -- Sri Lankan Rupee -> Sri Lanka
        ('NPR', 'np'),   -- Nepalese Rupee -> Nepal
        ('MMK', 'mm'),   -- Myanmar Kyat -> Myanmar
        ('KHR', 'kh'),   -- Cambodian Riel -> Cambodia
        ('LAK', 'la'),   -- Lao Kip -> Laos
        ('MNT', 'mn'),   -- Mongolian Tugrik -> Mongolia
        ('KZT', 'kz'),   -- Kazakhstani Tenge -> Kazakhstan
        ('UZS', 'uz'),   -- Uzbekistani Som -> Uzbekistan
        ('KGS', 'kg'),   -- Kyrgyzstani Som -> Kyrgyzstan
        ('TJS', 'tj'),   -- Tajikistani Somoni -> Tajikistan
        ('TMT', 'tm'),   -- Turkmenistani Manat -> Turkmenistan
        ('AFN', 'af'),   -- Afghan Afghani -> Afghanistan
        ('AUD', 'au'),   -- Australian Dollar -> Australia
        ('NZD', 'nz'),   -- New Zealand Dollar -> New Zealand
        ('FJD', 'fj'),   -- Fijian Dollar -> Fiji
        ('PGK', 'pg'),   -- Papua New Guinean Kina -> Papua New Guinea
        ('SBD', 'sb'),   -- Solomon Islands Dollar -> Solomon Islands
        ('VUV', 'vu'),   -- Vanuatu Vatu -> Vanuatu
        ('WST', 'ws'),   -- Samoan Tala -> Samoa
        ('TOP', 'to'),   -- Tongan Paanga -> Tonga
        ('MOP', 'mo'),   -- Macanese Pataca -> Macau
        ('BND', 'bn'),   -- Brunei Dollar -> Brunei
        ('BTN', 'bt'),   -- Bhutanese Ngultrum -> Bhutan
        ('MVR', 'mv'),   -- Maldivian Rufiyaa -> Maldives

        -- Middle East
        ('SAR', 'sa'),   -- Saudi Riyal -> Saudi Arabia
        ('AED', 'ae'),   -- UAE Dirham -> United Arab Emirates
        ('QAR', 'qa'),   -- Qatari Riyal -> Qatar
        ('KWD', 'kw'),   -- Kuwaiti Dinar -> Kuwait
        ('BHD', 'bh'),   -- Bahraini Dinar -> Bahrain
        ('OMR', 'om'),   -- Omani Rial -> Oman
        ('JOD', 'jo'),   -- Jordanian Dinar -> Jordan
        ('ILS', 'il'),   -- Israeli New Shekel -> Israel
        ('LBP', 'lb'),   -- Lebanese Pound -> Lebanon
        ('SYP', 'sy'),   -- Syrian Pound -> Syria
        ('IQD', 'iq'),   -- Iraqi Dinar -> Iraq
        ('IRR', 'ir'),   -- Iranian Rial -> Iran
        ('YER', 'ye'),   -- Yemeni Rial -> Yemen

        -- Africa
        ('ZAR', 'za'),   -- South African Rand -> South Africa
        ('EGP', 'eg'),   -- Egyptian Pound -> Egypt
        ('NGN', 'ng'),   -- Nigerian Naira -> Nigeria
        ('KES', 'ke'),   -- Kenyan Shilling -> Kenya
        ('GHS', 'gh'),   -- Ghanaian Cedi -> Ghana
        ('TZS', 'tz'),   -- Tanzanian Shilling -> Tanzania
        ('UGX', 'ug'),   -- Ugandan Shilling -> Uganda
        ('ETB', 'et'),   -- Ethiopian Birr -> Ethiopia
        ('MAD', 'ma'),   -- Moroccan Dirham -> Morocco
        ('TND', 'tn'),   -- Tunisian Dinar -> Tunisia
        ('DZD', 'dz'),   -- Algerian Dinar -> Algeria
        ('LYD', 'ly'),   -- Libyan Dinar -> Libya
        ('SDG', 'sd'),   -- Sudanese Pound -> Sudan
        ('SSP', 'ss'),   -- South Sudanese Pound -> South Sudan
        ('AOA', 'ao'),   -- Angolan Kwanza -> Angola
        ('XAF', 'cm'),   -- Central African CFA Franc -> Cameroon (CEMAC HQ)
        ('XOF', 'sn'),   -- West African CFA Franc -> Senegal (BCEAO HQ)
        ('ZMW', 'zm'),   -- Zambian Kwacha -> Zambia
        ('MZN', 'mz'),   -- Mozambican Metical -> Mozambique
        ('BWP', 'bw'),   -- Botswana Pula -> Botswana
        ('NAD', 'na'),   -- Namibian Dollar -> Namibia
        ('SZL', 'sz'),   -- Swazi Lilangeni -> Eswatini
        ('LSL', 'ls'),   -- Lesotho Loti -> Lesotho
        ('MWK', 'mw'),   -- Malawian Kwacha -> Malawi
        ('ZWL', 'zw'),   -- Zimbabwean Dollar -> Zimbabwe
        ('RWF', 'rw'),   -- Rwandan Franc -> Rwanda
        ('BIF', 'bi'),   -- Burundian Franc -> Burundi
        ('CDF', 'cd'),   -- Congolese Franc -> DR Congo
        ('DJF', 'dj'),   -- Djiboutian Franc -> Djibouti
        ('ERN', 'er'),   -- Eritrean Nakfa -> Eritrea
        ('SOS', 'so'),   -- Somali Shilling -> Somalia
        ('GMD', 'gm'),   -- Gambian Dalasi -> Gambia
        ('GNF', 'gn'),   -- Guinean Franc -> Guinea
        ('SLL', 'sl'),   -- Sierra Leonean Leone -> Sierra Leone
        ('LRD', 'lr'),   -- Liberian Dollar -> Liberia
        ('CVE', 'cv'),   -- Cape Verdean Escudo -> Cape Verde
        ('STN', 'st'),   -- Sao Tome Dobra -> Sao Tome and Principe
        ('MRU', 'mr'),   -- Mauritanian Ouguiya -> Mauritania
        ('MGA', 'mg'),   -- Malagasy Ariary -> Madagascar
        ('MUR', 'mu'),   -- Mauritian Rupee -> Mauritius
        ('SCR', 'sc'),   -- Seychellois Rupee -> Seychelles
        ('KMF', 'km'),   -- Comorian Franc -> Comoros

        -- Commodity currencies (precious metals)
        ('XAU', 'xau'),  -- Gold -> Gold bar icon
        ('XAG', 'xag'),  -- Silver -> Silver bar icon
        ('XPT', 'xpt'),  -- Platinum -> Platinum bar icon
        ('XPD', 'xpd'),  -- Palladium -> Palladium bar icon

        -- Supranational currency
        ('XDR', 'xdr')   -- Special Drawing Rights -> SDR globe icon
) AS v(currency_code, flag_key)
JOIN images i ON i.key = v.flag_key AND i.valid_to = '9999-12-31 23:59:59'::timestamptz
WHERE c.iso_code = v.currency_code AND c.valid_to = '9999-12-31 23:59:59'::timestamptz;
