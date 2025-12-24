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
-- Populates the currencies table with ISO 4217 currency data
--
-- Currency types: fiat, crypto, commodity, supranational
-- Rounding types: standard, swedish (rounds to 0.05), none
--

SET search_path TO ores;

-- Major world currencies (ISO 4217)
INSERT INTO currencies (iso_code, version, name, numeric_code, symbol, fraction_symbol, fractions_per_unit, rounding_type, rounding_precision, format, currency_type, modified_by)
VALUES
-- Americas
('USD', 0, 'US Dollar', '840', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('CAD', 0, 'Canadian Dollar', '124', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('MXN', 0, 'Mexican Peso', '484', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('BRL', 0, 'Brazilian Real', '986', 'R$', '¢', 100, 'standard', 2, 'R$#,##0.00', 'fiat', 'system'),
('ARS', 0, 'Argentine Peso', '032', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('CLP', 0, 'Chilean Peso', '152', '$', '', 0, 'standard', 0, '$#,##0', 'fiat', 'system'),
('COP', 0, 'Colombian Peso', '170', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('PEN', 0, 'Peruvian Sol', '604', 'S/', '¢', 100, 'standard', 2, 'S/#,##0.00', 'fiat', 'system'),
('UYU', 0, 'Uruguayan Peso', '858', '$U', '¢', 100, 'standard', 2, '$U#,##0.00', 'fiat', 'system'),
('PYG', 0, 'Paraguayan Guarani', '600', '₲', '', 0, 'standard', 0, '₲#,##0', 'fiat', 'system'),
('BOB', 0, 'Bolivian Boliviano', '068', 'Bs', '¢', 100, 'standard', 2, 'Bs#,##0.00', 'fiat', 'system'),
('VES', 0, 'Venezuelan Bolivar', '928', 'Bs', '¢', 100, 'standard', 2, 'Bs#,##0.00', 'fiat', 'system'),
('CRC', 0, 'Costa Rican Colon', '188', '₡', '¢', 100, 'standard', 2, '₡#,##0.00', 'fiat', 'system'),
('PAB', 0, 'Panamanian Balboa', '590', 'B/.', '¢', 100, 'standard', 2, 'B/.#,##0.00', 'fiat', 'system'),
('GTQ', 0, 'Guatemalan Quetzal', '320', 'Q', '¢', 100, 'standard', 2, 'Q#,##0.00', 'fiat', 'system'),
('HNL', 0, 'Honduran Lempira', '340', 'L', '¢', 100, 'standard', 2, 'L#,##0.00', 'fiat', 'system'),
('NIO', 0, 'Nicaraguan Cordoba', '558', 'C$', '¢', 100, 'standard', 2, 'C$#,##0.00', 'fiat', 'system'),
('DOP', 0, 'Dominican Peso', '214', 'RD$', '¢', 100, 'standard', 2, 'RD$#,##0.00', 'fiat', 'system'),
('CUP', 0, 'Cuban Peso', '192', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('JMD', 0, 'Jamaican Dollar', '388', 'J$', '¢', 100, 'standard', 2, 'J$#,##0.00', 'fiat', 'system'),
('TTD', 0, 'Trinidad and Tobago Dollar', '780', 'TT$', '¢', 100, 'standard', 2, 'TT$#,##0.00', 'fiat', 'system'),
('BBD', 0, 'Barbadian Dollar', '052', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('BSD', 0, 'Bahamian Dollar', '044', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('HTG', 0, 'Haitian Gourde', '332', 'G', '¢', 100, 'standard', 2, 'G#,##0.00', 'fiat', 'system'),
('SRD', 0, 'Surinamese Dollar', '968', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('GYD', 0, 'Guyanese Dollar', '328', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('BZD', 0, 'Belize Dollar', '084', 'BZ$', '¢', 100, 'standard', 2, 'BZ$#,##0.00', 'fiat', 'system'),
('AWG', 0, 'Aruban Florin', '533', 'ƒ', '¢', 100, 'standard', 2, 'ƒ#,##0.00', 'fiat', 'system'),
('ANG', 0, 'Netherlands Antillean Guilder', '532', 'ƒ', '¢', 100, 'standard', 2, 'ƒ#,##0.00', 'fiat', 'system'),
('XCD', 0, 'East Caribbean Dollar', '951', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('KYD', 0, 'Cayman Islands Dollar', '136', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('BMD', 0, 'Bermudian Dollar', '060', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('FKP', 0, 'Falkland Islands Pound', '238', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system'),

-- Europe
('EUR', 0, 'Euro', '978', '€', 'c', 100, 'standard', 2, '€#,##0.00', 'fiat', 'system'),
('GBP', 0, 'British Pound Sterling', '826', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system'),
('CHF', 0, 'Swiss Franc', '756', 'CHF', 'c', 100, 'swedish', 2, 'CHF #,##0.00', 'fiat', 'system'),
('NOK', 0, 'Norwegian Krone', '578', 'kr', 'ø', 100, 'standard', 2, 'kr #,##0.00', 'fiat', 'system'),
('SEK', 0, 'Swedish Krona', '752', 'kr', 'ö', 100, 'standard', 2, '#,##0.00 kr', 'fiat', 'system'),
('DKK', 0, 'Danish Krone', '208', 'kr', 'ø', 100, 'standard', 2, 'kr #,##0.00', 'fiat', 'system'),
('ISK', 0, 'Icelandic Krona', '352', 'kr', '', 0, 'standard', 0, '#,##0 kr', 'fiat', 'system'),
('PLN', 0, 'Polish Zloty', '985', 'zł', 'gr', 100, 'standard', 2, '#,##0.00 zł', 'fiat', 'system'),
('CZK', 0, 'Czech Koruna', '203', 'Kč', 'h', 100, 'standard', 2, '#,##0.00 Kč', 'fiat', 'system'),
('HUF', 0, 'Hungarian Forint', '348', 'Ft', 'f', 100, 'standard', 2, '#,##0.00 Ft', 'fiat', 'system'),
('RON', 0, 'Romanian Leu', '946', 'lei', 'b', 100, 'standard', 2, '#,##0.00 lei', 'fiat', 'system'),
('BGN', 0, 'Bulgarian Lev', '975', 'лв', 'ст', 100, 'standard', 2, '#,##0.00 лв', 'fiat', 'system'),
('HRK', 0, 'Croatian Kuna', '191', 'kn', 'lp', 100, 'standard', 2, '#,##0.00 kn', 'fiat', 'system'),
('RSD', 0, 'Serbian Dinar', '941', 'дин', 'п', 100, 'standard', 2, '#,##0.00 дин', 'fiat', 'system'),
('BAM', 0, 'Bosnia and Herzegovina Convertible Mark', '977', 'KM', 'pf', 100, 'standard', 2, '#,##0.00 KM', 'fiat', 'system'),
('MKD', 0, 'Macedonian Denar', '807', 'ден', 'д', 100, 'standard', 2, '#,##0.00 ден', 'fiat', 'system'),
('ALL', 0, 'Albanian Lek', '008', 'L', 'q', 100, 'standard', 2, 'L #,##0.00', 'fiat', 'system'),
('MDL', 0, 'Moldovan Leu', '498', 'L', 'b', 100, 'standard', 2, '#,##0.00 L', 'fiat', 'system'),
('UAH', 0, 'Ukrainian Hryvnia', '980', '₴', 'к', 100, 'standard', 2, '#,##0.00 ₴', 'fiat', 'system'),
('BYN', 0, 'Belarusian Ruble', '933', 'Br', 'к', 100, 'standard', 2, 'Br #,##0.00', 'fiat', 'system'),
('RUB', 0, 'Russian Ruble', '643', '₽', 'к', 100, 'standard', 2, '#,##0.00 ₽', 'fiat', 'system'),
('GEL', 0, 'Georgian Lari', '981', '₾', 'თ', 100, 'standard', 2, '#,##0.00 ₾', 'fiat', 'system'),
('AMD', 0, 'Armenian Dram', '051', '֏', 'լ', 100, 'standard', 2, '#,##0.00 ֏', 'fiat', 'system'),
('AZN', 0, 'Azerbaijani Manat', '944', '₼', 'q', 100, 'standard', 2, '#,##0.00 ₼', 'fiat', 'system'),
('TRY', 0, 'Turkish Lira', '949', '₺', 'kr', 100, 'standard', 2, '₺#,##0.00', 'fiat', 'system'),
('GIP', 0, 'Gibraltar Pound', '292', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system'),

-- Asia-Pacific
('JPY', 0, 'Japanese Yen', '392', '¥', '', 0, 'standard', 0, '¥#,##0', 'fiat', 'system'),
('CNY', 0, 'Chinese Yuan', '156', '¥', '分', 100, 'standard', 2, '¥#,##0.00', 'fiat', 'system'),
('HKD', 0, 'Hong Kong Dollar', '344', 'HK$', '¢', 100, 'standard', 2, 'HK$#,##0.00', 'fiat', 'system'),
('TWD', 0, 'New Taiwan Dollar', '901', 'NT$', '¢', 100, 'standard', 2, 'NT$#,##0.00', 'fiat', 'system'),
('KRW', 0, 'South Korean Won', '410', '₩', '', 0, 'standard', 0, '₩#,##0', 'fiat', 'system'),
('KPW', 0, 'North Korean Won', '408', '₩', '전', 100, 'standard', 2, '₩#,##0.00', 'fiat', 'system'),
('SGD', 0, 'Singapore Dollar', '702', 'S$', '¢', 100, 'standard', 2, 'S$#,##0.00', 'fiat', 'system'),
('MYR', 0, 'Malaysian Ringgit', '458', 'RM', 'sen', 100, 'standard', 2, 'RM#,##0.00', 'fiat', 'system'),
('IDR', 0, 'Indonesian Rupiah', '360', 'Rp', 'sen', 100, 'standard', 2, 'Rp#,##0.00', 'fiat', 'system'),
('THB', 0, 'Thai Baht', '764', '฿', 'ส', 100, 'standard', 2, '฿#,##0.00', 'fiat', 'system'),
('VND', 0, 'Vietnamese Dong', '704', '₫', '', 0, 'standard', 0, '#,##0 ₫', 'fiat', 'system'),
('PHP', 0, 'Philippine Peso', '608', '₱', '¢', 100, 'standard', 2, '₱#,##0.00', 'fiat', 'system'),
('INR', 0, 'Indian Rupee', '356', '₹', 'p', 100, 'standard', 2, '₹#,##0.00', 'fiat', 'system'),
('PKR', 0, 'Pakistani Rupee', '586', 'Rs', 'p', 100, 'standard', 2, 'Rs#,##0.00', 'fiat', 'system'),
('BDT', 0, 'Bangladeshi Taka', '050', '৳', 'p', 100, 'standard', 2, '৳#,##0.00', 'fiat', 'system'),
('LKR', 0, 'Sri Lankan Rupee', '144', 'Rs', '¢', 100, 'standard', 2, 'Rs#,##0.00', 'fiat', 'system'),
('NPR', 0, 'Nepalese Rupee', '524', 'Rs', 'p', 100, 'standard', 2, 'Rs#,##0.00', 'fiat', 'system'),
('MMK', 0, 'Myanmar Kyat', '104', 'K', 'p', 100, 'standard', 2, 'K#,##0.00', 'fiat', 'system'),
('KHR', 0, 'Cambodian Riel', '116', '៛', 'សេន', 100, 'standard', 2, '៛#,##0.00', 'fiat', 'system'),
('LAK', 0, 'Lao Kip', '418', '₭', 'ອັດ', 100, 'standard', 2, '₭#,##0.00', 'fiat', 'system'),
('MNT', 0, 'Mongolian Tugrik', '496', '₮', 'м', 100, 'standard', 2, '₮#,##0.00', 'fiat', 'system'),
('KZT', 0, 'Kazakhstani Tenge', '398', '₸', 'т', 100, 'standard', 2, '₸#,##0.00', 'fiat', 'system'),
('UZS', 0, 'Uzbekistani Som', '860', 'сўм', 'т', 100, 'standard', 2, '#,##0.00 сўм', 'fiat', 'system'),
('KGS', 0, 'Kyrgyzstani Som', '417', 'с', 'т', 100, 'standard', 2, '#,##0.00 с', 'fiat', 'system'),
('TJS', 0, 'Tajikistani Somoni', '972', 'ЅМ', 'д', 100, 'standard', 2, '#,##0.00 ЅМ', 'fiat', 'system'),
('TMT', 0, 'Turkmenistani Manat', '934', 'm', 't', 100, 'standard', 2, '#,##0.00 m', 'fiat', 'system'),
('AFN', 0, 'Afghan Afghani', '971', '؋', 'پ', 100, 'standard', 2, '؋#,##0.00', 'fiat', 'system'),
('AUD', 0, 'Australian Dollar', '036', 'A$', '¢', 100, 'standard', 2, 'A$#,##0.00', 'fiat', 'system'),
('NZD', 0, 'New Zealand Dollar', '554', 'NZ$', '¢', 100, 'standard', 2, 'NZ$#,##0.00', 'fiat', 'system'),
('FJD', 0, 'Fijian Dollar', '242', 'FJ$', '¢', 100, 'standard', 2, 'FJ$#,##0.00', 'fiat', 'system'),
('PGK', 0, 'Papua New Guinean Kina', '598', 'K', 't', 100, 'standard', 2, 'K#,##0.00', 'fiat', 'system'),
('SBD', 0, 'Solomon Islands Dollar', '090', 'SI$', '¢', 100, 'standard', 2, 'SI$#,##0.00', 'fiat', 'system'),
('VUV', 0, 'Vanuatu Vatu', '548', 'VT', '', 0, 'standard', 0, 'VT#,##0', 'fiat', 'system'),
('WST', 0, 'Samoan Tala', '882', 'WS$', 's', 100, 'standard', 2, 'WS$#,##0.00', 'fiat', 'system'),
('TOP', 0, 'Tongan Paanga', '776', 'T$', 's', 100, 'standard', 2, 'T$#,##0.00', 'fiat', 'system'),
('MOP', 0, 'Macanese Pataca', '446', 'MOP$', 'a', 100, 'standard', 2, 'MOP$#,##0.00', 'fiat', 'system'),
('BND', 0, 'Brunei Dollar', '096', 'B$', 'sen', 100, 'standard', 2, 'B$#,##0.00', 'fiat', 'system'),
('BTN', 0, 'Bhutanese Ngultrum', '064', 'Nu.', 'ch', 100, 'standard', 2, 'Nu.#,##0.00', 'fiat', 'system'),
('MVR', 0, 'Maldivian Rufiyaa', '462', 'Rf', 'l', 100, 'standard', 2, 'Rf#,##0.00', 'fiat', 'system'),

-- Middle East
('SAR', 0, 'Saudi Riyal', '682', '﷼', 'h', 100, 'standard', 2, '﷼#,##0.00', 'fiat', 'system'),
('AED', 0, 'UAE Dirham', '784', 'د.إ', 'ف', 100, 'standard', 2, 'د.إ#,##0.00', 'fiat', 'system'),
('QAR', 0, 'Qatari Riyal', '634', '﷼', 'd', 100, 'standard', 2, '﷼#,##0.00', 'fiat', 'system'),
('KWD', 0, 'Kuwaiti Dinar', '414', 'د.ك', 'ف', 1000, 'standard', 3, 'د.ك#,##0.000', 'fiat', 'system'),
('BHD', 0, 'Bahraini Dinar', '048', 'BD', 'f', 1000, 'standard', 3, 'BD#,##0.000', 'fiat', 'system'),
('OMR', 0, 'Omani Rial', '512', '﷼', 'b', 1000, 'standard', 3, '﷼#,##0.000', 'fiat', 'system'),
('JOD', 0, 'Jordanian Dinar', '400', 'د.ا', 'ف', 1000, 'standard', 3, 'د.ا#,##0.000', 'fiat', 'system'),
('ILS', 0, 'Israeli New Shekel', '376', '₪', 'a', 100, 'standard', 2, '₪#,##0.00', 'fiat', 'system'),
('LBP', 0, 'Lebanese Pound', '422', 'ل.ل', 'ق', 100, 'standard', 2, 'ل.ل#,##0.00', 'fiat', 'system'),
('SYP', 0, 'Syrian Pound', '760', '£', 'ق', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system'),
('IQD', 0, 'Iraqi Dinar', '368', 'ع.د', 'ف', 1000, 'standard', 3, 'ع.د#,##0.000', 'fiat', 'system'),
('IRR', 0, 'Iranian Rial', '364', '﷼', 'd', 100, 'standard', 2, '﷼#,##0.00', 'fiat', 'system'),
('YER', 0, 'Yemeni Rial', '886', '﷼', 'f', 100, 'standard', 2, '﷼#,##0.00', 'fiat', 'system'),

-- Africa
('ZAR', 0, 'South African Rand', '710', 'R', 'c', 100, 'standard', 2, 'R#,##0.00', 'fiat', 'system'),
('EGP', 0, 'Egyptian Pound', '818', '£', 'pt', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system'),
('NGN', 0, 'Nigerian Naira', '566', '₦', 'k', 100, 'standard', 2, '₦#,##0.00', 'fiat', 'system'),
('KES', 0, 'Kenyan Shilling', '404', 'KSh', 'c', 100, 'standard', 2, 'KSh#,##0.00', 'fiat', 'system'),
('GHS', 0, 'Ghanaian Cedi', '936', 'GH₵', 'p', 100, 'standard', 2, 'GH₵#,##0.00', 'fiat', 'system'),
('TZS', 0, 'Tanzanian Shilling', '834', 'TSh', 'c', 100, 'standard', 2, 'TSh#,##0.00', 'fiat', 'system'),
('UGX', 0, 'Ugandan Shilling', '800', 'USh', '', 0, 'standard', 0, 'USh#,##0', 'fiat', 'system'),
('ETB', 0, 'Ethiopian Birr', '230', 'Br', 's', 100, 'standard', 2, 'Br#,##0.00', 'fiat', 'system'),
('MAD', 0, 'Moroccan Dirham', '504', 'د.م.', 's', 100, 'standard', 2, 'د.م.#,##0.00', 'fiat', 'system'),
('TND', 0, 'Tunisian Dinar', '788', 'د.ت', 'm', 1000, 'standard', 3, 'د.ت#,##0.000', 'fiat', 'system'),
('DZD', 0, 'Algerian Dinar', '012', 'د.ج', 's', 100, 'standard', 2, 'د.ج#,##0.00', 'fiat', 'system'),
('LYD', 0, 'Libyan Dinar', '434', 'ل.د', 'd', 1000, 'standard', 3, 'ل.د#,##0.000', 'fiat', 'system'),
('SDG', 0, 'Sudanese Pound', '938', 'ج.س', 'q', 100, 'standard', 2, 'ج.س#,##0.00', 'fiat', 'system'),
('SSP', 0, 'South Sudanese Pound', '728', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system'),
('AOA', 0, 'Angolan Kwanza', '973', 'Kz', 'c', 100, 'standard', 2, 'Kz#,##0.00', 'fiat', 'system'),
('XAF', 0, 'Central African CFA Franc', '950', 'FCFA', 'c', 100, 'standard', 2, 'FCFA#,##0.00', 'fiat', 'system'),
('XOF', 0, 'West African CFA Franc', '952', 'CFA', 'c', 100, 'standard', 2, 'CFA#,##0.00', 'fiat', 'system'),
('ZMW', 0, 'Zambian Kwacha', '967', 'ZK', 'n', 100, 'standard', 2, 'ZK#,##0.00', 'fiat', 'system'),
('MZN', 0, 'Mozambican Metical', '943', 'MT', 'c', 100, 'standard', 2, 'MT#,##0.00', 'fiat', 'system'),
('BWP', 0, 'Botswana Pula', '072', 'P', 't', 100, 'standard', 2, 'P#,##0.00', 'fiat', 'system'),
('NAD', 0, 'Namibian Dollar', '516', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('SZL', 0, 'Swazi Lilangeni', '748', 'L', 'c', 100, 'standard', 2, 'L#,##0.00', 'fiat', 'system'),
('LSL', 0, 'Lesotho Loti', '426', 'L', 's', 100, 'standard', 2, 'L#,##0.00', 'fiat', 'system'),
('MWK', 0, 'Malawian Kwacha', '454', 'MK', 't', 100, 'standard', 2, 'MK#,##0.00', 'fiat', 'system'),
('ZWL', 0, 'Zimbabwean Dollar', '932', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('RWF', 0, 'Rwandan Franc', '646', 'FRw', 'c', 100, 'standard', 2, 'FRw#,##0.00', 'fiat', 'system'),
('BIF', 0, 'Burundian Franc', '108', 'FBu', 'c', 100, 'standard', 2, 'FBu#,##0.00', 'fiat', 'system'),
('CDF', 0, 'Congolese Franc', '976', 'FC', 'c', 100, 'standard', 2, 'FC#,##0.00', 'fiat', 'system'),
('DJF', 0, 'Djiboutian Franc', '262', 'Fdj', 'c', 100, 'standard', 2, 'Fdj#,##0.00', 'fiat', 'system'),
('ERN', 0, 'Eritrean Nakfa', '232', 'Nfk', 'c', 100, 'standard', 2, 'Nfk#,##0.00', 'fiat', 'system'),
('SOS', 0, 'Somali Shilling', '706', 'Sh', 'c', 100, 'standard', 2, 'Sh#,##0.00', 'fiat', 'system'),
('GMD', 0, 'Gambian Dalasi', '270', 'D', 'b', 100, 'standard', 2, 'D#,##0.00', 'fiat', 'system'),
('GNF', 0, 'Guinean Franc', '324', 'FG', 'c', 100, 'standard', 2, 'FG#,##0.00', 'fiat', 'system'),
('SLL', 0, 'Sierra Leonean Leone', '694', 'Le', 'c', 100, 'standard', 2, 'Le#,##0.00', 'fiat', 'system'),
('LRD', 0, 'Liberian Dollar', '430', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('CVE', 0, 'Cape Verdean Escudo', '132', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system'),
('STN', 0, 'Sao Tome and Principe Dobra', '930', 'Db', 'c', 100, 'standard', 2, 'Db#,##0.00', 'fiat', 'system'),
('MRU', 0, 'Mauritanian Ouguiya', '929', 'UM', 'k', 5, 'standard', 1, 'UM#,##0.0', 'fiat', 'system'),
('MGA', 0, 'Malagasy Ariary', '969', 'Ar', 'ir', 5, 'standard', 1, 'Ar#,##0.0', 'fiat', 'system'),
('MUR', 0, 'Mauritian Rupee', '480', '₨', 'c', 100, 'standard', 2, '₨#,##0.00', 'fiat', 'system'),
('SCR', 0, 'Seychellois Rupee', '690', '₨', 'c', 100, 'standard', 2, '₨#,##0.00', 'fiat', 'system'),
('KMF', 0, 'Comorian Franc', '174', 'CF', 'c', 100, 'standard', 2, 'CF#,##0.00', 'fiat', 'system'),

-- Precious Metals (commodity currencies)
('XAU', 0, 'Gold (troy ounce)', '959', 'Au', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'system'),
('XAG', 0, 'Silver (troy ounce)', '961', 'Ag', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'system'),
('XPT', 0, 'Platinum (troy ounce)', '962', 'Pt', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'system'),
('XPD', 0, 'Palladium (troy ounce)', '964', 'Pd', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'system'),

-- Supranational
('XDR', 0, 'Special Drawing Rights', '960', 'SDR', '', 0, 'standard', 6, 'SDR #,##0.000000', 'supranational', 'system');
