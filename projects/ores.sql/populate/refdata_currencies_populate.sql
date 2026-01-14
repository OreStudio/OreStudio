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
INSERT INTO refdata_currencies_tbl (iso_code, version, name, numeric_code, symbol, fraction_symbol, fractions_per_unit, rounding_type, rounding_precision, format, currency_type, modified_by, change_reason_code, change_commentary)
VALUES
-- Americas
('USD', 0, 'US Dollar', '840', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('CAD', 0, 'Canadian Dollar', '124', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MXN', 0, 'Mexican Peso', '484', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BRL', 0, 'Brazilian Real', '986', 'R$', '¢', 100, 'standard', 2, 'R$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('ARS', 0, 'Argentine Peso', '032', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('CLP', 0, 'Chilean Peso', '152', '$', '', 0, 'standard', 0, '$#,##0', 'fiat', 'system', 'system.new_record', 'System seed data'),
('COP', 0, 'Colombian Peso', '170', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('PEN', 0, 'Peruvian Sol', '604', 'S/', '¢', 100, 'standard', 2, 'S/#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('UYU', 0, 'Uruguayan Peso', '858', '$U', '¢', 100, 'standard', 2, '$U#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('PYG', 0, 'Paraguayan Guarani', '600', '₲', '', 0, 'standard', 0, '₲#,##0', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BOB', 0, 'Bolivian Boliviano', '068', 'Bs', '¢', 100, 'standard', 2, 'Bs#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('VES', 0, 'Venezuelan Bolivar', '928', 'Bs', '¢', 100, 'standard', 2, 'Bs#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('CRC', 0, 'Costa Rican Colon', '188', '₡', '¢', 100, 'standard', 2, '₡#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('PAB', 0, 'Panamanian Balboa', '590', 'B/.', '¢', 100, 'standard', 2, 'B/.#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('GTQ', 0, 'Guatemalan Quetzal', '320', 'Q', '¢', 100, 'standard', 2, 'Q#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('HNL', 0, 'Honduran Lempira', '340', 'L', '¢', 100, 'standard', 2, 'L#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('NIO', 0, 'Nicaraguan Cordoba', '558', 'C$', '¢', 100, 'standard', 2, 'C$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('DOP', 0, 'Dominican Peso', '214', 'RD$', '¢', 100, 'standard', 2, 'RD$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('CUP', 0, 'Cuban Peso', '192', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('JMD', 0, 'Jamaican Dollar', '388', 'J$', '¢', 100, 'standard', 2, 'J$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('TTD', 0, 'Trinidad and Tobago Dollar', '780', 'TT$', '¢', 100, 'standard', 2, 'TT$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BBD', 0, 'Barbadian Dollar', '052', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BSD', 0, 'Bahamian Dollar', '044', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('HTG', 0, 'Haitian Gourde', '332', 'G', '¢', 100, 'standard', 2, 'G#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('SRD', 0, 'Surinamese Dollar', '968', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('GYD', 0, 'Guyanese Dollar', '328', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BZD', 0, 'Belize Dollar', '084', 'BZ$', '¢', 100, 'standard', 2, 'BZ$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('AWG', 0, 'Aruban Florin', '533', 'ƒ', '¢', 100, 'standard', 2, 'ƒ#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('ANG', 0, 'Netherlands Antillean Guilder', '532', 'ƒ', '¢', 100, 'standard', 2, 'ƒ#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('XCD', 0, 'East Caribbean Dollar', '951', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('KYD', 0, 'Cayman Islands Dollar', '136', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BMD', 0, 'Bermudian Dollar', '060', '$', '¢', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('FKP', 0, 'Falkland Islands Pound', '238', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),

-- Europe
('EUR', 0, 'Euro', '978', '€', 'c', 100, 'standard', 2, '€#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('GBP', 0, 'British Pound Sterling', '826', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('CHF', 0, 'Swiss Franc', '756', 'CHF', 'c', 100, 'swedish', 2, 'CHF #,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('NOK', 0, 'Norwegian Krone', '578', 'kr', 'ø', 100, 'standard', 2, 'kr #,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('SEK', 0, 'Swedish Krona', '752', 'kr', 'ö', 100, 'standard', 2, '#,##0.00 kr', 'fiat', 'system', 'system.new_record', 'System seed data'),
('DKK', 0, 'Danish Krone', '208', 'kr', 'ø', 100, 'standard', 2, 'kr #,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('ISK', 0, 'Icelandic Krona', '352', 'kr', '', 0, 'standard', 0, '#,##0 kr', 'fiat', 'system', 'system.new_record', 'System seed data'),
('PLN', 0, 'Polish Zloty', '985', 'zł', 'gr', 100, 'standard', 2, '#,##0.00 zł', 'fiat', 'system', 'system.new_record', 'System seed data'),
('CZK', 0, 'Czech Koruna', '203', 'Kč', 'h', 100, 'standard', 2, '#,##0.00 Kč', 'fiat', 'system', 'system.new_record', 'System seed data'),
('HUF', 0, 'Hungarian Forint', '348', 'Ft', 'f', 100, 'standard', 2, '#,##0.00 Ft', 'fiat', 'system', 'system.new_record', 'System seed data'),
('RON', 0, 'Romanian Leu', '946', 'lei', 'b', 100, 'standard', 2, '#,##0.00 lei', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BGN', 0, 'Bulgarian Lev', '975', 'лв', 'ст', 100, 'standard', 2, '#,##0.00 лв', 'fiat', 'system', 'system.new_record', 'System seed data'),
('HRK', 0, 'Croatian Kuna', '191', 'kn', 'lp', 100, 'standard', 2, '#,##0.00 kn', 'fiat', 'system', 'system.new_record', 'System seed data'),
('RSD', 0, 'Serbian Dinar', '941', 'дин', 'п', 100, 'standard', 2, '#,##0.00 дин', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BAM', 0, 'Bosnia and Herzegovina Convertible Mark', '977', 'KM', 'pf', 100, 'standard', 2, '#,##0.00 KM', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MKD', 0, 'Macedonian Denar', '807', 'ден', 'д', 100, 'standard', 2, '#,##0.00 ден', 'fiat', 'system', 'system.new_record', 'System seed data'),
('ALL', 0, 'Albanian Lek', '008', 'L', 'q', 100, 'standard', 2, 'L #,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MDL', 0, 'Moldovan Leu', '498', 'L', 'b', 100, 'standard', 2, '#,##0.00 L', 'fiat', 'system', 'system.new_record', 'System seed data'),
('UAH', 0, 'Ukrainian Hryvnia', '980', '₴', 'к', 100, 'standard', 2, '#,##0.00 ₴', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BYN', 0, 'Belarusian Ruble', '933', 'Br', 'к', 100, 'standard', 2, 'Br #,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('RUB', 0, 'Russian Ruble', '643', '₽', 'к', 100, 'standard', 2, '#,##0.00 ₽', 'fiat', 'system', 'system.new_record', 'System seed data'),
('GEL', 0, 'Georgian Lari', '981', '₾', 'თ', 100, 'standard', 2, '#,##0.00 ₾', 'fiat', 'system', 'system.new_record', 'System seed data'),
('AMD', 0, 'Armenian Dram', '051', '֏', 'լ', 100, 'standard', 2, '#,##0.00 ֏', 'fiat', 'system', 'system.new_record', 'System seed data'),
('AZN', 0, 'Azerbaijani Manat', '944', '₼', 'q', 100, 'standard', 2, '#,##0.00 ₼', 'fiat', 'system', 'system.new_record', 'System seed data'),
('TRY', 0, 'Turkish Lira', '949', '₺', 'kr', 100, 'standard', 2, '₺#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('GIP', 0, 'Gibraltar Pound', '292', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),

-- Asia-Pacific
('JPY', 0, 'Japanese Yen', '392', '¥', '', 0, 'standard', 0, '¥#,##0', 'fiat', 'system', 'system.new_record', 'System seed data'),
('CNY', 0, 'Chinese Yuan', '156', '¥', '分', 100, 'standard', 2, '¥#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('HKD', 0, 'Hong Kong Dollar', '344', 'HK$', '¢', 100, 'standard', 2, 'HK$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('TWD', 0, 'New Taiwan Dollar', '901', 'NT$', '¢', 100, 'standard', 2, 'NT$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('KRW', 0, 'South Korean Won', '410', '₩', '', 0, 'standard', 0, '₩#,##0', 'fiat', 'system', 'system.new_record', 'System seed data'),
('KPW', 0, 'North Korean Won', '408', '₩', '전', 100, 'standard', 2, '₩#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('SGD', 0, 'Singapore Dollar', '702', 'S$', '¢', 100, 'standard', 2, 'S$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MYR', 0, 'Malaysian Ringgit', '458', 'RM', 'sen', 100, 'standard', 2, 'RM#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('IDR', 0, 'Indonesian Rupiah', '360', 'Rp', 'sen', 100, 'standard', 2, 'Rp#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('THB', 0, 'Thai Baht', '764', '฿', 'ส', 100, 'standard', 2, '฿#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('VND', 0, 'Vietnamese Dong', '704', '₫', '', 0, 'standard', 0, '#,##0 ₫', 'fiat', 'system', 'system.new_record', 'System seed data'),
('PHP', 0, 'Philippine Peso', '608', '₱', '¢', 100, 'standard', 2, '₱#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('INR', 0, 'Indian Rupee', '356', '₹', 'p', 100, 'standard', 2, '₹#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('PKR', 0, 'Pakistani Rupee', '586', 'Rs', 'p', 100, 'standard', 2, 'Rs#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BDT', 0, 'Bangladeshi Taka', '050', '৳', 'p', 100, 'standard', 2, '৳#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('LKR', 0, 'Sri Lankan Rupee', '144', 'Rs', '¢', 100, 'standard', 2, 'Rs#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('NPR', 0, 'Nepalese Rupee', '524', 'Rs', 'p', 100, 'standard', 2, 'Rs#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MMK', 0, 'Myanmar Kyat', '104', 'K', 'p', 100, 'standard', 2, 'K#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('KHR', 0, 'Cambodian Riel', '116', '៛', 'សេន', 100, 'standard', 2, '៛#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('LAK', 0, 'Lao Kip', '418', '₭', 'ອັດ', 100, 'standard', 2, '₭#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MNT', 0, 'Mongolian Tugrik', '496', '₮', 'м', 100, 'standard', 2, '₮#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('KZT', 0, 'Kazakhstani Tenge', '398', '₸', 'т', 100, 'standard', 2, '₸#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('UZS', 0, 'Uzbekistani Som', '860', 'сўм', 'т', 100, 'standard', 2, '#,##0.00 сўм', 'fiat', 'system', 'system.new_record', 'System seed data'),
('KGS', 0, 'Kyrgyzstani Som', '417', 'с', 'т', 100, 'standard', 2, '#,##0.00 с', 'fiat', 'system', 'system.new_record', 'System seed data'),
('TJS', 0, 'Tajikistani Somoni', '972', 'ЅМ', 'д', 100, 'standard', 2, '#,##0.00 ЅМ', 'fiat', 'system', 'system.new_record', 'System seed data'),
('TMT', 0, 'Turkmenistani Manat', '934', 'm', 't', 100, 'standard', 2, '#,##0.00 m', 'fiat', 'system', 'system.new_record', 'System seed data'),
('AFN', 0, 'Afghan Afghani', '971', '؋', 'پ', 100, 'standard', 2, '؋#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('AUD', 0, 'Australian Dollar', '036', 'A$', '¢', 100, 'standard', 2, 'A$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('NZD', 0, 'New Zealand Dollar', '554', 'NZ$', '¢', 100, 'standard', 2, 'NZ$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('FJD', 0, 'Fijian Dollar', '242', 'FJ$', '¢', 100, 'standard', 2, 'FJ$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('PGK', 0, 'Papua New Guinean Kina', '598', 'K', 't', 100, 'standard', 2, 'K#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('SBD', 0, 'Solomon Islands Dollar', '090', 'SI$', '¢', 100, 'standard', 2, 'SI$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('VUV', 0, 'Vanuatu Vatu', '548', 'VT', '', 0, 'standard', 0, 'VT#,##0', 'fiat', 'system', 'system.new_record', 'System seed data'),
('WST', 0, 'Samoan Tala', '882', 'WS$', 's', 100, 'standard', 2, 'WS$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('TOP', 0, 'Tongan Paanga', '776', 'T$', 's', 100, 'standard', 2, 'T$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MOP', 0, 'Macanese Pataca', '446', 'MOP$', 'a', 100, 'standard', 2, 'MOP$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BND', 0, 'Brunei Dollar', '096', 'B$', 'sen', 100, 'standard', 2, 'B$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BTN', 0, 'Bhutanese Ngultrum', '064', 'Nu.', 'ch', 100, 'standard', 2, 'Nu.#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MVR', 0, 'Maldivian Rufiyaa', '462', 'Rf', 'l', 100, 'standard', 2, 'Rf#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),

-- Middle East
('SAR', 0, 'Saudi Riyal', '682', '﷼', 'h', 100, 'standard', 2, '﷼#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('AED', 0, 'UAE Dirham', '784', 'د.إ', 'ف', 100, 'standard', 2, 'د.إ#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('QAR', 0, 'Qatari Riyal', '634', '﷼', 'd', 100, 'standard', 2, '﷼#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('KWD', 0, 'Kuwaiti Dinar', '414', 'د.ك', 'ف', 1000, 'standard', 3, 'د.ك#,##0.000', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BHD', 0, 'Bahraini Dinar', '048', 'BD', 'f', 1000, 'standard', 3, 'BD#,##0.000', 'fiat', 'system', 'system.new_record', 'System seed data'),
('OMR', 0, 'Omani Rial', '512', '﷼', 'b', 1000, 'standard', 3, '﷼#,##0.000', 'fiat', 'system', 'system.new_record', 'System seed data'),
('JOD', 0, 'Jordanian Dinar', '400', 'د.ا', 'ف', 1000, 'standard', 3, 'د.ا#,##0.000', 'fiat', 'system', 'system.new_record', 'System seed data'),
('ILS', 0, 'Israeli New Shekel', '376', '₪', 'a', 100, 'standard', 2, '₪#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('LBP', 0, 'Lebanese Pound', '422', 'ل.ل', 'ق', 100, 'standard', 2, 'ل.ل#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('SYP', 0, 'Syrian Pound', '760', '£', 'ق', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('IQD', 0, 'Iraqi Dinar', '368', 'ع.د', 'ف', 1000, 'standard', 3, 'ع.د#,##0.000', 'fiat', 'system', 'system.new_record', 'System seed data'),
('IRR', 0, 'Iranian Rial', '364', '﷼', 'd', 100, 'standard', 2, '﷼#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('YER', 0, 'Yemeni Rial', '886', '﷼', 'f', 100, 'standard', 2, '﷼#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),

-- Africa
('ZAR', 0, 'South African Rand', '710', 'R', 'c', 100, 'standard', 2, 'R#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('EGP', 0, 'Egyptian Pound', '818', '£', 'pt', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('NGN', 0, 'Nigerian Naira', '566', '₦', 'k', 100, 'standard', 2, '₦#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('KES', 0, 'Kenyan Shilling', '404', 'KSh', 'c', 100, 'standard', 2, 'KSh#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('GHS', 0, 'Ghanaian Cedi', '936', 'GH₵', 'p', 100, 'standard', 2, 'GH₵#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('TZS', 0, 'Tanzanian Shilling', '834', 'TSh', 'c', 100, 'standard', 2, 'TSh#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('UGX', 0, 'Ugandan Shilling', '800', 'USh', '', 0, 'standard', 0, 'USh#,##0', 'fiat', 'system', 'system.new_record', 'System seed data'),
('ETB', 0, 'Ethiopian Birr', '230', 'Br', 's', 100, 'standard', 2, 'Br#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MAD', 0, 'Moroccan Dirham', '504', 'د.م.', 's', 100, 'standard', 2, 'د.م.#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('TND', 0, 'Tunisian Dinar', '788', 'د.ت', 'm', 1000, 'standard', 3, 'د.ت#,##0.000', 'fiat', 'system', 'system.new_record', 'System seed data'),
('DZD', 0, 'Algerian Dinar', '012', 'د.ج', 's', 100, 'standard', 2, 'د.ج#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('LYD', 0, 'Libyan Dinar', '434', 'ل.د', 'd', 1000, 'standard', 3, 'ل.د#,##0.000', 'fiat', 'system', 'system.new_record', 'System seed data'),
('SDG', 0, 'Sudanese Pound', '938', 'ج.س', 'q', 100, 'standard', 2, 'ج.س#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('SSP', 0, 'South Sudanese Pound', '728', '£', 'p', 100, 'standard', 2, '£#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('AOA', 0, 'Angolan Kwanza', '973', 'Kz', 'c', 100, 'standard', 2, 'Kz#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('XAF', 0, 'Central African CFA Franc', '950', 'FCFA', 'c', 100, 'standard', 2, 'FCFA#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('XOF', 0, 'West African CFA Franc', '952', 'CFA', 'c', 100, 'standard', 2, 'CFA#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('ZMW', 0, 'Zambian Kwacha', '967', 'ZK', 'n', 100, 'standard', 2, 'ZK#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MZN', 0, 'Mozambican Metical', '943', 'MT', 'c', 100, 'standard', 2, 'MT#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BWP', 0, 'Botswana Pula', '072', 'P', 't', 100, 'standard', 2, 'P#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('NAD', 0, 'Namibian Dollar', '516', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('SZL', 0, 'Swazi Lilangeni', '748', 'L', 'c', 100, 'standard', 2, 'L#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('LSL', 0, 'Lesotho Loti', '426', 'L', 's', 100, 'standard', 2, 'L#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MWK', 0, 'Malawian Kwacha', '454', 'MK', 't', 100, 'standard', 2, 'MK#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('ZWL', 0, 'Zimbabwean Dollar', '932', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('RWF', 0, 'Rwandan Franc', '646', 'FRw', 'c', 100, 'standard', 2, 'FRw#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('BIF', 0, 'Burundian Franc', '108', 'FBu', 'c', 100, 'standard', 2, 'FBu#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('CDF', 0, 'Congolese Franc', '976', 'FC', 'c', 100, 'standard', 2, 'FC#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('DJF', 0, 'Djiboutian Franc', '262', 'Fdj', 'c', 100, 'standard', 2, 'Fdj#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('ERN', 0, 'Eritrean Nakfa', '232', 'Nfk', 'c', 100, 'standard', 2, 'Nfk#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('SOS', 0, 'Somali Shilling', '706', 'Sh', 'c', 100, 'standard', 2, 'Sh#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('GMD', 0, 'Gambian Dalasi', '270', 'D', 'b', 100, 'standard', 2, 'D#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('GNF', 0, 'Guinean Franc', '324', 'FG', 'c', 100, 'standard', 2, 'FG#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('SLL', 0, 'Sierra Leonean Leone', '694', 'Le', 'c', 100, 'standard', 2, 'Le#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('LRD', 0, 'Liberian Dollar', '430', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('CVE', 0, 'Cape Verdean Escudo', '132', '$', 'c', 100, 'standard', 2, '$#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('STN', 0, 'Sao Tome and Principe Dobra', '930', 'Db', 'c', 100, 'standard', 2, 'Db#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MRU', 0, 'Mauritanian Ouguiya', '929', 'UM', 'k', 5, 'standard', 1, 'UM#,##0.0', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MGA', 0, 'Malagasy Ariary', '969', 'Ar', 'ir', 5, 'standard', 1, 'Ar#,##0.0', 'fiat', 'system', 'system.new_record', 'System seed data'),
('MUR', 0, 'Mauritian Rupee', '480', '₨', 'c', 100, 'standard', 2, '₨#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('SCR', 0, 'Seychellois Rupee', '690', '₨', 'c', 100, 'standard', 2, '₨#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),
('KMF', 0, 'Comorian Franc', '174', 'CF', 'c', 100, 'standard', 2, 'CF#,##0.00', 'fiat', 'system', 'system.new_record', 'System seed data'),

-- Precious Metals (commodity currencies)
('XAU', 0, 'Gold (troy ounce)', '959', 'Au', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'system', 'system.new_record', 'System seed data'),
('XAG', 0, 'Silver (troy ounce)', '961', 'Ag', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'system', 'system.new_record', 'System seed data'),
('XPT', 0, 'Platinum (troy ounce)', '962', 'Pt', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'system', 'system.new_record', 'System seed data'),
('XPD', 0, 'Palladium (troy ounce)', '964', 'Pd', '', 0, 'none', 6, '#,##0.000000', 'commodity', 'system', 'system.new_record', 'System seed data'),

-- Supranational
('XDR', 0, 'Special Drawing Rights', '960', 'SDR', '', 0, 'standard', 6, 'SDR #,##0.000000', 'supranational', 'system', 'system.new_record', 'System seed data');
