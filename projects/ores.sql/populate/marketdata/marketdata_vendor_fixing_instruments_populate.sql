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
 * Market Data Vendor Instruments of the Simulated Fixings
 *
 * Installs the ids of the fixings the simulator publishes, under the provider simulated-fixings,
 * and maps each to its fixing address. The simulated fixing notation names a fixing by that
 * address, which carries the index's market name. The fixings are the index of each curve config
 * of the synthetic seeds, and each pair a spot config quotes at the fixing source of the pair's
 * default cut. It runs after the currency pairs are staged, because it reads their default cuts.
 *
 * No id is written by hand. tools/vendor_catalogue writes the block between the markers below,
 * from the query in the header of marketdata_vendor_instruments_populate.sql: it writes each
 * address with the curve feed's own function or the datum codec, calls the notation to write
 * each id, and reads every id back.
 *
 * This script is idempotent.
 */

\echo '--- Market Data Vendor Instruments of the Simulated Fixings ---'

do $$
declare
    v_stale_fixing text;
begin
create temporary table t_fixing_inventory (
    provider_code text, id_scheme text, external_id text, field text, oresmd_uri text
) on commit drop;

-- The fixings the simulator publishes, in the simulated fixing notation.
create temporary table t_fixing_id (
    kind text, key_a text, key_b text, key_c text,
    id_scheme text, external_id text, field text, oresmd_uri text
) on commit drop;

-- BEGIN GENERATED fixing ids: tools/vendor_catalogue writes this block, see the header
    -- simulated-fixings: 82 with an id, 1 without
    insert into t_fixing_id (kind, key_a, key_b, key_c,
        id_scheme, external_id, field, oresmd_uri) values
    ('fx', 'AUD', 'JPY', 'WMR1000NYC', 'oresmd', 'oresmd://fx/AUD?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY', 'value', 'oresmd://fx/AUD?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY'),
    ('fx', 'AUD', 'NZD', 'WMR1000NYC', 'oresmd', 'oresmd://fx/AUD?type=fixing&index=fx&source=WMR1000NYC&ccy=NZD', 'value', 'oresmd://fx/AUD?type=fixing&index=fx&source=WMR1000NYC&ccy=NZD'),
    ('fx', 'AUD', 'USD', 'WMR1000NYC', 'oresmd', 'oresmd://fx/AUD?type=fixing&index=fx&source=WMR1000NYC&ccy=USD', 'value', 'oresmd://fx/AUD?type=fixing&index=fx&source=WMR1000NYC&ccy=USD'),
    ('fx', 'CAD', 'JPY', 'WMR1000NYC', 'oresmd', 'oresmd://fx/CAD?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY', 'value', 'oresmd://fx/CAD?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY'),
    ('fx', 'CHF', 'JPY', 'WMR1000NYC', 'oresmd', 'oresmd://fx/CHF?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY', 'value', 'oresmd://fx/CHF?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY'),
    ('fx', 'EUR', 'AUD', 'WMR1000NYC', 'oresmd', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=AUD', 'value', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=AUD'),
    ('fx', 'EUR', 'CAD', 'WMR1000NYC', 'oresmd', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=CAD', 'value', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=CAD'),
    ('fx', 'EUR', 'CHF', 'WMR1000NYC', 'oresmd', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=CHF', 'value', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=CHF'),
    ('fx', 'EUR', 'GBP', 'WMR1000NYC', 'oresmd', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=GBP', 'value', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=GBP'),
    ('fx', 'EUR', 'JPY', 'WMR1000NYC', 'oresmd', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY', 'value', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY'),
    ('fx', 'EUR', 'NOK', 'WMR1000NYC', 'oresmd', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=NOK', 'value', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=NOK'),
    ('fx', 'EUR', 'SEK', 'WMR1000NYC', 'oresmd', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=SEK', 'value', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=SEK'),
    ('fx', 'EUR', 'USD', 'WMR1000NYC', 'oresmd', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=USD', 'value', 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=USD'),
    ('fx', 'GBP', 'CHF', 'WMR1000NYC', 'oresmd', 'oresmd://fx/GBP?type=fixing&index=fx&source=WMR1000NYC&ccy=CHF', 'value', 'oresmd://fx/GBP?type=fixing&index=fx&source=WMR1000NYC&ccy=CHF'),
    ('fx', 'GBP', 'JPY', 'WMR1000NYC', 'oresmd', 'oresmd://fx/GBP?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY', 'value', 'oresmd://fx/GBP?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY'),
    ('fx', 'GBP', 'USD', 'WMR1000NYC', 'oresmd', 'oresmd://fx/GBP?type=fixing&index=fx&source=WMR1000NYC&ccy=USD', 'value', 'oresmd://fx/GBP?type=fixing&index=fx&source=WMR1000NYC&ccy=USD'),
    ('fx', 'NZD', 'JPY', 'WMR1000NYC', 'oresmd', 'oresmd://fx/NZD?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY', 'value', 'oresmd://fx/NZD?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY'),
    ('fx', 'NZD', 'USD', 'WMR1000NYC', 'oresmd', 'oresmd://fx/NZD?type=fixing&index=fx&source=WMR1000NYC&ccy=USD', 'value', 'oresmd://fx/NZD?type=fixing&index=fx&source=WMR1000NYC&ccy=USD'),
    ('fx', 'USD', 'BRL', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=BRL', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=BRL'),
    ('fx', 'USD', 'CAD', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=CAD', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=CAD'),
    ('fx', 'USD', 'CHF', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=CHF', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=CHF'),
    ('fx', 'USD', 'CLP', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=CLP', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=CLP'),
    ('fx', 'USD', 'COP', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=COP', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=COP'),
    ('fx', 'USD', 'CZK', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=CZK', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=CZK'),
    ('fx', 'USD', 'HKD', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=HKD', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=HKD'),
    ('fx', 'USD', 'HUF', 'WMR1200BUD', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1200BUD&ccy=HUF', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1200BUD&ccy=HUF'),
    ('fx', 'USD', 'ILS', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=ILS', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=ILS'),
    ('fx', 'USD', 'INR', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=INR', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=INR'),
    ('fx', 'USD', 'JPY', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY'),
    ('fx', 'USD', 'KRW', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=KRW', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=KRW'),
    ('fx', 'USD', 'MXN', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=MXN', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=MXN'),
    ('fx', 'USD', 'PLN', 'WMR1100WAR', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1100WAR&ccy=PLN', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1100WAR&ccy=PLN'),
    ('fx', 'USD', 'SGD', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=SGD', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=SGD'),
    ('fx', 'USD', 'TRY', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=TRY', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=TRY'),
    ('fx', 'USD', 'TWD', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=TWD', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=TWD'),
    ('fx', 'USD', 'ZAR', 'WMR1000NYC', 'oresmd', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=ZAR', 'value', 'oresmd://fx/USD?type=fixing&index=fx&source=WMR1000NYC&ccy=ZAR'),
    ('ir', 'AUD', 'aonia', '', 'oresmd', 'oresmd://ir/AUD?type=fixing&index=ibor&name=AONIA', 'value', 'oresmd://ir/AUD?type=fixing&index=ibor&name=AONIA'),
    ('ir', 'BRL', 'cdi', '', 'oresmd', 'oresmd://ir/BRL?type=fixing&index=ibor&name=CDI', 'value', 'oresmd://ir/BRL?type=fixing&index=ibor&name=CDI'),
    ('ir', 'CAD', 'corra', '', 'oresmd', 'oresmd://ir/CAD?type=fixing&index=ibor&name=CORRA', 'value', 'oresmd://ir/CAD?type=fixing&index=ibor&name=CORRA'),
    ('ir', 'CHF', 'libor', '3M', 'oresmd', 'oresmd://ir/CHF?type=fixing&index=ibor&name=LIBOR&tenor=3M', 'value', 'oresmd://ir/CHF?type=fixing&index=ibor&name=LIBOR&tenor=3M'),
    ('ir', 'CHF', 'libor', '6M', 'oresmd', 'oresmd://ir/CHF?type=fixing&index=ibor&name=LIBOR&tenor=6M', 'value', 'oresmd://ir/CHF?type=fixing&index=ibor&name=LIBOR&tenor=6M'),
    ('ir', 'CHF', 'saron', '', 'oresmd', 'oresmd://ir/CHF?type=fixing&index=ibor&name=SARON', 'value', 'oresmd://ir/CHF?type=fixing&index=ibor&name=SARON'),
    ('ir', 'CLP', 'camara', '', 'oresmd', 'oresmd://ir/CLP?type=fixing&index=ibor&name=CAMARA', 'value', 'oresmd://ir/CLP?type=fixing&index=ibor&name=CAMARA'),
    ('ir', 'CNH', 'hibor', '', 'oresmd', 'oresmd://ir/CNH?type=fixing&index=ibor&name=HIBOR', 'value', 'oresmd://ir/CNH?type=fixing&index=ibor&name=HIBOR'),
    ('ir', 'CNY', 'shibor', '', 'oresmd', 'oresmd://ir/CNY?type=fixing&index=ibor&name=SHIBOR', 'value', 'oresmd://ir/CNY?type=fixing&index=ibor&name=SHIBOR'),
    ('ir', 'COP', 'ibr', '', 'oresmd', 'oresmd://ir/COP?type=fixing&index=ibor&name=IBR', 'value', 'oresmd://ir/COP?type=fixing&index=ibor&name=IBR'),
    ('ir', 'CZK', 'czeonia', '', 'oresmd', 'oresmd://ir/CZK?type=fixing&index=ibor&name=CZEONIA', 'value', 'oresmd://ir/CZK?type=fixing&index=ibor&name=CZEONIA'),
    ('ir', 'DKK', 'destr', '', 'oresmd', 'oresmd://ir/DKK?type=fixing&index=ibor&name=DESTR', 'value', 'oresmd://ir/DKK?type=fixing&index=ibor&name=DESTR'),
    ('ir', 'EUR', 'estr', '', 'oresmd', 'oresmd://ir/EUR?type=fixing&index=ibor&name=ESTR', 'value', 'oresmd://ir/EUR?type=fixing&index=ibor&name=ESTR'),
    ('ir', 'EUR', 'euribor', '12M', 'oresmd', 'oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=12M', 'value', 'oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=12M'),
    ('ir', 'EUR', 'euribor', '1M', 'oresmd', 'oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=1M', 'value', 'oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=1M'),
    ('ir', 'EUR', 'euribor', '1W', 'oresmd', 'oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=1W', 'value', 'oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=1W'),
    ('ir', 'EUR', 'euribor', '6M', 'oresmd', 'oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M', 'value', 'oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M'),
    ('ir', 'GBP', 'libor', '12M', 'oresmd', 'oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=12M', 'value', 'oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=12M'),
    ('ir', 'GBP', 'libor', '1M', 'oresmd', 'oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=1M', 'value', 'oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=1M'),
    ('ir', 'GBP', 'libor', '1W', 'oresmd', 'oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=1W', 'value', 'oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=1W'),
    ('ir', 'GBP', 'libor', '3M', 'oresmd', 'oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=3M', 'value', 'oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=3M'),
    ('ir', 'GBP', 'libor', '6M', 'oresmd', 'oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=6M', 'value', 'oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=6M'),
    ('ir', 'GBP', 'sonia', '', 'oresmd', 'oresmd://ir/GBP?type=fixing&index=ibor&name=SONIA', 'value', 'oresmd://ir/GBP?type=fixing&index=ibor&name=SONIA'),
    ('ir', 'HKD', 'hibor', '1M', 'oresmd', 'oresmd://ir/HKD?type=fixing&index=ibor&name=HIBOR&tenor=1M', 'value', 'oresmd://ir/HKD?type=fixing&index=ibor&name=HIBOR&tenor=1M'),
    ('ir', 'HKD', 'hibor', '2W', 'oresmd', 'oresmd://ir/HKD?type=fixing&index=ibor&name=HIBOR&tenor=2W', 'value', 'oresmd://ir/HKD?type=fixing&index=ibor&name=HIBOR&tenor=2W'),
    ('ir', 'HKD', 'hibor', '3M', 'oresmd', 'oresmd://ir/HKD?type=fixing&index=ibor&name=HIBOR&tenor=3M', 'value', 'oresmd://ir/HKD?type=fixing&index=ibor&name=HIBOR&tenor=3M'),
    ('ir', 'HKD', 'honia', '', 'oresmd', 'oresmd://ir/HKD?type=fixing&index=ibor&name=HONIA', 'value', 'oresmd://ir/HKD?type=fixing&index=ibor&name=HONIA'),
    ('ir', 'HUF', 'hufonia', '', 'oresmd', 'oresmd://ir/HUF?type=fixing&index=ibor&name=HUFONIA', 'value', 'oresmd://ir/HUF?type=fixing&index=ibor&name=HUFONIA'),
    ('ir', 'ILS', 'shir', '', 'oresmd', 'oresmd://ir/ILS?type=fixing&index=ibor&name=SHIR', 'value', 'oresmd://ir/ILS?type=fixing&index=ibor&name=SHIR'),
    ('ir', 'INR', 'mibor', '', 'oresmd', 'oresmd://ir/INR?type=fixing&index=ibor&name=MIBOR', 'value', 'oresmd://ir/INR?type=fixing&index=ibor&name=MIBOR'),
    ('ir', 'JPY', 'libor', '1M', 'oresmd', 'oresmd://ir/JPY?type=fixing&index=ibor&name=LIBOR&tenor=1M', 'value', 'oresmd://ir/JPY?type=fixing&index=ibor&name=LIBOR&tenor=1M'),
    ('ir', 'JPY', 'libor', '3M', 'oresmd', 'oresmd://ir/JPY?type=fixing&index=ibor&name=LIBOR&tenor=3M', 'value', 'oresmd://ir/JPY?type=fixing&index=ibor&name=LIBOR&tenor=3M'),
    ('ir', 'JPY', 'tona', '', 'oresmd', 'oresmd://ir/JPY?type=fixing&index=ibor&name=TONA', 'value', 'oresmd://ir/JPY?type=fixing&index=ibor&name=TONA'),
    ('ir', 'KRW', 'kofr', '', 'oresmd', 'oresmd://ir/KRW?type=fixing&index=ibor&name=KOFR', 'value', 'oresmd://ir/KRW?type=fixing&index=ibor&name=KOFR'),
    ('ir', 'MXN', 'ftiie', '', 'oresmd', 'oresmd://ir/MXN?type=fixing&index=ibor&name=FTIIE', 'value', 'oresmd://ir/MXN?type=fixing&index=ibor&name=FTIIE'),
    ('ir', 'NOK', 'nowa', '', 'oresmd', 'oresmd://ir/NOK?type=fixing&index=ibor&name=NOWA', 'value', 'oresmd://ir/NOK?type=fixing&index=ibor&name=NOWA'),
    ('ir', 'NZD', 'nzonia', '', 'oresmd', 'oresmd://ir/NZD?type=fixing&index=ibor&name=NZONIA', 'value', 'oresmd://ir/NZD?type=fixing&index=ibor&name=NZONIA'),
    ('ir', 'PLN', 'polonia', '', 'oresmd', 'oresmd://ir/PLN?type=fixing&index=ibor&name=POLONIA', 'value', 'oresmd://ir/PLN?type=fixing&index=ibor&name=POLONIA'),
    ('ir', 'SEK', 'swestr', '', 'oresmd', 'oresmd://ir/SEK?type=fixing&index=ibor&name=SWESTR', 'value', 'oresmd://ir/SEK?type=fixing&index=ibor&name=SWESTR'),
    ('ir', 'SGD', 'sora', '', 'oresmd', 'oresmd://ir/SGD?type=fixing&index=ibor&name=SORA', 'value', 'oresmd://ir/SGD?type=fixing&index=ibor&name=SORA'),
    ('ir', 'TRY', 'tlref', '', 'oresmd', 'oresmd://ir/TRY?type=fixing&index=ibor&name=TLREF', 'value', 'oresmd://ir/TRY?type=fixing&index=ibor&name=TLREF'),
    ('ir', 'TWD', 'taibor', '', 'oresmd', 'oresmd://ir/TWD?type=fixing&index=ibor&name=TAIBOR', 'value', 'oresmd://ir/TWD?type=fixing&index=ibor&name=TAIBOR'),
    ('ir', 'USD', 'libor', '1M', 'oresmd', 'oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=1M', 'value', 'oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=1M'),
    ('ir', 'USD', 'libor', '6M', 'oresmd', 'oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=6M', 'value', 'oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=6M'),
    ('ir', 'USD', 'sofr', '', 'oresmd', 'oresmd://ir/USD?type=fixing&index=ibor&name=SOFR', 'value', 'oresmd://ir/USD?type=fixing&index=ibor&name=SOFR'),
    ('ir', 'USD', 'sofr', 'FOMC', null, null, null, null),
    ('ir', 'ZAR', 'zaronia', '', 'oresmd', 'oresmd://ir/ZAR?type=fixing&index=ibor&name=ZARONIA', 'value', 'oresmd://ir/ZAR?type=fixing&index=ibor&name=ZARONIA');
-- END GENERATED fixing ids

-- The fixings the seeds' configs state: the index of each curve, and each spot-fed pair at the
-- source that fixes its default cut.
create temporary table t_fixing_feed (
    kind text, key_a text, key_b text, key_c text
) on commit drop;

insert into t_fixing_feed
select distinct 'ir', c.currency_code, c.index_family, c.tenor
from ores_dq_ir_curve_generation_configs_artefact_tbl c
where c.provider_code is not null
union
select distinct 'fx', s.base_currency_code, s.quote_currency_code, k.fixing_source_code
from ores_dq_fx_spot_generation_configs_artefact_tbl s
join ores_dq_currency_pairs_artefact_tbl p
  on p.base_currency = s.base_currency_code and p.quote_currency = s.quote_currency_code
join ores_refdata_cut_codes_tbl k
  on k.code = p.default_cut_code and k.valid_to = ores_utility_infinity_timestamp_fn()
where s.provider_code is not null;

select string_agg(distinct f.kind || ' ' || f.key_a || ' ' || f.key_b || ' ' || f.key_c, '; ')
into v_stale_fixing
from t_fixing_feed f
where not exists (
    select 1 from t_fixing_id i
    where i.kind = f.kind and i.key_a = f.key_a and i.key_b = f.key_b and i.key_c = f.key_c
);

if v_stale_fixing is not null then
    raise exception 'The configs state fixings the vendor instrument seed has no id for: %. Regenerate the seed: see the header of marketdata_vendor_instruments_populate.sql.', v_stale_fixing;
end if;

insert into t_fixing_inventory (provider_code, id_scheme, external_id, field, oresmd_uri)
select distinct 'simulated-fixings', i.id_scheme, i.external_id, i.field, i.oresmd_uri
from t_fixing_feed f
join t_fixing_id i
  on i.kind = f.kind and i.key_a = f.key_a and i.key_b = f.key_b and i.key_c = f.key_c
where i.external_id is not null;

insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted, deviation_note,
    modified_by, performed_by, change_reason_code, change_commentary
)
select md5(t.provider_code || '|' || t.id_scheme || '|' || t.external_id || '|' || t.field)::uuid,
       ores_utility_system_tenant_id_fn(), 0, t.provider_code, t.id_scheme, t.external_id, t.field,
       false, '', current_user, current_user, 'system.initial_load',
       'Initial population of the simulated fixing vendor instruments'
from t_fixing_inventory t
where not exists (
    select 1 from ores_marketdata_market_data_vendor_instruments_tbl v
    where v.tenant_id = ores_utility_system_tenant_id_fn()
      and v.provider_code = t.provider_code
      and v.id_scheme = t.id_scheme
      and v.external_id = t.external_id
      and v.field = t.field
      and v.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_market_data_mappings_tbl (
    id, tenant_id, version, vendor_instrument_id, oresmd_uri, effective_from,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v.id, t.oresmd_uri,
       timestamptz '2026-01-01 00:00:00+00', current_user, current_user, 'system.initial_load',
       'Initial population of the simulated fixing mappings'
from t_fixing_inventory t
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.tenant_id = ores_utility_system_tenant_id_fn()
 and v.provider_code = t.provider_code
 and v.id_scheme = t.id_scheme
 and v.external_id = t.external_id
 and v.field = t.field
 and v.valid_to = ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_marketdata_market_data_mappings_tbl m
    where m.tenant_id = ores_utility_system_tenant_id_fn()
      and m.vendor_instrument_id = v.id
      and m.valid_to = ores_utility_infinity_timestamp_fn()
);
end $$;
