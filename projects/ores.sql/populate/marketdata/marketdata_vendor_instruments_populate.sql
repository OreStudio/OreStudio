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
 * Market Data Vendor Instruments and Mappings
 *
 * Installs the instruments that the simulated feeds quote, in the notation of Bloomberg, of LSEG
 * and of the fictional provider Demo, and maps each vendor instrument to its oresmd item. The
 * three providers quote the same item, so a party that changes its provider keeps the series of
 * the item. The inventory page lists the rows and says which are marked for a check against the
 * vendor.
 *
 * Two markets are installed. The FX spot pairs are the pairs the FX feeds quote. The interest rate
 * pillars are one row for each pillar of each curve of the synthetic.feeds.uniform_demo dataset:
 * 20 currencies, each with a deposit, a forward rate agreement and a swap, for each of the three
 * providers. The providers ICE and Tradeweb have no rows yet.
 *
 * No id here is minted: each follows the vendor's own notation. The text of each id, and the
 * field it carries, is what the notation of its provider writes for that quote, so the notation
 * reads every row back as the quote the row stands for.
 *
 * A pillar's oresmd item is the datum the curve's own pillar key names: the currency, the index
 * tenor the curve holds, and the tenors the pillar starts and ends at, which is the item both the
 * feed that publishes the pillar and the bootstrap that reads it ask for.
 *
 * The ids of the vendor instruments are derived from their identity, so a rerun writes the same
 * rows. An instrument that already has a current mapping gets no second one, whatever its date.
 * This script is idempotent.
 */

\echo '--- Market Data Vendor Instruments and Mappings ---'

-- The temporary table lives inside one block, because psql runs each statement in a transaction
-- of its own and a table that drops on commit would be gone before the second insert.
do $$
begin
create temporary table t_vendor_inventory (
    provider_code text, id_scheme text, external_id text, field text, oresmd_uri text
) on commit drop;

-- The spot quotes of the FX pairs.
insert into t_vendor_inventory (provider_code, id_scheme, external_id, field, oresmd_uri) values
    ('bloomberg', 'bbg_ticker', 'EURUSD Curncy', 'PX_LAST', 'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('lseg', 'ric', 'EUR=', 'BID', 'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('demo', 'demo_key', 'EUR/USD', 'value', 'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('bloomberg', 'bbg_ticker', 'GBPUSD Curncy', 'PX_LAST', 'oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('lseg', 'ric', 'GBP=', 'BID', 'oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('demo', 'demo_key', 'GBP/USD', 'value', 'oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('bloomberg', 'bbg_ticker', 'AUDUSD Curncy', 'PX_LAST', 'oresmd://fx/AUD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('lseg', 'ric', 'AUD=', 'BID', 'oresmd://fx/AUD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('demo', 'demo_key', 'AUD/USD', 'value', 'oresmd://fx/AUD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('bloomberg', 'bbg_ticker', 'NZDUSD Curncy', 'PX_LAST', 'oresmd://fx/NZD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('lseg', 'ric', 'NZD=', 'BID', 'oresmd://fx/NZD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('demo', 'demo_key', 'NZD/USD', 'value', 'oresmd://fx/NZD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('bloomberg', 'bbg_ticker', 'USDCAD Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CAD'),
    ('lseg', 'ric', 'CAD=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CAD'),
    ('demo', 'demo_key', 'USD/CAD', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CAD'),
    ('bloomberg', 'bbg_ticker', 'USDCHF Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CHF'),
    ('lseg', 'ric', 'CHF=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CHF'),
    ('demo', 'demo_key', 'USD/CHF', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CHF'),
    ('bloomberg', 'bbg_ticker', 'USDDKK Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=DKK'),
    ('lseg', 'ric', 'DKK=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=DKK'),
    ('demo', 'demo_key', 'USD/DKK', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=DKK'),
    ('bloomberg', 'bbg_ticker', 'USDINR Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=INR'),
    ('lseg', 'ric', 'INR=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=INR'),
    ('demo', 'demo_key', 'USD/INR', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=INR'),
    ('bloomberg', 'bbg_ticker', 'USDJPY Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=JPY'),
    ('lseg', 'ric', 'JPY=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=JPY'),
    ('demo', 'demo_key', 'USD/JPY', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=JPY'),
    ('bloomberg', 'bbg_ticker', 'USDMXN Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=MXN'),
    ('lseg', 'ric', 'MXN=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=MXN'),
    ('demo', 'demo_key', 'USD/MXN', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=MXN'),
    ('bloomberg', 'bbg_ticker', 'USDNOK Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=NOK'),
    ('lseg', 'ric', 'NOK=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=NOK'),
    ('demo', 'demo_key', 'USD/NOK', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=NOK'),
    ('bloomberg', 'bbg_ticker', 'USDSEK Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SEK'),
    ('lseg', 'ric', 'SEK=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SEK'),
    ('demo', 'demo_key', 'USD/SEK', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SEK'),
    ('bloomberg', 'bbg_ticker', 'USDZAR Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ZAR'),
    ('lseg', 'ric', 'ZAR=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ZAR'),
    ('demo', 'demo_key', 'USD/ZAR', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ZAR'),
    ('bloomberg', 'bbg_ticker', 'USDHKD Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HKD'),
    ('lseg', 'ric', 'HKD=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HKD'),
    ('demo', 'demo_key', 'USD/HKD', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HKD'),
    ('bloomberg', 'bbg_ticker', 'USDSGD Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SGD'),
    ('lseg', 'ric', 'SGD=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SGD'),
    ('demo', 'demo_key', 'USD/SGD', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SGD'),
    ('bloomberg', 'bbg_ticker', 'USDCNH Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CNH'),
    ('lseg', 'ric', 'CNH=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CNH'),
    ('demo', 'demo_key', 'USD/CNH', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CNH'),
    ('bloomberg', 'bbg_ticker', 'USDKRW Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=KRW'),
    ('lseg', 'ric', 'KRW=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=KRW'),
    ('demo', 'demo_key', 'USD/KRW', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=KRW'),
    ('bloomberg', 'bbg_ticker', 'USDTWD Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TWD'),
    ('lseg', 'ric', 'TWD=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TWD'),
    ('demo', 'demo_key', 'USD/TWD', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TWD'),
    ('bloomberg', 'bbg_ticker', 'USDPLN Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=PLN'),
    ('lseg', 'ric', 'PLN=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=PLN'),
    ('demo', 'demo_key', 'USD/PLN', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=PLN'),
    ('bloomberg', 'bbg_ticker', 'USDCZK Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CZK'),
    ('lseg', 'ric', 'CZK=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CZK'),
    ('demo', 'demo_key', 'USD/CZK', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CZK'),
    ('bloomberg', 'bbg_ticker', 'USDHUF Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HUF'),
    ('lseg', 'ric', 'HUF=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HUF'),
    ('demo', 'demo_key', 'USD/HUF', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HUF'),
    ('bloomberg', 'bbg_ticker', 'USDTRY Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TRY'),
    ('lseg', 'ric', 'TRY=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TRY'),
    ('demo', 'demo_key', 'USD/TRY', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TRY'),
    ('bloomberg', 'bbg_ticker', 'USDILS Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ILS'),
    ('lseg', 'ric', 'ILS=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ILS'),
    ('demo', 'demo_key', 'USD/ILS', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ILS'),
    ('bloomberg', 'bbg_ticker', 'USDBRL Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=BRL'),
    ('lseg', 'ric', 'BRL=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=BRL'),
    ('demo', 'demo_key', 'USD/BRL', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=BRL'),
    ('bloomberg', 'bbg_ticker', 'USDCLP Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CLP'),
    ('lseg', 'ric', 'CLP=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CLP'),
    ('demo', 'demo_key', 'USD/CLP', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CLP'),
    ('bloomberg', 'bbg_ticker', 'USDCOP Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=COP'),
    ('lseg', 'ric', 'COP=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=COP'),
    ('demo', 'demo_key', 'USD/COP', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=COP');

-- The pillars of the interest rate curves: for each currency and index of the uniform demo
-- dataset, the deposit, the forward rate agreement and the swap that start and end it.
insert into t_vendor_inventory (provider_code, id_scheme, external_id, field, oresmd_uri) values
    ('bloomberg', 'bbg_ticker', 'USD SOFR DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'USD SOFR FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'USD SOFR IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'EUR ESTR DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'EUR ESTR FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'EUR ESTR IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'JPY TONA DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'JPY TONA FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'JPY TONA IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'GBP SONIA DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'GBP SONIA FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'GBP SONIA IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'CHF SARON DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'CHF SARON FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'CHF SARON IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'AUD AONIA DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'AUD AONIA FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'AUD AONIA IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'CAD CORRA DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'CAD CORRA FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'CAD CORRA IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'CNY SHIBOR DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'CNY SHIBOR FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'CNY SHIBOR IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'HKD HONIA DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'HKD HONIA FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'HKD HONIA IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'SGD SORA DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'SGD SORA FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'SGD SORA IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'SEK SWESTR DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'SEK SWESTR FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'SEK SWESTR IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'NOK NOWA DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'NOK NOWA FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'NOK NOWA IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'NZD NZONIA DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'NZD NZONIA FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'NZD NZONIA IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'KRW KOFR DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'KRW KOFR FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'KRW KOFR IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'INR MIBOR DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'INR MIBOR FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'INR MIBOR IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'MXN TIIE DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'MXN TIIE FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'MXN TIIE IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'ZAR ZARONIA DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'ZAR ZARONIA FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'ZAR ZARONIA IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'DKK DESTR DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'DKK DESTR FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'DKK DESTR IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'PLN POLONIA DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'PLN POLONIA FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'PLN POLONIA IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'bbg_ticker', 'TWD TAIBOR DEPO SPOT 3M Curncy', 'PX_LAST',
     'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'bbg_ticker', 'TWD TAIBOR FRA 3M 6M Curncy', 'PX_LAST',
     'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('bloomberg', 'bbg_ticker', 'TWD TAIBOR IRS SPOT 2Y Curncy', 'PX_LAST',
     'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'USDSOFR.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'USDSOFR.FRA.3M.6M=', 'BID',
     'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'USDSOFR.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'EURESTR.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'EURESTR.FRA.3M.6M=', 'BID',
     'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'EURESTR.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'JPYTONA.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'JPYTONA.FRA.3M.6M=', 'BID',
     'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'JPYTONA.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'GBPSONIA.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'GBPSONIA.FRA.3M.6M=', 'BID',
     'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'GBPSONIA.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'CHFSARON.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'CHFSARON.FRA.3M.6M=', 'BID',
     'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'CHFSARON.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'AUDAONIA.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'AUDAONIA.FRA.3M.6M=', 'BID',
     'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'AUDAONIA.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'CADCORRA.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'CADCORRA.FRA.3M.6M=', 'BID',
     'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'CADCORRA.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'CNYSHIBOR.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'CNYSHIBOR.FRA.3M.6M=', 'BID',
     'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'CNYSHIBOR.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'HKDHONIA.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'HKDHONIA.FRA.3M.6M=', 'BID',
     'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'HKDHONIA.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'SGDSORA.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'SGDSORA.FRA.3M.6M=', 'BID',
     'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'SGDSORA.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'SEKSWESTR.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'SEKSWESTR.FRA.3M.6M=', 'BID',
     'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'SEKSWESTR.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'NOKNOWA.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'NOKNOWA.FRA.3M.6M=', 'BID',
     'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'NOKNOWA.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'NZDNZONIA.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'NZDNZONIA.FRA.3M.6M=', 'BID',
     'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'NZDNZONIA.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'KRWKOFR.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'KRWKOFR.FRA.3M.6M=', 'BID',
     'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'KRWKOFR.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'INRMIBOR.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'INRMIBOR.FRA.3M.6M=', 'BID',
     'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'INRMIBOR.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'MXNTIIE.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'MXNTIIE.FRA.3M.6M=', 'BID',
     'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'MXNTIIE.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'ZARZARONIA.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'ZARZARONIA.FRA.3M.6M=', 'BID',
     'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'ZARZARONIA.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'DKKDESTR.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'DKKDESTR.FRA.3M.6M=', 'BID',
     'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'DKKDESTR.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'PLNPOLONIA.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'PLNPOLONIA.FRA.3M.6M=', 'BID',
     'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'PLNPOLONIA.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('lseg', 'ric', 'TWDTAIBOR.DEPO.SPOT.3M=', 'BID',
     'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('lseg', 'ric', 'TWDTAIBOR.FRA.3M.6M=', 'BID',
     'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('lseg', 'ric', 'TWDTAIBOR.IRS.SPOT.2Y=', 'BID',
     'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'USD/sofr/DEPO/SPOT/3M', 'value',
     'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'USD/sofr/FRA/3M/6M', 'value',
     'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'USD/sofr/IRS/SPOT/2Y', 'value',
     'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'EUR/estr/DEPO/SPOT/3M', 'value',
     'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'EUR/estr/FRA/3M/6M', 'value',
     'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'EUR/estr/IRS/SPOT/2Y', 'value',
     'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'JPY/tona/DEPO/SPOT/3M', 'value',
     'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'JPY/tona/FRA/3M/6M', 'value',
     'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'JPY/tona/IRS/SPOT/2Y', 'value',
     'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'GBP/sonia/DEPO/SPOT/3M', 'value',
     'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'GBP/sonia/FRA/3M/6M', 'value',
     'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'GBP/sonia/IRS/SPOT/2Y', 'value',
     'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'CHF/saron/DEPO/SPOT/3M', 'value',
     'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'CHF/saron/FRA/3M/6M', 'value',
     'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'CHF/saron/IRS/SPOT/2Y', 'value',
     'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'AUD/aonia/DEPO/SPOT/3M', 'value',
     'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'AUD/aonia/FRA/3M/6M', 'value',
     'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'AUD/aonia/IRS/SPOT/2Y', 'value',
     'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'CAD/corra/DEPO/SPOT/3M', 'value',
     'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'CAD/corra/FRA/3M/6M', 'value',
     'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'CAD/corra/IRS/SPOT/2Y', 'value',
     'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'CNY/shibor/DEPO/SPOT/3M', 'value',
     'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'CNY/shibor/FRA/3M/6M', 'value',
     'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'CNY/shibor/IRS/SPOT/2Y', 'value',
     'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'HKD/honia/DEPO/SPOT/3M', 'value',
     'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'HKD/honia/FRA/3M/6M', 'value',
     'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'HKD/honia/IRS/SPOT/2Y', 'value',
     'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'SGD/sora/DEPO/SPOT/3M', 'value',
     'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'SGD/sora/FRA/3M/6M', 'value',
     'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'SGD/sora/IRS/SPOT/2Y', 'value',
     'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'SEK/swestr/DEPO/SPOT/3M', 'value',
     'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'SEK/swestr/FRA/3M/6M', 'value',
     'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'SEK/swestr/IRS/SPOT/2Y', 'value',
     'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'NOK/nowa/DEPO/SPOT/3M', 'value',
     'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'NOK/nowa/FRA/3M/6M', 'value',
     'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'NOK/nowa/IRS/SPOT/2Y', 'value',
     'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'NZD/nzonia/DEPO/SPOT/3M', 'value',
     'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'NZD/nzonia/FRA/3M/6M', 'value',
     'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'NZD/nzonia/IRS/SPOT/2Y', 'value',
     'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'KRW/kofr/DEPO/SPOT/3M', 'value',
     'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'KRW/kofr/FRA/3M/6M', 'value',
     'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'KRW/kofr/IRS/SPOT/2Y', 'value',
     'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'INR/mibor/DEPO/SPOT/3M', 'value',
     'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'INR/mibor/FRA/3M/6M', 'value',
     'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'INR/mibor/IRS/SPOT/2Y', 'value',
     'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'MXN/tiie/DEPO/SPOT/3M', 'value',
     'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'MXN/tiie/FRA/3M/6M', 'value',
     'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'MXN/tiie/IRS/SPOT/2Y', 'value',
     'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'ZAR/zaronia/DEPO/SPOT/3M', 'value',
     'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'ZAR/zaronia/FRA/3M/6M', 'value',
     'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'ZAR/zaronia/IRS/SPOT/2Y', 'value',
     'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'DKK/destr/DEPO/SPOT/3M', 'value',
     'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'DKK/destr/FRA/3M/6M', 'value',
     'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'DKK/destr/IRS/SPOT/2Y', 'value',
     'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'PLN/polonia/DEPO/SPOT/3M', 'value',
     'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'PLN/polonia/FRA/3M/6M', 'value',
     'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'PLN/polonia/IRS/SPOT/2Y', 'value',
     'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'demo_key', 'TWD/taibor/DEPO/SPOT/3M', 'value',
     'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'demo_key', 'TWD/taibor/FRA/3M/6M', 'value',
     'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'demo_key', 'TWD/taibor/IRS/SPOT/2Y', 'value',
     'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y');

insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted, deviation_note,
    modified_by, performed_by, change_reason_code, change_commentary
)
select md5(t.provider_code || '|' || t.id_scheme || '|' || t.external_id || '|' || t.field)::uuid,
       ores_utility_system_tenant_id_fn(), 0, t.provider_code, t.id_scheme, t.external_id, t.field,
       false, '', current_user, current_user, 'system.initial_load',
       'Initial population of the FX spot and interest rate pillar vendor instruments'
from t_vendor_inventory t
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
       'Initial population of the FX spot and interest rate pillar mappings'
from t_vendor_inventory t
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

-- Summary
select 'marketdata_vendor_instruments' as entity, count(*) as count
from ores_marketdata_market_data_vendor_instruments_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'marketdata_mappings', count(*)
from ores_marketdata_market_data_mappings_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
