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
 * The FX spot pairs are the pairs the FX feeds quote. Each is one row for each of the three
 * providers, and the mapping of a pair is the item the pair's own quote names.
 *
 * The interest rate pillars are the pillars of the IR feeds themselves, and no list of them is
 * written down here. The seed reads every IR curve config of every DQ dataset the synthetic seeds
 * write, and for each of its Curve Template entries it emits one vendor instrument and one mapping
 * for the config's provider: the currency, the index family, the instrument and the two tenors of
 * the entry are the quote, and the currency, the index tenor of the curve and those tenors are the
 * item's key. A provider's catalogue is therefore exactly the set of ids its feeds can serve. No
 * row names a pillar no feed quotes, and every pillar a feed quotes has its row.
 *
 * A pillar whose provider's notation cannot write it gets no row. The meeting-dated FOMC pillars
 * are spelled with meeting codes such as 1F, which no notation writes, so the FOMC curve's short
 * end is quoted on the internal subjects and not through an adapter. ICE and Tradeweb have no
 * notation at all, so a curve of theirs has no row either; the rows below say so and how many
 * pillars each provider carries.
 *
 * No id here is minted: each follows the vendor's own notation. The text of each id, and the
 * field it carries, is what the notation of its provider writes for that quote, so the notation
 * reads every row back as the quote the row stands for.
 *
 * A pillar's oresmd item is the datum the curve's own pillar key names: the currency, the index
 * tenor the curve holds, and the tenors the pillar starts and ends at, which is the item both the
 * feed that publishes the pillar and the bootstrap that reads it ask for.
 *
 * Neither half is written by hand. `tools/ir_catalogue` writes the block between the markers
 * below: it calls the provider's own notation to write each id, calls make_pillar_quote_key and
 * pillar_datum_uri for each item, and reads every id it wrote back to prove it names the same
 * quote. So the ids in this seed and the ids the runtime writes cannot drift. Regenerate the
 * block when an IR curve config or a notation changes, from the repository root, against a seeded
 * database:
 *
 *   ./compass.sh sql -- -t -A -F $'\t' -o tmp/ir-pillars.tsv -c "
 *       select c.provider_code, c.currency_code, c.index_family, e.instrument_code,
 *              e.start_tenor_code, e.end_tenor_code, c.tenor
 *       from ores_dq_ir_curve_generation_configs_artefact_tbl c
 *       join ores_dq_ir_curve_template_entries_artefact_tbl e
 *         on e.dataset_id = c.dataset_id
 *        and e.currency_code = c.currency_code
 *        and e.index_family = c.index_family
 *        and e.tenor = c.tenor
 *       where c.provider_code is not null
 *       order by 1, 2, 3, 4, 5, 6"
 *   build/publish/bin/ores.marketdata.ir-catalogue --write tmp/ir-pillars.tsv
 *
 * A pillar of a config this seed has no row for stops the seed, and says so, rather than leaving
 * the catalogue quietly short of a feed's ids.
 *
 * The ids of the vendor instruments are derived from their identity, so a rerun writes the same
 * rows. An instrument that already has a current mapping gets no second one, whatever its date.
 * This script is idempotent.
 */

\echo '--- Market Data Vendor Instruments and Mappings ---'

-- The temporary tables live inside one block, because psql runs each statement in a transaction
-- of its own and a table that drops on commit would be gone before the second insert.
do $$
declare
    v_stale text;
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

-- The id and the item of every pillar the feeds quote, one row for each (provider, pillar) pair.
-- The index tenor is the curve's own, which decides the series the pillar lands in; it is the
-- artefact config's tenor field, so the lookup joins to the config it came from.
create temporary table t_ir_pillar_id (
    provider_code text, currency_code text, index_family text, instrument_code text,
    start_tenor_code text, end_tenor_code text, config_tenor text,
    id_scheme text, external_id text, field text, oresmd_uri text
) on commit drop;

-- BEGIN GENERATED ir pillar ids: tools/ir_catalogue writes this block, see the header
    -- bloomberg: 336 pillar(s) with an id, 9 without
    -- demo: 60 pillar(s) with an id, 0 without
    -- ice: 0 pillar(s) with an id, 35 without
    -- tradeweb: 0 pillar(s) with an id, 35 without
    insert into t_ir_pillar_id (provider_code, currency_code, index_family,
        instrument_code, start_tenor_code, end_tenor_code, config_tenor,
        id_scheme, external_id, field, oresmd_uri) values
    ('bloomberg', 'AUD', 'aonia', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'AUD AONIA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'AUD', 'aonia', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'AUD AONIA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'AUD', 'aonia', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'AUD AONIA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'AUD', 'aonia', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'AUD AONIA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'AUD', 'aonia', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'AUD AONIA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'AUD', 'aonia', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'AUD AONIA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'AUD', 'aonia', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'AUD AONIA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'AUD', 'aonia', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'AUD AONIA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'AUD', 'aonia', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'AUD AONIA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'AUD', 'aonia', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'AUD AONIA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'AUD', 'aonia', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'AUD AONIA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'AUD', 'aonia', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'AUD AONIA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'BRL', 'cdi', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'BRL CDI DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'BRL', 'cdi', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'BRL CDI DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'BRL', 'cdi', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'BRL CDI FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'BRL', 'cdi', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'BRL CDI IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'BRL', 'cdi', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'BRL CDI IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'BRL', 'cdi', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'BRL CDI IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'BRL', 'cdi', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'BRL CDI IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'BRL', 'cdi', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'BRL CDI IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'BRL', 'cdi', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'BRL CDI IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'BRL', 'cdi', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'BRL CDI IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'BRL', 'cdi', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'BRL CDI IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'BRL', 'cdi', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'BRL CDI IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/BRL?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'CAD', 'corra', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'CAD CORRA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'CAD', 'corra', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'CAD CORRA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'CAD', 'corra', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'CAD CORRA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'CAD', 'corra', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'CAD CORRA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'CAD', 'corra', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'CAD CORRA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'CAD', 'corra', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'CAD CORRA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'CAD', 'corra', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'CAD CORRA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'CAD', 'corra', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'CAD CORRA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'CAD', 'corra', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'CAD CORRA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'CAD', 'corra', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'CAD CORRA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'CAD', 'corra', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'CAD CORRA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'CAD', 'corra', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'CAD CORRA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'CHF', 'saron', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'CHF SARON DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'CHF', 'saron', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'CHF SARON DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'CHF', 'saron', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'CHF SARON FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'CHF', 'saron', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'CHF SARON IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'CHF', 'saron', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'CHF SARON IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'CHF', 'saron', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'CHF SARON IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'CHF', 'saron', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'CHF SARON IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'CHF', 'saron', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'CHF SARON IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'CHF', 'saron', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'CHF SARON IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'CHF', 'saron', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'CHF SARON IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'CHF', 'saron', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'CHF SARON IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'CHF', 'saron', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'CHF SARON IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'CLP', 'camara', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'CLP CAMARA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'CLP', 'camara', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'CLP CAMARA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'CLP', 'camara', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'CLP CAMARA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'CLP', 'camara', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'CLP CAMARA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'CLP', 'camara', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'CLP CAMARA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'CLP', 'camara', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'CLP CAMARA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'CLP', 'camara', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'CLP CAMARA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'CLP', 'camara', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'CLP CAMARA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'CLP', 'camara', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'CLP CAMARA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'CLP', 'camara', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'CLP CAMARA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'CLP', 'camara', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'CLP CAMARA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'CLP', 'camara', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'CLP CAMARA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/CLP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'CNH', 'hibor', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'CNH HIBOR DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'CNH', 'hibor', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'CNH HIBOR DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'CNH', 'hibor', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'CNH HIBOR FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'CNH', 'hibor', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'CNH HIBOR IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'CNH', 'hibor', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'CNH HIBOR IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'CNH', 'hibor', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'CNH HIBOR IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'CNH', 'hibor', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'CNH HIBOR IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'CNH', 'hibor', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'CNH HIBOR IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'CNH', 'hibor', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'CNH HIBOR IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'CNH', 'hibor', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'CNH HIBOR IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'CNH', 'hibor', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'CNH HIBOR IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'CNH', 'hibor', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'CNH HIBOR IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/CNH?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'CNY', 'shibor', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'CNY SHIBOR DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'CNY', 'shibor', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'CNY SHIBOR DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'CNY', 'shibor', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'CNY SHIBOR FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'CNY', 'shibor', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'CNY SHIBOR IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'CNY', 'shibor', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'CNY SHIBOR IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'CNY', 'shibor', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'CNY SHIBOR IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'CNY', 'shibor', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'CNY SHIBOR IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'CNY', 'shibor', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'CNY SHIBOR IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'CNY', 'shibor', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'CNY SHIBOR IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'CNY', 'shibor', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'CNY SHIBOR IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'CNY', 'shibor', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'CNY SHIBOR IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'CNY', 'shibor', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'CNY SHIBOR IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'COP', 'ibr', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'COP IBR DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'COP', 'ibr', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'COP IBR DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'COP', 'ibr', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'COP IBR FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'COP', 'ibr', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'COP IBR IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'COP', 'ibr', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'COP IBR IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'COP', 'ibr', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'COP IBR IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'COP', 'ibr', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'COP IBR IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'COP', 'ibr', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'COP IBR IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'COP', 'ibr', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'COP IBR IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'COP', 'ibr', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'COP IBR IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'COP', 'ibr', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'COP IBR IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'COP', 'ibr', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'COP IBR IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/COP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'CZK', 'czeonia', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'CZK CZEONIA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'CZK', 'czeonia', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'CZK CZEONIA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'CZK', 'czeonia', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'CZK CZEONIA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'CZK', 'czeonia', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'CZK CZEONIA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'CZK', 'czeonia', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'CZK CZEONIA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'CZK', 'czeonia', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'CZK CZEONIA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'CZK', 'czeonia', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'CZK CZEONIA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'CZK', 'czeonia', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'CZK CZEONIA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'CZK', 'czeonia', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'CZK CZEONIA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'CZK', 'czeonia', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'CZK CZEONIA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'CZK', 'czeonia', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'CZK CZEONIA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'CZK', 'czeonia', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'CZK CZEONIA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/CZK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'DKK', 'destr', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'DKK DESTR DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'DKK', 'destr', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'DKK DESTR DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'DKK', 'destr', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'DKK DESTR FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'DKK', 'destr', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'DKK DESTR IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'DKK', 'destr', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'DKK DESTR IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'DKK', 'destr', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'DKK DESTR IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'DKK', 'destr', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'DKK DESTR IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'DKK', 'destr', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'DKK DESTR IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'DKK', 'destr', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'DKK DESTR IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'DKK', 'destr', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'DKK DESTR IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'DKK', 'destr', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'DKK DESTR IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'DKK', 'destr', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'DKK DESTR IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'EUR', 'estr', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'EUR ESTR DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'EUR', 'estr', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'EUR ESTR DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'EUR', 'estr', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'EUR ESTR FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'EUR', 'estr', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'EUR ESTR IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'EUR', 'estr', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'EUR ESTR IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'EUR', 'estr', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'EUR ESTR IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'EUR', 'estr', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'EUR ESTR IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'EUR', 'estr', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'EUR ESTR IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'EUR', 'estr', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'EUR ESTR IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'EUR', 'estr', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'EUR ESTR IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'EUR', 'estr', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'EUR ESTR IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'EUR', 'estr', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'EUR ESTR IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'GBP', 'sonia', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'GBP SONIA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'GBP', 'sonia', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'GBP SONIA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'GBP', 'sonia', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'GBP SONIA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'GBP', 'sonia', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'GBP SONIA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'GBP', 'sonia', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'GBP SONIA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'GBP', 'sonia', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'GBP SONIA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'GBP', 'sonia', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'GBP SONIA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'GBP', 'sonia', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'GBP SONIA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'GBP', 'sonia', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'GBP SONIA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'GBP', 'sonia', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'GBP SONIA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'GBP', 'sonia', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'GBP SONIA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'GBP', 'sonia', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'GBP SONIA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'HKD', 'honia', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'HKD HONIA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'HKD', 'honia', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'HKD HONIA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'HKD', 'honia', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'HKD HONIA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'HKD', 'honia', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'HKD HONIA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'HKD', 'honia', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'HKD HONIA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'HKD', 'honia', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'HKD HONIA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'HKD', 'honia', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'HKD HONIA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'HKD', 'honia', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'HKD HONIA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'HKD', 'honia', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'HKD HONIA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'HKD', 'honia', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'HKD HONIA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'HKD', 'honia', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'HKD HONIA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'HKD', 'honia', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'HKD HONIA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'HUF', 'buboron', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'HUF BUBORON DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'HUF', 'buboron', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'HUF BUBORON DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'HUF', 'buboron', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'HUF BUBORON FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'HUF', 'buboron', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'HUF BUBORON IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'HUF', 'buboron', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'HUF BUBORON IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'HUF', 'buboron', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'HUF BUBORON IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'HUF', 'buboron', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'HUF BUBORON IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'HUF', 'buboron', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'HUF BUBORON IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'HUF', 'buboron', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'HUF BUBORON IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'HUF', 'buboron', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'HUF BUBORON IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'HUF', 'buboron', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'HUF BUBORON IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'HUF', 'buboron', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'HUF BUBORON IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/HUF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'ILS', 'telboron', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'ILS TELBORON DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'ILS', 'telboron', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'ILS TELBORON DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'ILS', 'telboron', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'ILS TELBORON FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'ILS', 'telboron', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'ILS TELBORON IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'ILS', 'telboron', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'ILS TELBORON IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'ILS', 'telboron', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'ILS TELBORON IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'ILS', 'telboron', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'ILS TELBORON IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'ILS', 'telboron', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'ILS TELBORON IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'ILS', 'telboron', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'ILS TELBORON IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'ILS', 'telboron', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'ILS TELBORON IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'ILS', 'telboron', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'ILS TELBORON IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'ILS', 'telboron', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'ILS TELBORON IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/ILS?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'INR', 'mibor', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'INR MIBOR DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'INR', 'mibor', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'INR MIBOR DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'INR', 'mibor', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'INR MIBOR FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'INR', 'mibor', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'INR MIBOR IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'INR', 'mibor', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'INR MIBOR IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'INR', 'mibor', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'INR MIBOR IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'INR', 'mibor', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'INR MIBOR IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'INR', 'mibor', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'INR MIBOR IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'INR', 'mibor', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'INR MIBOR IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'INR', 'mibor', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'INR MIBOR IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'INR', 'mibor', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'INR MIBOR IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'INR', 'mibor', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'INR MIBOR IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'JPY', 'tona', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'JPY TONA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'JPY', 'tona', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'JPY TONA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'JPY', 'tona', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'JPY TONA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'JPY', 'tona', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'JPY TONA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'JPY', 'tona', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'JPY TONA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'JPY', 'tona', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'JPY TONA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'JPY', 'tona', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'JPY TONA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'JPY', 'tona', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'JPY TONA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'JPY', 'tona', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'JPY TONA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'JPY', 'tona', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'JPY TONA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'JPY', 'tona', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'JPY TONA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'JPY', 'tona', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'JPY TONA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'KRW', 'kofr', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'KRW KOFR DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'KRW', 'kofr', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'KRW KOFR DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'KRW', 'kofr', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'KRW KOFR FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'KRW', 'kofr', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'KRW KOFR IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'KRW', 'kofr', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'KRW KOFR IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'KRW', 'kofr', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'KRW KOFR IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'KRW', 'kofr', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'KRW KOFR IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'KRW', 'kofr', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'KRW KOFR IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'KRW', 'kofr', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'KRW KOFR IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'KRW', 'kofr', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'KRW KOFR IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'KRW', 'kofr', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'KRW KOFR IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'KRW', 'kofr', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'KRW KOFR IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'MXN', 'tiie', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'MXN TIIE DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'MXN', 'tiie', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'MXN TIIE DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'MXN', 'tiie', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'MXN TIIE FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'MXN', 'tiie', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'MXN TIIE IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'MXN', 'tiie', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'MXN TIIE IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'MXN', 'tiie', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'MXN TIIE IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'MXN', 'tiie', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'MXN TIIE IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'MXN', 'tiie', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'MXN TIIE IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'MXN', 'tiie', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'MXN TIIE IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'MXN', 'tiie', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'MXN TIIE IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'MXN', 'tiie', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'MXN TIIE IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'MXN', 'tiie', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'MXN TIIE IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'NOK', 'nowa', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'NOK NOWA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'NOK', 'nowa', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'NOK NOWA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'NOK', 'nowa', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'NOK NOWA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'NOK', 'nowa', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'NOK NOWA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'NOK', 'nowa', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'NOK NOWA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'NOK', 'nowa', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'NOK NOWA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'NOK', 'nowa', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'NOK NOWA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'NOK', 'nowa', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'NOK NOWA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'NOK', 'nowa', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'NOK NOWA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'NOK', 'nowa', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'NOK NOWA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'NOK', 'nowa', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'NOK NOWA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'NOK', 'nowa', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'NOK NOWA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'NZD', 'nzonia', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'NZD NZONIA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'NZD', 'nzonia', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'NZD NZONIA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'NZD', 'nzonia', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'NZD NZONIA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'NZD', 'nzonia', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'NZD NZONIA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'NZD', 'nzonia', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'NZD NZONIA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'NZD', 'nzonia', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'NZD NZONIA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'NZD', 'nzonia', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'NZD NZONIA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'NZD', 'nzonia', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'NZD NZONIA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'NZD', 'nzonia', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'NZD NZONIA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'NZD', 'nzonia', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'NZD NZONIA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'NZD', 'nzonia', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'NZD NZONIA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'NZD', 'nzonia', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'NZD NZONIA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'PLN', 'polonia', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'PLN POLONIA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'PLN', 'polonia', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'PLN POLONIA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'PLN', 'polonia', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'PLN POLONIA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'PLN', 'polonia', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'PLN POLONIA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'PLN', 'polonia', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'PLN POLONIA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'PLN', 'polonia', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'PLN POLONIA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'PLN', 'polonia', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'PLN POLONIA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'PLN', 'polonia', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'PLN POLONIA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'PLN', 'polonia', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'PLN POLONIA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'PLN', 'polonia', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'PLN POLONIA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'PLN', 'polonia', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'PLN POLONIA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'PLN', 'polonia', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'PLN POLONIA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'SEK', 'swestr', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'SEK SWESTR DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'SEK', 'swestr', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'SEK SWESTR DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'SEK', 'swestr', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'SEK SWESTR FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'SEK', 'swestr', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'SEK SWESTR IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'SEK', 'swestr', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'SEK SWESTR IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'SEK', 'swestr', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'SEK SWESTR IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'SEK', 'swestr', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'SEK SWESTR IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'SEK', 'swestr', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'SEK SWESTR IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'SEK', 'swestr', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'SEK SWESTR IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'SEK', 'swestr', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'SEK SWESTR IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'SEK', 'swestr', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'SEK SWESTR IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'SEK', 'swestr', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'SEK SWESTR IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'SGD', 'sora', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'SGD SORA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'SGD', 'sora', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'SGD SORA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'SGD', 'sora', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'SGD SORA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'SGD', 'sora', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'SGD SORA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'SGD', 'sora', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'SGD SORA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'SGD', 'sora', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'SGD SORA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'SGD', 'sora', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'SGD SORA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'SGD', 'sora', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'SGD SORA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'SGD', 'sora', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'SGD SORA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'SGD', 'sora', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'SGD SORA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'SGD', 'sora', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'SGD SORA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'SGD', 'sora', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'SGD SORA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'TRY', 'tlref', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'TRY TLREF DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'TRY', 'tlref', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'TRY TLREF DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'TRY', 'tlref', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'TRY TLREF FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'TRY', 'tlref', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'TRY TLREF IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'TRY', 'tlref', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'TRY TLREF IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'TRY', 'tlref', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'TRY TLREF IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'TRY', 'tlref', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'TRY TLREF IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'TRY', 'tlref', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'TRY TLREF IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'TRY', 'tlref', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'TRY TLREF IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'TRY', 'tlref', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'TRY TLREF IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'TRY', 'tlref', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'TRY TLREF IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'TRY', 'tlref', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'TRY TLREF IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/TRY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'TWD', 'taibor', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'TWD TAIBOR DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'TWD', 'taibor', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'TWD TAIBOR DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'TWD', 'taibor', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'TWD TAIBOR FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'TWD', 'taibor', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'TWD TAIBOR IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'TWD', 'taibor', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'TWD TAIBOR IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'TWD', 'taibor', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'TWD TAIBOR IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'TWD', 'taibor', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'TWD TAIBOR IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'TWD', 'taibor', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'TWD TAIBOR IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'TWD', 'taibor', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'TWD TAIBOR IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'TWD', 'taibor', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'TWD TAIBOR IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'TWD', 'taibor', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'TWD TAIBOR IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'TWD', 'taibor', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'TWD TAIBOR IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'USD', 'sofr', 'DEPO', 'SPOT', '1F', 'FOMC', null, null, null, null),
    ('bloomberg', 'USD', 'sofr', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'USD SOFR DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'USD', 'sofr', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'USD SOFR DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'USD', 'sofr', 'FRA', '1F', '2F', 'FOMC', null, null, null, null),
    ('bloomberg', 'USD', 'sofr', 'FRA', '2F', '3F', 'FOMC', null, null, null, null),
    ('bloomberg', 'USD', 'sofr', 'FRA', '3F', '4F', 'FOMC', null, null, null, null),
    ('bloomberg', 'USD', 'sofr', 'FRA', '4F', '5F', 'FOMC', null, null, null, null),
    ('bloomberg', 'USD', 'sofr', 'FRA', '5F', '6F', 'FOMC', null, null, null, null),
    ('bloomberg', 'USD', 'sofr', 'FRA', '6F', '7F', 'FOMC', null, null, null, null),
    ('bloomberg', 'USD', 'sofr', 'FRA', '7F', '8F', 'FOMC', null, null, null, null),
    ('bloomberg', 'USD', 'sofr', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'USD SOFR FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'USD', 'sofr', 'IRS', '8F', '1Y', 'FOMC', null, null, null, null),
    ('bloomberg', 'USD', 'sofr', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'USD SOFR IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'USD', 'sofr', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'USD SOFR IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'USD', 'sofr', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'USD SOFR IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'USD', 'sofr', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'USD SOFR IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'USD', 'sofr', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'USD SOFR IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'USD', 'sofr', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'USD SOFR IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'USD', 'sofr', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'USD SOFR IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'USD', 'sofr', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'USD SOFR IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'USD', 'sofr', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'USD SOFR IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('bloomberg', 'ZAR', 'zaronia', 'DEPO', 'SPOT', '1M', '', 'bbg_ticker', 'ZAR ZARONIA DEPO SPOT 1M Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M'),
    ('bloomberg', 'ZAR', 'zaronia', 'DEPO', 'SPOT', '3M', '', 'bbg_ticker', 'ZAR ZARONIA DEPO SPOT 3M Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('bloomberg', 'ZAR', 'zaronia', 'FRA', 'SPOT', '6M', '', 'bbg_ticker', 'ZAR ZARONIA FRA SPOT 6M Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=6M'),
    ('bloomberg', 'ZAR', 'zaronia', 'IRS', 'SPOT', '10Y', '', 'bbg_ticker', 'ZAR ZARONIA IRS SPOT 10Y Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=10Y'),
    ('bloomberg', 'ZAR', 'zaronia', 'IRS', 'SPOT', '12M', '', 'bbg_ticker', 'ZAR ZARONIA IRS SPOT 12M Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M'),
    ('bloomberg', 'ZAR', 'zaronia', 'IRS', 'SPOT', '15Y', '', 'bbg_ticker', 'ZAR ZARONIA IRS SPOT 15Y Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=15Y'),
    ('bloomberg', 'ZAR', 'zaronia', 'IRS', 'SPOT', '20Y', '', 'bbg_ticker', 'ZAR ZARONIA IRS SPOT 20Y Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=20Y'),
    ('bloomberg', 'ZAR', 'zaronia', 'IRS', 'SPOT', '2Y', '', 'bbg_ticker', 'ZAR ZARONIA IRS SPOT 2Y Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('bloomberg', 'ZAR', 'zaronia', 'IRS', 'SPOT', '30Y', '', 'bbg_ticker', 'ZAR ZARONIA IRS SPOT 30Y Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=30Y'),
    ('bloomberg', 'ZAR', 'zaronia', 'IRS', 'SPOT', '3Y', '', 'bbg_ticker', 'ZAR ZARONIA IRS SPOT 3Y Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3Y'),
    ('bloomberg', 'ZAR', 'zaronia', 'IRS', 'SPOT', '5Y', '', 'bbg_ticker', 'ZAR ZARONIA IRS SPOT 5Y Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=5Y'),
    ('bloomberg', 'ZAR', 'zaronia', 'IRS', 'SPOT', '7Y', '', 'bbg_ticker', 'ZAR ZARONIA IRS SPOT 7Y Curncy', 'PX_LAST', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=7Y'),
    ('demo', 'AUD', 'aonia', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'AUD/aonia/DEPO/SPOT/3M', 'value', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'AUD', 'aonia', 'FRA', '3M', '6M', '', 'demo_key', 'AUD/aonia/FRA/3M/6M', 'value', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'AUD', 'aonia', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'AUD/aonia/IRS/SPOT/2Y', 'value', 'oresmd://ir/AUD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'CAD', 'corra', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'CAD/corra/DEPO/SPOT/3M', 'value', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'CAD', 'corra', 'FRA', '3M', '6M', '', 'demo_key', 'CAD/corra/FRA/3M/6M', 'value', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'CAD', 'corra', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'CAD/corra/IRS/SPOT/2Y', 'value', 'oresmd://ir/CAD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'CHF', 'saron', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'CHF/saron/DEPO/SPOT/3M', 'value', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'CHF', 'saron', 'FRA', '3M', '6M', '', 'demo_key', 'CHF/saron/FRA/3M/6M', 'value', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'CHF', 'saron', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'CHF/saron/IRS/SPOT/2Y', 'value', 'oresmd://ir/CHF?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'CNY', 'shibor', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'CNY/shibor/DEPO/SPOT/3M', 'value', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'CNY', 'shibor', 'FRA', '3M', '6M', '', 'demo_key', 'CNY/shibor/FRA/3M/6M', 'value', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'CNY', 'shibor', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'CNY/shibor/IRS/SPOT/2Y', 'value', 'oresmd://ir/CNY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'DKK', 'destr', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'DKK/destr/DEPO/SPOT/3M', 'value', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'DKK', 'destr', 'FRA', '3M', '6M', '', 'demo_key', 'DKK/destr/FRA/3M/6M', 'value', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'DKK', 'destr', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'DKK/destr/IRS/SPOT/2Y', 'value', 'oresmd://ir/DKK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'EUR', 'estr', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'EUR/estr/DEPO/SPOT/3M', 'value', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'EUR', 'estr', 'FRA', '3M', '6M', '', 'demo_key', 'EUR/estr/FRA/3M/6M', 'value', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'EUR', 'estr', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'EUR/estr/IRS/SPOT/2Y', 'value', 'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'GBP', 'sonia', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'GBP/sonia/DEPO/SPOT/3M', 'value', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'GBP', 'sonia', 'FRA', '3M', '6M', '', 'demo_key', 'GBP/sonia/FRA/3M/6M', 'value', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'GBP', 'sonia', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'GBP/sonia/IRS/SPOT/2Y', 'value', 'oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'HKD', 'honia', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'HKD/honia/DEPO/SPOT/3M', 'value', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'HKD', 'honia', 'FRA', '3M', '6M', '', 'demo_key', 'HKD/honia/FRA/3M/6M', 'value', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'HKD', 'honia', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'HKD/honia/IRS/SPOT/2Y', 'value', 'oresmd://ir/HKD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'INR', 'mibor', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'INR/mibor/DEPO/SPOT/3M', 'value', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'INR', 'mibor', 'FRA', '3M', '6M', '', 'demo_key', 'INR/mibor/FRA/3M/6M', 'value', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'INR', 'mibor', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'INR/mibor/IRS/SPOT/2Y', 'value', 'oresmd://ir/INR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'JPY', 'tona', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'JPY/tona/DEPO/SPOT/3M', 'value', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'JPY', 'tona', 'FRA', '3M', '6M', '', 'demo_key', 'JPY/tona/FRA/3M/6M', 'value', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'JPY', 'tona', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'JPY/tona/IRS/SPOT/2Y', 'value', 'oresmd://ir/JPY?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'KRW', 'kofr', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'KRW/kofr/DEPO/SPOT/3M', 'value', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'KRW', 'kofr', 'FRA', '3M', '6M', '', 'demo_key', 'KRW/kofr/FRA/3M/6M', 'value', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'KRW', 'kofr', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'KRW/kofr/IRS/SPOT/2Y', 'value', 'oresmd://ir/KRW?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'MXN', 'tiie', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'MXN/tiie/DEPO/SPOT/3M', 'value', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'MXN', 'tiie', 'FRA', '3M', '6M', '', 'demo_key', 'MXN/tiie/FRA/3M/6M', 'value', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'MXN', 'tiie', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'MXN/tiie/IRS/SPOT/2Y', 'value', 'oresmd://ir/MXN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'NOK', 'nowa', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'NOK/nowa/DEPO/SPOT/3M', 'value', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'NOK', 'nowa', 'FRA', '3M', '6M', '', 'demo_key', 'NOK/nowa/FRA/3M/6M', 'value', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'NOK', 'nowa', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'NOK/nowa/IRS/SPOT/2Y', 'value', 'oresmd://ir/NOK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'NZD', 'nzonia', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'NZD/nzonia/DEPO/SPOT/3M', 'value', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'NZD', 'nzonia', 'FRA', '3M', '6M', '', 'demo_key', 'NZD/nzonia/FRA/3M/6M', 'value', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'NZD', 'nzonia', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'NZD/nzonia/IRS/SPOT/2Y', 'value', 'oresmd://ir/NZD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'PLN', 'polonia', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'PLN/polonia/DEPO/SPOT/3M', 'value', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'PLN', 'polonia', 'FRA', '3M', '6M', '', 'demo_key', 'PLN/polonia/FRA/3M/6M', 'value', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'PLN', 'polonia', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'PLN/polonia/IRS/SPOT/2Y', 'value', 'oresmd://ir/PLN?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'SEK', 'swestr', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'SEK/swestr/DEPO/SPOT/3M', 'value', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'SEK', 'swestr', 'FRA', '3M', '6M', '', 'demo_key', 'SEK/swestr/FRA/3M/6M', 'value', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'SEK', 'swestr', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'SEK/swestr/IRS/SPOT/2Y', 'value', 'oresmd://ir/SEK?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'SGD', 'sora', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'SGD/sora/DEPO/SPOT/3M', 'value', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'SGD', 'sora', 'FRA', '3M', '6M', '', 'demo_key', 'SGD/sora/FRA/3M/6M', 'value', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'SGD', 'sora', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'SGD/sora/IRS/SPOT/2Y', 'value', 'oresmd://ir/SGD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'TWD', 'taibor', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'TWD/taibor/DEPO/SPOT/3M', 'value', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'TWD', 'taibor', 'FRA', '3M', '6M', '', 'demo_key', 'TWD/taibor/FRA/3M/6M', 'value', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'TWD', 'taibor', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'TWD/taibor/IRS/SPOT/2Y', 'value', 'oresmd://ir/TWD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'USD', 'sofr', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'USD/sofr/DEPO/SPOT/3M', 'value', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'USD', 'sofr', 'FRA', '3M', '6M', '', 'demo_key', 'USD/sofr/FRA/3M/6M', 'value', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'USD', 'sofr', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'USD/sofr/IRS/SPOT/2Y', 'value', 'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('demo', 'ZAR', 'zaronia', 'DEPO', 'SPOT', '3M', '', 'demo_key', 'ZAR/zaronia/DEPO/SPOT/3M', 'value', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=3M'),
    ('demo', 'ZAR', 'zaronia', 'FRA', '3M', '6M', '', 'demo_key', 'ZAR/zaronia/FRA/3M/6M', 'value', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D&term=6M'),
    ('demo', 'ZAR', 'zaronia', 'IRS', 'SPOT', '2Y', '', 'demo_key', 'ZAR/zaronia/IRS/SPOT/2Y', 'value', 'oresmd://ir/ZAR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=2Y'),
    ('ice', 'CHF', 'libor', 'DEPO', 'SPOT', '1M', '3M', null, null, null, null),
    ('ice', 'CHF', 'libor', 'DEPO', 'SPOT', '3M', '3M', null, null, null, null),
    ('ice', 'CHF', 'libor', 'FRA', '3M', '6M', '3M', null, null, null, null),
    ('ice', 'CHF', 'libor', 'FRA', '6M', '12M', '3M', null, null, null, null),
    ('ice', 'CHF', 'libor', 'IRS', 'SPOT', '10Y', '3M', null, null, null, null),
    ('ice', 'CHF', 'libor', 'IRS', 'SPOT', '2Y', '3M', null, null, null, null),
    ('ice', 'CHF', 'libor', 'IRS', 'SPOT', '5Y', '3M', null, null, null, null),
    ('ice', 'GBP', 'libor', 'DEPO', 'SPOT', '1M', '3M', null, null, null, null),
    ('ice', 'GBP', 'libor', 'DEPO', 'SPOT', '3M', '3M', null, null, null, null),
    ('ice', 'GBP', 'libor', 'FRA', '3M', '6M', '3M', null, null, null, null),
    ('ice', 'GBP', 'libor', 'FRA', '6M', '12M', '3M', null, null, null, null),
    ('ice', 'GBP', 'libor', 'IRS', 'SPOT', '10Y', '3M', null, null, null, null),
    ('ice', 'GBP', 'libor', 'IRS', 'SPOT', '2Y', '3M', null, null, null, null),
    ('ice', 'GBP', 'libor', 'IRS', 'SPOT', '5Y', '3M', null, null, null, null),
    ('ice', 'JPY', 'libor', 'DEPO', 'SPOT', '1M', '3M', null, null, null, null),
    ('ice', 'JPY', 'libor', 'DEPO', 'SPOT', '3M', '3M', null, null, null, null),
    ('ice', 'JPY', 'libor', 'FRA', '3M', '6M', '3M', null, null, null, null),
    ('ice', 'JPY', 'libor', 'FRA', '6M', '12M', '3M', null, null, null, null),
    ('ice', 'JPY', 'libor', 'IRS', 'SPOT', '10Y', '3M', null, null, null, null),
    ('ice', 'JPY', 'libor', 'IRS', 'SPOT', '2Y', '3M', null, null, null, null),
    ('ice', 'JPY', 'libor', 'IRS', 'SPOT', '5Y', '3M', null, null, null, null),
    ('ice', 'USD', 'libor', 'DEPO', 'SPOT', '1M', '1M', null, null, null, null),
    ('ice', 'USD', 'libor', 'DEPO', 'SPOT', '1M', '6M', null, null, null, null),
    ('ice', 'USD', 'libor', 'DEPO', 'SPOT', '3M', '1M', null, null, null, null),
    ('ice', 'USD', 'libor', 'DEPO', 'SPOT', '3M', '6M', null, null, null, null),
    ('ice', 'USD', 'libor', 'FRA', '3M', '6M', '1M', null, null, null, null),
    ('ice', 'USD', 'libor', 'FRA', '3M', '6M', '6M', null, null, null, null),
    ('ice', 'USD', 'libor', 'FRA', '6M', '12M', '1M', null, null, null, null),
    ('ice', 'USD', 'libor', 'FRA', '6M', '12M', '6M', null, null, null, null),
    ('ice', 'USD', 'libor', 'IRS', 'SPOT', '10Y', '1M', null, null, null, null),
    ('ice', 'USD', 'libor', 'IRS', 'SPOT', '10Y', '6M', null, null, null, null),
    ('ice', 'USD', 'libor', 'IRS', 'SPOT', '2Y', '1M', null, null, null, null),
    ('ice', 'USD', 'libor', 'IRS', 'SPOT', '2Y', '6M', null, null, null, null),
    ('ice', 'USD', 'libor', 'IRS', 'SPOT', '5Y', '1M', null, null, null, null),
    ('ice', 'USD', 'libor', 'IRS', 'SPOT', '5Y', '6M', null, null, null, null),
    ('tradeweb', 'CHF', 'libor', 'DEPO', 'SPOT', '1M', '6M', null, null, null, null),
    ('tradeweb', 'CHF', 'libor', 'DEPO', 'SPOT', '3M', '6M', null, null, null, null),
    ('tradeweb', 'CHF', 'libor', 'FRA', '3M', '6M', '6M', null, null, null, null),
    ('tradeweb', 'CHF', 'libor', 'FRA', '6M', '12M', '6M', null, null, null, null),
    ('tradeweb', 'CHF', 'libor', 'IRS', 'SPOT', '10Y', '6M', null, null, null, null),
    ('tradeweb', 'CHF', 'libor', 'IRS', 'SPOT', '2Y', '6M', null, null, null, null),
    ('tradeweb', 'CHF', 'libor', 'IRS', 'SPOT', '5Y', '6M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'DEPO', 'SPOT', '1M', '1M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'DEPO', 'SPOT', '1M', '6M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'DEPO', 'SPOT', '3M', '1M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'DEPO', 'SPOT', '3M', '6M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'FRA', '3M', '6M', '1M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'FRA', '3M', '6M', '6M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'FRA', '6M', '12M', '1M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'FRA', '6M', '12M', '6M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'IRS', 'SPOT', '10Y', '1M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'IRS', 'SPOT', '10Y', '6M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'IRS', 'SPOT', '2Y', '1M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'IRS', 'SPOT', '2Y', '6M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'IRS', 'SPOT', '5Y', '1M', null, null, null, null),
    ('tradeweb', 'EUR', 'euribor', 'IRS', 'SPOT', '5Y', '6M', null, null, null, null),
    ('tradeweb', 'GBP', 'libor', 'DEPO', 'SPOT', '1M', '1M', null, null, null, null),
    ('tradeweb', 'GBP', 'libor', 'DEPO', 'SPOT', '3M', '1M', null, null, null, null),
    ('tradeweb', 'GBP', 'libor', 'FRA', '3M', '6M', '1M', null, null, null, null),
    ('tradeweb', 'GBP', 'libor', 'FRA', '6M', '12M', '1M', null, null, null, null),
    ('tradeweb', 'GBP', 'libor', 'IRS', 'SPOT', '10Y', '1M', null, null, null, null),
    ('tradeweb', 'GBP', 'libor', 'IRS', 'SPOT', '2Y', '1M', null, null, null, null),
    ('tradeweb', 'GBP', 'libor', 'IRS', 'SPOT', '5Y', '1M', null, null, null, null),
    ('tradeweb', 'JPY', 'libor', 'DEPO', 'SPOT', '1M', '1M', null, null, null, null),
    ('tradeweb', 'JPY', 'libor', 'DEPO', 'SPOT', '3M', '1M', null, null, null, null),
    ('tradeweb', 'JPY', 'libor', 'FRA', '3M', '6M', '1M', null, null, null, null),
    ('tradeweb', 'JPY', 'libor', 'FRA', '6M', '12M', '1M', null, null, null, null),
    ('tradeweb', 'JPY', 'libor', 'IRS', 'SPOT', '10Y', '1M', null, null, null, null),
    ('tradeweb', 'JPY', 'libor', 'IRS', 'SPOT', '2Y', '1M', null, null, null, null),
    ('tradeweb', 'JPY', 'libor', 'IRS', 'SPOT', '5Y', '1M', null, null, null, null);
-- END GENERATED ir pillar ids

-- The pillars the seeds' IR curve configs quote: one row for each entry of each curve.
create temporary table t_ir_feed_pillar (
    provider_code text, currency_code text, index_family text, instrument_code text,
    start_tenor_code text, end_tenor_code text, config_tenor text
) on commit drop;

insert into t_ir_feed_pillar
select distinct c.provider_code, c.currency_code, c.index_family, e.instrument_code,
       e.start_tenor_code, e.end_tenor_code, c.tenor
from ores_dq_ir_curve_generation_configs_artefact_tbl c
join ores_dq_ir_curve_template_entries_artefact_tbl e
  on e.dataset_id = c.dataset_id
 and e.currency_code = c.currency_code
 and e.index_family = c.index_family
 and e.tenor = c.tenor
where c.provider_code is not null
  and c.enabled;

-- A pillar no row above names is one this seed cannot write, and a catalogue short of a feed's
-- ids is worse than a seed that stops: the ids and the items are generated, so the fix is to
-- regenerate them.
select string_agg(distinct f.provider_code || ' ' || f.currency_code || ' ' || f.index_family ||
                  ' ' || f.instrument_code || ' ' || f.start_tenor_code || ' to ' ||
                  f.end_tenor_code || ' (tenor ' || f.config_tenor || ')', '; ')
into v_stale
from t_ir_feed_pillar f
where not exists (
    select 1 from t_ir_pillar_id i
    where i.provider_code = f.provider_code
      and i.currency_code = f.currency_code
      and i.index_family = f.index_family
      and i.instrument_code = f.instrument_code
      and i.start_tenor_code = f.start_tenor_code
      and i.end_tenor_code = f.end_tenor_code
      and i.config_tenor = f.config_tenor
);

if v_stale is not null then
    raise exception 'The IR curve configs quote pillars the vendor instrument seed has no id for: %. Regenerate the seed: see the header of marketdata_vendor_instruments_populate.sql.', v_stale;
end if;

-- The pillars of the curves, in the notation of the provider of the curve they belong to.
insert into t_vendor_inventory (provider_code, id_scheme, external_id, field, oresmd_uri)
select distinct f.provider_code, i.id_scheme, i.external_id, i.field, i.oresmd_uri
from t_ir_feed_pillar f
join t_ir_pillar_id i
  on i.provider_code = f.provider_code
 and i.currency_code = f.currency_code
 and i.index_family = f.index_family
 and i.instrument_code = f.instrument_code
 and i.start_tenor_code = f.start_tenor_code
 and i.end_tenor_code = f.end_tenor_code
 and i.config_tenor = f.config_tenor
where i.external_id is not null;

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
