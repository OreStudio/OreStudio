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
 * Installs the FX spot pairs that the simulated feeds quote, in the notation of Bloomberg, of
 * LSEG and of ORE, and maps each vendor instrument to its oresmd item. The three providers quote
 * the same item, so a party that changes its provider keeps the series of the item. The
 * inventory page lists the rows and says which are marked for a check against the vendor.
 *
 * No id here is minted: each follows the vendor's own notation. The rate curves, and the
 * providers ICE and Tradeweb, have no rows yet.
 *
 * The ids of the vendor instruments are derived from their identity, so a rerun writes the same
 * rows. This script is idempotent.
 */

\echo '--- Market Data Vendor Instruments and Mappings ---'

create temporary table t_fx_inventory (
    provider_code text, id_scheme text, external_id text, field text, oresmd_uri text
) on commit drop;

insert into t_fx_inventory (provider_code, id_scheme, external_id, field, oresmd_uri) values
    ('bloomberg', 'bbg_ticker', 'EURUSD Curncy', 'PX_LAST', 'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('lseg', 'ric', 'EUR=', 'BID', 'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('ore', 'ore_key', 'FX/RATE/EUR/USD', 'value', 'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('bloomberg', 'bbg_ticker', 'GBPUSD Curncy', 'PX_LAST', 'oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('lseg', 'ric', 'GBP=', 'BID', 'oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('ore', 'ore_key', 'FX/RATE/GBP/USD', 'value', 'oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('bloomberg', 'bbg_ticker', 'AUDUSD Curncy', 'PX_LAST', 'oresmd://fx/AUD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('lseg', 'ric', 'AUD=', 'BID', 'oresmd://fx/AUD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('ore', 'ore_key', 'FX/RATE/AUD/USD', 'value', 'oresmd://fx/AUD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('bloomberg', 'bbg_ticker', 'NZDUSD Curncy', 'PX_LAST', 'oresmd://fx/NZD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('lseg', 'ric', 'NZD=', 'BID', 'oresmd://fx/NZD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('ore', 'ore_key', 'FX/RATE/NZD/USD', 'value', 'oresmd://fx/NZD?type=quote&instrument=fx_spot&quote=rate&ccy=USD'),
    ('bloomberg', 'bbg_ticker', 'USDCAD Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CAD'),
    ('lseg', 'ric', 'CAD=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CAD'),
    ('ore', 'ore_key', 'FX/RATE/USD/CAD', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CAD'),
    ('bloomberg', 'bbg_ticker', 'USDCHF Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CHF'),
    ('lseg', 'ric', 'CHF=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CHF'),
    ('ore', 'ore_key', 'FX/RATE/USD/CHF', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CHF'),
    ('bloomberg', 'bbg_ticker', 'USDDKK Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=DKK'),
    ('lseg', 'ric', 'DKK=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=DKK'),
    ('ore', 'ore_key', 'FX/RATE/USD/DKK', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=DKK'),
    ('bloomberg', 'bbg_ticker', 'USDINR Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=INR'),
    ('lseg', 'ric', 'INR=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=INR'),
    ('ore', 'ore_key', 'FX/RATE/USD/INR', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=INR'),
    ('bloomberg', 'bbg_ticker', 'USDJPY Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=JPY'),
    ('lseg', 'ric', 'JPY=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=JPY'),
    ('ore', 'ore_key', 'FX/RATE/USD/JPY', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=JPY'),
    ('bloomberg', 'bbg_ticker', 'USDMXN Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=MXN'),
    ('lseg', 'ric', 'MXN=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=MXN'),
    ('ore', 'ore_key', 'FX/RATE/USD/MXN', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=MXN'),
    ('bloomberg', 'bbg_ticker', 'USDNOK Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=NOK'),
    ('lseg', 'ric', 'NOK=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=NOK'),
    ('ore', 'ore_key', 'FX/RATE/USD/NOK', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=NOK'),
    ('bloomberg', 'bbg_ticker', 'USDSEK Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SEK'),
    ('lseg', 'ric', 'SEK=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SEK'),
    ('ore', 'ore_key', 'FX/RATE/USD/SEK', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SEK'),
    ('bloomberg', 'bbg_ticker', 'USDZAR Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ZAR'),
    ('lseg', 'ric', 'ZAR=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ZAR'),
    ('ore', 'ore_key', 'FX/RATE/USD/ZAR', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ZAR'),
    ('bloomberg', 'bbg_ticker', 'USDHKD Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HKD'),
    ('lseg', 'ric', 'HKD=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HKD'),
    ('ore', 'ore_key', 'FX/RATE/USD/HKD', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HKD'),
    ('bloomberg', 'bbg_ticker', 'USDSGD Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SGD'),
    ('lseg', 'ric', 'SGD=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SGD'),
    ('ore', 'ore_key', 'FX/RATE/USD/SGD', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=SGD'),
    ('bloomberg', 'bbg_ticker', 'USDCNH Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CNH'),
    ('lseg', 'ric', 'CNH=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CNH'),
    ('ore', 'ore_key', 'FX/RATE/USD/CNH', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CNH'),
    ('bloomberg', 'bbg_ticker', 'USDKRW Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=KRW'),
    ('lseg', 'ric', 'KRW=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=KRW'),
    ('ore', 'ore_key', 'FX/RATE/USD/KRW', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=KRW'),
    ('bloomberg', 'bbg_ticker', 'USDTWD Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TWD'),
    ('lseg', 'ric', 'TWD=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TWD'),
    ('ore', 'ore_key', 'FX/RATE/USD/TWD', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TWD'),
    ('bloomberg', 'bbg_ticker', 'USDPLN Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=PLN'),
    ('lseg', 'ric', 'PLN=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=PLN'),
    ('ore', 'ore_key', 'FX/RATE/USD/PLN', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=PLN'),
    ('bloomberg', 'bbg_ticker', 'USDCZK Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CZK'),
    ('lseg', 'ric', 'CZK=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CZK'),
    ('ore', 'ore_key', 'FX/RATE/USD/CZK', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CZK'),
    ('bloomberg', 'bbg_ticker', 'USDHUF Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HUF'),
    ('lseg', 'ric', 'HUF=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HUF'),
    ('ore', 'ore_key', 'FX/RATE/USD/HUF', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=HUF'),
    ('bloomberg', 'bbg_ticker', 'USDTRY Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TRY'),
    ('lseg', 'ric', 'TRY=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TRY'),
    ('ore', 'ore_key', 'FX/RATE/USD/TRY', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TRY'),
    ('bloomberg', 'bbg_ticker', 'USDILS Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ILS'),
    ('lseg', 'ric', 'ILS=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ILS'),
    ('ore', 'ore_key', 'FX/RATE/USD/ILS', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=ILS'),
    ('bloomberg', 'bbg_ticker', 'USDBRL Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=BRL'),
    ('lseg', 'ric', 'BRL=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=BRL'),
    ('ore', 'ore_key', 'FX/RATE/USD/BRL', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=BRL'),
    ('bloomberg', 'bbg_ticker', 'USDCLP Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CLP'),
    ('lseg', 'ric', 'CLP=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CLP'),
    ('ore', 'ore_key', 'FX/RATE/USD/CLP', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=CLP'),
    ('bloomberg', 'bbg_ticker', 'USDCOP Curncy', 'PX_LAST', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=COP'),
    ('lseg', 'ric', 'COP=', 'BID', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=COP'),
    ('ore', 'ore_key', 'FX/RATE/USD/COP', 'value', 'oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=COP');

insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted, deviation_note,
    modified_by, performed_by, change_reason_code, change_commentary
)
select md5(t.provider_code || '|' || t.id_scheme || '|' || t.external_id || '|' || t.field)::uuid,
       ores_utility_system_tenant_id_fn(), 0, t.provider_code, t.id_scheme, t.external_id, t.field,
       false, '', current_user, current_user, 'system.initial_load',
       'Initial population of the FX spot vendor instruments'
from t_fx_inventory t
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
       'Initial population of the FX spot mappings'
from t_fx_inventory t
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
      and m.effective_from = timestamptz '2026-01-01 00:00:00+00'
      and m.valid_to = ores_utility_infinity_timestamp_fn()
);

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
