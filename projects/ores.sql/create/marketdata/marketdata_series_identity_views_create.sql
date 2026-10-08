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

-- One view per asset class, so a caller reads the identity columns its class
-- fills without knowing which of the sixty-one fields its instrument type
-- declares. Each view names the instrument types of its class, which is the
-- index the filter uses, and every view is security_invoker: the tenant policy
-- on the table decides what the reading session sees.
--
-- The columns are the identity fields the codec schema gives that class, and
-- check_marketdata_identity_columns.py holds them to it.

-- The commodity identity.
create or replace view ores_marketdata_series_identity_commodity_vw
with (security_invoker = true) as
select
    series_id,
    tenant_id,
    party_id,
    instrument_type,
    quote_type,
    asset_class,
    identity_kind,
    ccy,
    commodity_name,
    "offset",
    option_type
from ores_marketdata_market_series_identity_tbl
where identity_kind = 'series'
  and instrument_type in (
        'commodity_calendar_spread_option',
        'commodity_fwd',
        'commodity_option',
        'commodity_spot'
    );

-- The correlation identity.
create or replace view ores_marketdata_series_identity_correlation_vw
with (security_invoker = true) as
select
    series_id,
    tenant_id,
    party_id,
    instrument_type,
    quote_type,
    asset_class,
    identity_kind,
    index1,
    index2
from ores_marketdata_market_series_identity_tbl
where identity_kind = 'series'
  and instrument_type in (
        'correlation'
    );

-- The credit identity.
create or replace view ores_marketdata_series_identity_credit_vw
with (security_invoker = true) as
select
    series_id,
    tenant_id,
    party_id,
    instrument_type,
    quote_type,
    asset_class,
    identity_kind,
    ccy,
    cds_index_name,
    doc_clause,
    index_name,
    index_term,
    running_spread,
    seniority,
    side,
    term,
    underlying_name
from ores_marketdata_market_series_identity_tbl
where identity_kind = 'series'
  and instrument_type in (
        'assumed_recovery_rate',
        'cds',
        'cds_index',
        'hazard_rate',
        'index_cds_option',
        'index_cds_tranche',
        'recovery_rate'
    );

-- The equity identity.
create or replace view ores_marketdata_series_identity_equity_vw
with (security_invoker = true) as
select
    series_id,
    tenant_id,
    party_id,
    instrument_type,
    quote_type,
    asset_class,
    identity_kind,
    ccy,
    eq_name,
    option_type
from ores_marketdata_market_series_identity_tbl
where identity_kind = 'series'
  and instrument_type in (
        'equity_dividend',
        'equity_fwd',
        'equity_option',
        'equity_spot'
    );

-- The fx identity.
create or replace view ores_marketdata_series_identity_fx_vw
with (security_invoker = true) as
select
    series_id,
    tenant_id,
    party_id,
    instrument_type,
    quote_type,
    asset_class,
    identity_kind,
    ccy,
    unit_ccy
from ores_marketdata_market_series_identity_tbl
where identity_kind = 'series'
  and instrument_type in (
        'fx_fwd',
        'fx_option',
        'fx_spot'
    );

-- The inflation identity.
create or replace view ores_marketdata_series_identity_inflation_vw
with (security_invoker = true) as
select
    series_id,
    tenant_id,
    party_id,
    instrument_type,
    quote_type,
    asset_class,
    identity_kind,
    cap_floor,
    index,
    seasonality_type
from ores_marketdata_market_series_identity_tbl
where identity_kind = 'series'
  and instrument_type in (
        'seasonality',
        'yy_inflation_capfloor',
        'yy_inflation_swap',
        'zc_inflation_capfloor',
        'zc_inflation_swap'
    );

-- The ir identity.
create or replace view ores_marketdata_series_identity_ir_vw
with (security_invoker = true) as
select
    series_id,
    tenant_id,
    party_id,
    instrument_type,
    quote_type,
    asset_class,
    identity_kind,
    atm,
    cap_floor,
    ccy,
    contract,
    curve_id,
    day_counter,
    fixed_ccy,
    fixed_tenor,
    flat_ccy,
    flat_term,
    float_ccy,
    float_tenor,
    fwd_start,
    identifier,
    index_name,
    index_tenor,
    payer_receiver,
    quote_tag,
    relative,
    tenor,
    term
from ores_marketdata_market_series_identity_tbl
where identity_kind = 'series'
  and instrument_type in (
        'basis_swap',
        'bma_swap',
        'capfloor',
        'cc_basis_swap',
        'cc_fix_float_swap',
        'discount',
        'fra',
        'imm_fra',
        'ir_swap',
        'mm',
        'mm_future',
        'oi_future',
        'swaption',
        'zero'
    );

-- The rating identity.
create or replace view ores_marketdata_series_identity_rating_vw
with (security_invoker = true) as
select
    series_id,
    tenant_id,
    party_id,
    instrument_type,
    quote_type,
    asset_class,
    identity_kind,
    rating_name
from ores_marketdata_market_series_identity_tbl
where identity_kind = 'series'
  and instrument_type in (
        'rating'
    );

-- The security identity.
create or replace view ores_marketdata_series_identity_security_vw
with (security_invoker = true) as
select
    series_id,
    tenant_id,
    party_id,
    instrument_type,
    quote_type,
    asset_class,
    identity_kind,
    contract_name,
    future_contract,
    option_type,
    qualifier,
    security_id
from ores_marketdata_market_series_identity_tbl
where identity_kind = 'series'
  and instrument_type in (
        'bond',
        'bond_future',
        'bond_future_option',
        'bond_option',
        'cpr'
    );

-- The shape_profile identity.
create or replace view ores_marketdata_series_identity_shape_profile_vw
with (security_invoker = true) as
select
    series_id,
    tenant_id,
    party_id,
    instrument_type,
    quote_type,
    asset_class,
    identity_kind,
    dst,
    quote_name,
    time_unit
from ores_marketdata_market_series_identity_tbl
where identity_kind = 'series'
  and instrument_type in (
        'shape_profile'
    );

