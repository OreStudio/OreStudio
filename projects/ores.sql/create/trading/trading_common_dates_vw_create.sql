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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_trade_type_common_dates_create.mustache
 *
 * Common-date views
 *
 * Generated from ores.trading.trade_type_catalogue and the :common_date:
 * declarations on the family models.
 */

-- The dates rate_instrument maps:
--   start_date <- rate_instrument.start_date
--   maturity_date <- rate_instrument.maturity_date
--   expiry_date <- swaption_instrument.expiry_date
create or replace view ores_trading_rate_instruments_common_dates_vw
with (security_invoker = true) as
select
    h.tenant_id,
    h.trade_id,
    j2.trade_date as trade_date,
    h.start_date as start_date,
    j1.expiry_date as expiry_date,
    h.maturity_date as maturity_date
from ores_trading_rate_instruments_tbl h
left join ores_trading_swaption_instruments_tbl j1
  on j1.tenant_id = h.tenant_id
 and j1.trade_id = h.trade_id
 and j1.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_trading_trade_bookings_tbl j2
  on j2.tenant_id = h.tenant_id
 and j2.trade_id = h.trade_id
 and j2.valid_to = ores_utility_infinity_timestamp_fn()
where h.valid_to = ores_utility_infinity_timestamp_fn();

