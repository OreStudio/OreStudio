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
 * ORE Envelope View
 *
 * Flattens the latest active version of each trade into the ORE <Envelope>
 * structure (TradeID, TradeType, Counterparty, NettingSetId).
 *
 * The counterparty is resolved via the party role with role = 'Counterparty'.
 * Trades without a Counterparty role are excluded (INNER JOIN).
 *
 * For historical queries ("state of the portfolio at time X"), replace the
 * valid_to = ores_utility_infinity_timestamp_fn() conditions with:
 *   valid_from <= X AND (valid_to = ores_utility_infinity_timestamp_fn() OR valid_to > X)
 */

create or replace view ores_trade_ore_envelope_vw as
select
    t.id                    as trade_id,
    t.tenant_id,
    t.trade_type,
    t.netting_set_id,
    cp.full_name            as counterparty,
    cp.id                   as counterparty_id,
    t.portfolio_id,
    p.name                  as portfolio_name,
    t.book_id,
    t.trade_date,
    t.effective_date,
    t.termination_date,
    t.lifecycle_event,
    t.execution_timestamp,
    t.external_id,
    t.version,
    t.modified_by,
    t.valid_from
from ores_trading_trades_tbl t
join ores_trading_party_roles_tbl tpr
    on t.tenant_id = tpr.tenant_id
    and t.id = tpr.trade_id
    and tpr.role = 'Counterparty'
    and tpr.valid_to = ores_utility_infinity_timestamp_fn()
join ores_refdata_counterparties_tbl cp
    on tpr.tenant_id = cp.tenant_id
    and tpr.counterparty_id = cp.id
    and cp.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_refdata_portfolios_tbl p
    on t.tenant_id = p.tenant_id
    and t.portfolio_id = p.id
    and p.valid_to = ores_utility_infinity_timestamp_fn()
where t.valid_to = ores_utility_infinity_timestamp_fn();
