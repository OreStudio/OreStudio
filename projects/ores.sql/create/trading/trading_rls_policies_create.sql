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

-- =============================================================================
-- Row-Level Security Policies for Trade Tables
-- =============================================================================
-- These policies enforce strict tenant isolation for trade tables.

-- -----------------------------------------------------------------------------
-- Trade Types
-- -----------------------------------------------------------------------------
alter table ores_trading_trade_types_tbl enable row level security;

create policy ores_trading_trade_types_tenant_isolation_policy on ores_trading_trade_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- FpML Event Types
-- -----------------------------------------------------------------------------
alter table ores_trading_fpml_event_types_tbl enable row level security;

create policy ores_trading_fpml_event_types_tenant_isolation_policy on ores_trading_fpml_event_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Activity Types
-- -----------------------------------------------------------------------------
alter table ores_trading_activity_types_tbl enable row level security;

create policy ores_trading_activity_types_tenant_isolation_policy on ores_trading_activity_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Party Role Types
-- -----------------------------------------------------------------------------
alter table ores_trading_party_role_types_tbl enable row level security;

create policy ores_trading_party_role_types_tenant_isolation_policy on ores_trading_party_role_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Trade Identifier Types
-- -----------------------------------------------------------------------------
alter table ores_trading_trade_id_types_tbl enable row level security;

create policy ores_trading_trade_id_types_tenant_isolation_policy on ores_trading_trade_id_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Trades
-- -----------------------------------------------------------------------------
alter table ores_trading_trades_tbl enable row level security;

create policy ores_trading_trades_tenant_isolation_policy on ores_trading_trades_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement — no party context means no rows visible.
-- party_id is denormalised from book_id by the insert trigger.
-- FOR SELECT only: party_id is auto-populated by trigger; WITH CHECK would
-- block inserts from the publisher where the new party is not yet in the session.
create policy ores_trading_trades_party_isolation_policy
on ores_trading_trades_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Trade Identifiers
-- -----------------------------------------------------------------------------
alter table ores_trading_identifiers_tbl enable row level security;

create policy ores_trading_identifiers_tenant_isolation_policy on ores_trading_identifiers_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Trade Party Roles
-- -----------------------------------------------------------------------------
alter table ores_trading_party_roles_tbl enable row level security;

create policy ores_trading_party_roles_tenant_isolation_policy on ores_trading_party_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- =============================================================================
-- Instrument Subtables
-- =============================================================================
-- Each instrument subtype table carries tenant_id but no party_id.
-- Tenant-level isolation is sufficient since the parent instruments table
-- already enforces party isolation. These policies harden direct-query access.

-- -----------------------------------------------------------------------------
-- Instruments (parent)
-- -----------------------------------------------------------------------------
alter table ores_trading_instruments_tbl enable row level security;

create policy ores_trading_instruments_tenant_isolation_policy on ores_trading_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Bond Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_bond_instruments_tbl enable row level security;

create policy ores_trading_bond_instruments_tenant_isolation_policy on ores_trading_bond_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Commodity Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_commodity_instruments_tbl enable row level security;

create policy ores_trading_commodity_instruments_tenant_isolation_policy on ores_trading_commodity_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Equity Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_instruments_tbl enable row level security;

create policy ores_trading_equity_instruments_tenant_isolation_policy on ores_trading_equity_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- FX Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_instruments_tbl enable row level security;

create policy ores_trading_fx_instruments_tenant_isolation_policy on ores_trading_fx_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Credit Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_credit_instruments_tbl enable row level security;

create policy ores_trading_credit_instruments_tenant_isolation_policy on ores_trading_credit_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Scripted Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_scripted_instruments_tbl enable row level security;

create policy ores_trading_scripted_instruments_tenant_isolation_policy on ores_trading_scripted_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Composite Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_composite_instruments_tbl enable row level security;

create policy ores_trading_composite_instruments_tenant_isolation_policy on ores_trading_composite_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Composite Legs
-- -----------------------------------------------------------------------------
alter table ores_trading_composite_legs_tbl enable row level security;

create policy ores_trading_composite_legs_tenant_isolation_policy on ores_trading_composite_legs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Swap Legs
-- -----------------------------------------------------------------------------
alter table ores_trading_swap_legs_tbl enable row level security;

create policy ores_trading_swap_legs_tenant_isolation_policy on ores_trading_swap_legs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
