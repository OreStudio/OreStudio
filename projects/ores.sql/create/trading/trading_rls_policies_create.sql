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
-- Instrument Tables
-- =============================================================================
-- Each standalone instrument table carries party_id directly.
-- Party isolation is enforced via a restrictive policy on each table.
-- Policies are added here as each instrument family table is implemented.

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

create policy ores_trading_bond_instruments_party_isolation_policy
on ores_trading_bond_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
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

create policy ores_trading_commodity_instruments_party_isolation_policy
on ores_trading_commodity_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_option_instruments_tbl enable row level security;

create policy ores_trading_equity_option_instruments_tenant_isolation_policy on ores_trading_equity_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_equity_option_instruments_party_isolation_policy
on ores_trading_equity_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Digital Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_digital_option_instruments_tbl enable row level security;

create policy ores_trading_equity_digital_option_instruments_tenant_isolation_policy on ores_trading_equity_digital_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_equity_digital_option_instruments_party_isolation_policy
on ores_trading_equity_digital_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Barrier Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_barrier_option_instruments_tbl enable row level security;

create policy ores_trading_equity_barrier_option_instruments_tenant_isolation_policy on ores_trading_equity_barrier_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_equity_barrier_option_instruments_party_isolation_policy
on ores_trading_equity_barrier_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Asian Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_asian_option_instruments_tbl enable row level security;

create policy ores_trading_equity_asian_option_instruments_tenant_isolation_policy on ores_trading_equity_asian_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_equity_asian_option_instruments_party_isolation_policy
on ores_trading_equity_asian_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Forward Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_forward_instruments_tbl enable row level security;

create policy ores_trading_equity_forward_instruments_tenant_isolation_policy on ores_trading_equity_forward_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_equity_forward_instruments_party_isolation_policy
on ores_trading_equity_forward_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Variance Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_variance_swap_instruments_tbl enable row level security;

create policy ores_trading_equity_variance_swap_instruments_tenant_isolation_policy on ores_trading_equity_variance_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_equity_variance_swap_instruments_party_isolation_policy
on ores_trading_equity_variance_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_swap_instruments_tbl enable row level security;

create policy ores_trading_equity_swap_instruments_tenant_isolation_policy on ores_trading_equity_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_equity_swap_instruments_party_isolation_policy
on ores_trading_equity_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Accumulator Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_accumulator_instruments_tbl enable row level security;

create policy ores_trading_equity_accumulator_instruments_tenant_isolation_policy on ores_trading_equity_accumulator_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_equity_accumulator_instruments_party_isolation_policy
on ores_trading_equity_accumulator_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Position Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_position_instruments_tbl enable row level security;

create policy ores_trading_equity_position_instruments_tenant_isolation_policy on ores_trading_equity_position_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_equity_position_instruments_party_isolation_policy
on ores_trading_equity_position_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
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

create policy ores_trading_credit_instruments_party_isolation_policy
on ores_trading_credit_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
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

create policy ores_trading_scripted_instruments_party_isolation_policy
on ores_trading_scripted_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
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

create policy ores_trading_composite_instruments_party_isolation_policy
on ores_trading_composite_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
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

create policy ores_trading_composite_legs_party_isolation_policy
on ores_trading_composite_legs_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
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

create policy ores_trading_swap_legs_party_isolation_policy
on ores_trading_swap_legs_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- =============================================================================
-- Rates Instrument Tables
-- =============================================================================

-- -----------------------------------------------------------------------------
-- FRA Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fra_instruments_tbl enable row level security;

create policy ores_trading_fra_instruments_tenant_isolation_policy on ores_trading_fra_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_fra_instruments_party_isolation_policy
on ores_trading_fra_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Vanilla Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_vanilla_swap_instruments_tbl enable row level security;

create policy ores_trading_vanilla_swap_instruments_tenant_isolation_policy on ores_trading_vanilla_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_vanilla_swap_instruments_party_isolation_policy
on ores_trading_vanilla_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Cap/Floor Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_cap_floor_instruments_tbl enable row level security;

create policy ores_trading_cap_floor_instruments_tenant_isolation_policy on ores_trading_cap_floor_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_cap_floor_instruments_party_isolation_policy
on ores_trading_cap_floor_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Swaption Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_swaption_instruments_tbl enable row level security;

create policy ores_trading_swaption_instruments_tenant_isolation_policy on ores_trading_swaption_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_swaption_instruments_party_isolation_policy
on ores_trading_swaption_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Balance Guaranteed Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_balance_guaranteed_swap_instruments_tbl enable row level security;

create policy ores_trading_bgs_instruments_tenant_isolation_policy on ores_trading_balance_guaranteed_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_bgs_instruments_party_isolation_policy
on ores_trading_balance_guaranteed_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Callable Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_callable_swap_instruments_tbl enable row level security;

create policy ores_trading_callable_swap_instruments_tenant_isolation_policy on ores_trading_callable_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_callable_swap_instruments_party_isolation_policy
on ores_trading_callable_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Knock-Out Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_knock_out_swap_instruments_tbl enable row level security;

create policy ores_trading_knock_out_swap_instruments_tenant_isolation_policy on ores_trading_knock_out_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_knock_out_swap_instruments_party_isolation_policy
on ores_trading_knock_out_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Inflation Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_inflation_swap_instruments_tbl enable row level security;

create policy ores_trading_inflation_swap_instruments_tenant_isolation_policy on ores_trading_inflation_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_inflation_swap_instruments_party_isolation_policy
on ores_trading_inflation_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Risk Participation Agreement Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_rpa_instruments_tbl enable row level security;

create policy ores_trading_rpa_instruments_tenant_isolation_policy on ores_trading_rpa_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_rpa_instruments_party_isolation_policy
on ores_trading_rpa_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- =============================================================================
-- Per-Type FX Instrument Tables (Phase 2)
-- =============================================================================

-- -----------------------------------------------------------------------------
-- FX Forward Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_forward_instruments_tbl enable row level security;

create policy ores_trading_fx_forward_instruments_tenant_isolation_policy on ores_trading_fx_forward_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_fx_forward_instruments_party_isolation_policy
on ores_trading_fx_forward_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Vanilla Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_vanilla_option_instruments_tbl enable row level security;

create policy ores_trading_fx_vanilla_option_instruments_tenant_isolation_policy on ores_trading_fx_vanilla_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_fx_vanilla_option_instruments_party_isolation_policy
on ores_trading_fx_vanilla_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Barrier Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_barrier_option_instruments_tbl enable row level security;

create policy ores_trading_fx_barrier_option_instruments_tenant_isolation_policy on ores_trading_fx_barrier_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_fx_barrier_option_instruments_party_isolation_policy
on ores_trading_fx_barrier_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Digital Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_digital_option_instruments_tbl enable row level security;

create policy ores_trading_fx_digital_option_instruments_tenant_isolation_policy on ores_trading_fx_digital_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_fx_digital_option_instruments_party_isolation_policy
on ores_trading_fx_digital_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Asian Forward Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_asian_forward_instruments_tbl enable row level security;

create policy ores_trading_fx_asian_forward_instruments_tenant_isolation_policy on ores_trading_fx_asian_forward_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_fx_asian_forward_instruments_party_isolation_policy
on ores_trading_fx_asian_forward_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Accumulator Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_accumulator_instruments_tbl enable row level security;

create policy ores_trading_fx_accumulator_instruments_tenant_isolation_policy on ores_trading_fx_accumulator_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_fx_accumulator_instruments_party_isolation_policy
on ores_trading_fx_accumulator_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Variance Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_variance_swap_instruments_tbl enable row level security;

create policy ores_trading_fx_variance_swap_instruments_tenant_isolation_policy on ores_trading_fx_variance_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_trading_fx_variance_swap_instruments_party_isolation_policy
on ores_trading_fx_variance_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);
