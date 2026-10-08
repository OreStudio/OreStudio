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
-- Trade activities
-- -----------------------------------------------------------------------------
alter table ores_trading_trade_activities_tbl enable row level security;

drop policy if exists trade_activities_tenant_isolation_policy
    on ores_trading_trade_activities_tbl;

create policy trade_activities_tenant_isolation_policy on ores_trading_trade_activities_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement, as on trades. FOR SELECT only, so the
-- booking write is checked by its own party validation.
drop policy if exists trade_activities_party_isolation_policy
    on ores_trading_trade_activities_tbl;

create policy trade_activities_party_isolation_policy
on ores_trading_trade_activities_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Trades
-- -----------------------------------------------------------------------------
alter table ores_trading_trades_tbl enable row level security;

drop policy if exists trades_tenant_isolation_policy
    on ores_trading_trades_tbl;

create policy trades_tenant_isolation_policy on ores_trading_trades_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement, as on trades. FOR SELECT only, so the
-- booking write is checked by its own party validation.
drop policy if exists trades_party_isolation_policy
    on ores_trading_trades_tbl;

create policy trades_party_isolation_policy
on ores_trading_trades_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Trade bookings
-- -----------------------------------------------------------------------------
alter table ores_trading_trade_bookings_tbl enable row level security;

drop policy if exists trade_bookings_tenant_isolation_policy
    on ores_trading_trade_bookings_tbl;

create policy trade_bookings_tenant_isolation_policy on ores_trading_trade_bookings_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement, as on the anchor. FOR SELECT only.
drop policy if exists trade_bookings_party_isolation_policy
    on ores_trading_trade_bookings_tbl;

create policy trade_bookings_party_isolation_policy
on ores_trading_trade_bookings_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Trade states
-- -----------------------------------------------------------------------------
alter table ores_trading_trade_states_tbl enable row level security;

drop policy if exists trade_states_tenant_isolation_policy
    on ores_trading_trade_states_tbl;

create policy trade_states_tenant_isolation_policy on ores_trading_trade_states_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement, as on the anchor. FOR SELECT only.
drop policy if exists trade_states_party_isolation_policy
    on ores_trading_trade_states_tbl;

create policy trade_states_party_isolation_policy
on ores_trading_trade_states_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Trade Identifiers
-- -----------------------------------------------------------------------------
alter table ores_trading_trade_identifiers_tbl enable row level security;

drop policy if exists identifiers_tenant_isolation_policy
    on ores_trading_trade_identifiers_tbl;

create policy identifiers_tenant_isolation_policy on ores_trading_trade_identifiers_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement, as on the anchor. FOR SELECT only.
drop policy if exists identifiers_party_isolation_policy
    on ores_trading_trade_identifiers_tbl;

create policy identifiers_party_isolation_policy
on ores_trading_trade_identifiers_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Trade Party Roles
-- -----------------------------------------------------------------------------
alter table ores_trading_party_roles_tbl enable row level security;

drop policy if exists party_roles_tenant_isolation_policy
    on ores_trading_party_roles_tbl;

create policy party_roles_tenant_isolation_policy on ores_trading_party_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement, as on the anchor. FOR SELECT only.
drop policy if exists party_roles_party_isolation_policy
    on ores_trading_party_roles_tbl;

create policy party_roles_party_isolation_policy
on ores_trading_party_roles_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Trade Additional Fields
-- -----------------------------------------------------------------------------
alter table ores_trading_trade_additional_fields_tbl enable row level security;

drop policy if exists trade_additional_fields_tenant_isolation_policy
    on ores_trading_trade_additional_fields_tbl;

create policy trade_additional_fields_tenant_isolation_policy on ores_trading_trade_additional_fields_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement, as on the anchor. FOR SELECT only.
drop policy if exists trade_additional_fields_party_isolation_policy
    on ores_trading_trade_additional_fields_tbl;

create policy trade_additional_fields_party_isolation_policy
on ores_trading_trade_additional_fields_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Trade Portfolios
-- -----------------------------------------------------------------------------
alter table ores_trading_trade_portfolios_tbl enable row level security;

drop policy if exists trade_portfolios_tenant_isolation_policy
    on ores_trading_trade_portfolios_tbl;

create policy trade_portfolios_tenant_isolation_policy on ores_trading_trade_portfolios_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement, as on the anchor. FOR SELECT only.
drop policy if exists trade_portfolios_party_isolation_policy
    on ores_trading_trade_portfolios_tbl;

create policy trade_portfolios_party_isolation_policy
on ores_trading_trade_portfolios_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Trade Links
-- -----------------------------------------------------------------------------
alter table ores_trading_trade_links_tbl enable row level security;

drop policy if exists trade_links_tenant_isolation_policy
    on ores_trading_trade_links_tbl;

create policy trade_links_tenant_isolation_policy on ores_trading_trade_links_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement, as on the anchor. FOR SELECT only. The
-- link holds the party of its from end, and the anchor_party pin keeps that
-- copy true, so this policy bounds the link by the same party as the trade it
-- starts at.
drop policy if exists trade_links_party_isolation_policy
    on ores_trading_trade_links_tbl;

create policy trade_links_party_isolation_policy
on ores_trading_trade_links_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
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

drop policy if exists bond_instruments_tenant_isolation_policy
    on ores_trading_bond_instruments_tbl;

create policy bond_instruments_tenant_isolation_policy on ores_trading_bond_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists bond_instruments_party_isolation_policy
    on ores_trading_bond_instruments_tbl;

create policy bond_instruments_party_isolation_policy
on ores_trading_bond_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Bond relational model (pilot), task D7943D7E
-- -----------------------------------------------------------------------------
-- The instrument row above is the family's party boundary; the issue
-- (one row per ISIN, shared by every party's trades of it), the fact
-- and the child rows carry no party_id, so each gets the tenant
-- isolation policy only.

-- -----------------------------------------------------------------------------
-- Commodity Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_commodity_instruments_tbl enable row level security;

drop policy if exists commodity_instruments_tenant_isolation_policy
    on ores_trading_commodity_instruments_tbl;

create policy commodity_instruments_tenant_isolation_policy on ores_trading_commodity_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists commodity_instruments_party_isolation_policy
    on ores_trading_commodity_instruments_tbl;

create policy commodity_instruments_party_isolation_policy
on ores_trading_commodity_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_option_instruments_tbl enable row level security;

drop policy if exists equity_option_instruments_tenant_isolation_policy
    on ores_trading_equity_option_instruments_tbl;

create policy equity_option_instruments_tenant_isolation_policy on ores_trading_equity_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists equity_option_instruments_party_isolation_policy
    on ores_trading_equity_option_instruments_tbl;

create policy equity_option_instruments_party_isolation_policy
on ores_trading_equity_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Digital Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_digital_option_instruments_tbl enable row level security;

drop policy if exists equity_digital_option_instruments_tenant_isolation_policy
    on ores_trading_equity_digital_option_instruments_tbl;

create policy equity_digital_option_instruments_tenant_isolation_policy on ores_trading_equity_digital_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists equity_digital_option_instruments_party_isolation_policy
    on ores_trading_equity_digital_option_instruments_tbl;

create policy equity_digital_option_instruments_party_isolation_policy
on ores_trading_equity_digital_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Barrier Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_barrier_option_instruments_tbl enable row level security;

drop policy if exists equity_barrier_option_instruments_tenant_isolation_policy
    on ores_trading_equity_barrier_option_instruments_tbl;

create policy equity_barrier_option_instruments_tenant_isolation_policy on ores_trading_equity_barrier_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists equity_barrier_option_instruments_party_isolation_policy
    on ores_trading_equity_barrier_option_instruments_tbl;

create policy equity_barrier_option_instruments_party_isolation_policy
on ores_trading_equity_barrier_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Asian Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_asian_option_instruments_tbl enable row level security;

drop policy if exists equity_asian_option_instruments_tenant_isolation_policy
    on ores_trading_equity_asian_option_instruments_tbl;

create policy equity_asian_option_instruments_tenant_isolation_policy on ores_trading_equity_asian_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists equity_asian_option_instruments_party_isolation_policy
    on ores_trading_equity_asian_option_instruments_tbl;

create policy equity_asian_option_instruments_party_isolation_policy
on ores_trading_equity_asian_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Forward Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_forward_instruments_tbl enable row level security;

drop policy if exists equity_forward_instruments_tenant_isolation_policy
    on ores_trading_equity_forward_instruments_tbl;

create policy equity_forward_instruments_tenant_isolation_policy on ores_trading_equity_forward_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists equity_forward_instruments_party_isolation_policy
    on ores_trading_equity_forward_instruments_tbl;

create policy equity_forward_instruments_party_isolation_policy
on ores_trading_equity_forward_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Variance Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_variance_swap_instruments_tbl enable row level security;

drop policy if exists equity_variance_swap_instruments_tenant_isolation_policy
    on ores_trading_equity_variance_swap_instruments_tbl;

create policy equity_variance_swap_instruments_tenant_isolation_policy on ores_trading_equity_variance_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists equity_variance_swap_instruments_party_isolation_policy
    on ores_trading_equity_variance_swap_instruments_tbl;

create policy equity_variance_swap_instruments_party_isolation_policy
on ores_trading_equity_variance_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_swap_instruments_tbl enable row level security;

drop policy if exists equity_swap_instruments_tenant_isolation_policy
    on ores_trading_equity_swap_instruments_tbl;

create policy equity_swap_instruments_tenant_isolation_policy on ores_trading_equity_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists equity_swap_instruments_party_isolation_policy
    on ores_trading_equity_swap_instruments_tbl;

create policy equity_swap_instruments_party_isolation_policy
on ores_trading_equity_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Accumulator Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_accumulator_instruments_tbl enable row level security;

drop policy if exists equity_accumulator_instruments_tenant_isolation_policy
    on ores_trading_equity_accumulator_instruments_tbl;

create policy equity_accumulator_instruments_tenant_isolation_policy on ores_trading_equity_accumulator_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists equity_accumulator_instruments_party_isolation_policy
    on ores_trading_equity_accumulator_instruments_tbl;

create policy equity_accumulator_instruments_party_isolation_policy
on ores_trading_equity_accumulator_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Equity Position Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_equity_position_instruments_tbl enable row level security;

drop policy if exists equity_position_instruments_tenant_isolation_policy
    on ores_trading_equity_position_instruments_tbl;

create policy equity_position_instruments_tenant_isolation_policy on ores_trading_equity_position_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists equity_position_instruments_party_isolation_policy
    on ores_trading_equity_position_instruments_tbl;

create policy equity_position_instruments_party_isolation_policy
on ores_trading_equity_position_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Credit Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_credit_instruments_tbl enable row level security;

drop policy if exists credit_instruments_tenant_isolation_policy
    on ores_trading_credit_instruments_tbl;

create policy credit_instruments_tenant_isolation_policy on ores_trading_credit_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists credit_instruments_party_isolation_policy
    on ores_trading_credit_instruments_tbl;

create policy credit_instruments_party_isolation_policy
on ores_trading_credit_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Scripted Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_scripted_instruments_tbl enable row level security;

drop policy if exists scripted_instruments_tenant_isolation_policy
    on ores_trading_scripted_instruments_tbl;

create policy scripted_instruments_tenant_isolation_policy on ores_trading_scripted_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists scripted_instruments_party_isolation_policy
    on ores_trading_scripted_instruments_tbl;

create policy scripted_instruments_party_isolation_policy
on ores_trading_scripted_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Composite Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_composite_instruments_tbl enable row level security;

drop policy if exists composite_instruments_tenant_isolation_policy
    on ores_trading_composite_instruments_tbl;

create policy composite_instruments_tenant_isolation_policy on ores_trading_composite_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists composite_instruments_party_isolation_policy
    on ores_trading_composite_instruments_tbl;

create policy composite_instruments_party_isolation_policy
on ores_trading_composite_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Composite Legs
-- -----------------------------------------------------------------------------
alter table ores_trading_composite_legs_tbl enable row level security;

drop policy if exists composite_legs_tenant_isolation_policy
    on ores_trading_composite_legs_tbl;

create policy composite_legs_tenant_isolation_policy on ores_trading_composite_legs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists composite_legs_party_isolation_policy
    on ores_trading_composite_legs_tbl;

create policy composite_legs_party_isolation_policy
on ores_trading_composite_legs_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Swap Legs
-- -----------------------------------------------------------------------------
alter table ores_trading_swap_legs_tbl enable row level security;

drop policy if exists swap_legs_tenant_isolation_policy
    on ores_trading_swap_legs_tbl;

create policy swap_legs_tenant_isolation_policy on ores_trading_swap_legs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists swap_legs_party_isolation_policy
    on ores_trading_swap_legs_tbl;

create policy swap_legs_party_isolation_policy
on ores_trading_swap_legs_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Swap Leg Amounts
-- -----------------------------------------------------------------------------
-- The leg children carry no party_id of their own; their party is the leg's,
-- reached through trade_id. Tenant isolation is therefore the whole policy.
alter table ores_trading_swap_leg_amounts_tbl enable row level security;

drop policy if exists swap_leg_amounts_tenant_isolation_policy
    on ores_trading_swap_leg_amounts_tbl;

create policy swap_leg_amounts_tenant_isolation_policy on ores_trading_swap_leg_amounts_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Swap Leg Rates
-- -----------------------------------------------------------------------------
alter table ores_trading_swap_leg_rates_tbl enable row level security;

drop policy if exists swap_leg_rates_tenant_isolation_policy
    on ores_trading_swap_leg_rates_tbl;

create policy swap_leg_rates_tenant_isolation_policy on ores_trading_swap_leg_rates_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- =============================================================================
-- Rates Instrument Tables
-- =============================================================================

-- -----------------------------------------------------------------------------
-- FRA Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fra_instruments_tbl enable row level security;

drop policy if exists fra_instruments_tenant_isolation_policy
    on ores_trading_fra_instruments_tbl;

create policy fra_instruments_tenant_isolation_policy on ores_trading_fra_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists fra_instruments_party_isolation_policy
    on ores_trading_fra_instruments_tbl;

create policy fra_instruments_party_isolation_policy
on ores_trading_fra_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Vanilla Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_vanilla_swap_instruments_tbl enable row level security;

drop policy if exists vanilla_swap_instruments_tenant_isolation_policy
    on ores_trading_vanilla_swap_instruments_tbl;

create policy vanilla_swap_instruments_tenant_isolation_policy on ores_trading_vanilla_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists vanilla_swap_instruments_party_isolation_policy
    on ores_trading_vanilla_swap_instruments_tbl;

create policy vanilla_swap_instruments_party_isolation_policy
on ores_trading_vanilla_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Cap/Floor Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_cap_floor_instruments_tbl enable row level security;

drop policy if exists cap_floor_instruments_tenant_isolation_policy
    on ores_trading_cap_floor_instruments_tbl;

create policy cap_floor_instruments_tenant_isolation_policy on ores_trading_cap_floor_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists cap_floor_instruments_party_isolation_policy
    on ores_trading_cap_floor_instruments_tbl;

create policy cap_floor_instruments_party_isolation_policy
on ores_trading_cap_floor_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Swaption Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_swaption_instruments_tbl enable row level security;

drop policy if exists swaption_instruments_tenant_isolation_policy
    on ores_trading_swaption_instruments_tbl;

create policy swaption_instruments_tenant_isolation_policy on ores_trading_swaption_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists swaption_instruments_party_isolation_policy
    on ores_trading_swaption_instruments_tbl;

create policy swaption_instruments_party_isolation_policy
on ores_trading_swaption_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Balance Guaranteed Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_balance_guaranteed_swap_instruments_tbl enable row level security;

drop policy if exists bgs_instruments_tenant_isolation_policy
    on ores_trading_balance_guaranteed_swap_instruments_tbl;

create policy bgs_instruments_tenant_isolation_policy on ores_trading_balance_guaranteed_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists bgs_instruments_party_isolation_policy
    on ores_trading_balance_guaranteed_swap_instruments_tbl;

create policy bgs_instruments_party_isolation_policy
on ores_trading_balance_guaranteed_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Callable Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_callable_swap_instruments_tbl enable row level security;

drop policy if exists callable_swap_instruments_tenant_isolation_policy
    on ores_trading_callable_swap_instruments_tbl;

create policy callable_swap_instruments_tenant_isolation_policy on ores_trading_callable_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists callable_swap_instruments_party_isolation_policy
    on ores_trading_callable_swap_instruments_tbl;

create policy callable_swap_instruments_party_isolation_policy
on ores_trading_callable_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Knock-Out Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_knock_out_swap_instruments_tbl enable row level security;

drop policy if exists knock_out_swap_instruments_tenant_isolation_policy
    on ores_trading_knock_out_swap_instruments_tbl;

create policy knock_out_swap_instruments_tenant_isolation_policy on ores_trading_knock_out_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists knock_out_swap_instruments_party_isolation_policy
    on ores_trading_knock_out_swap_instruments_tbl;

create policy knock_out_swap_instruments_party_isolation_policy
on ores_trading_knock_out_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Inflation Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_inflation_swap_instruments_tbl enable row level security;

drop policy if exists inflation_swap_instruments_tenant_isolation_policy
    on ores_trading_inflation_swap_instruments_tbl;

create policy inflation_swap_instruments_tenant_isolation_policy on ores_trading_inflation_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists inflation_swap_instruments_party_isolation_policy
    on ores_trading_inflation_swap_instruments_tbl;

create policy inflation_swap_instruments_party_isolation_policy
on ores_trading_inflation_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Risk Participation Agreement Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_rpa_instruments_tbl enable row level security;

drop policy if exists rpa_instruments_tenant_isolation_policy
    on ores_trading_rpa_instruments_tbl;

create policy rpa_instruments_tenant_isolation_policy on ores_trading_rpa_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists rpa_instruments_party_isolation_policy
    on ores_trading_rpa_instruments_tbl;

create policy rpa_instruments_party_isolation_policy
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

drop policy if exists fx_forward_instruments_tenant_isolation_policy
    on ores_trading_fx_forward_instruments_tbl;

create policy fx_forward_instruments_tenant_isolation_policy on ores_trading_fx_forward_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists fx_forward_instruments_party_isolation_policy
    on ores_trading_fx_forward_instruments_tbl;

create policy fx_forward_instruments_party_isolation_policy
on ores_trading_fx_forward_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Vanilla Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_vanilla_option_instruments_tbl enable row level security;

drop policy if exists fx_vanilla_option_instruments_tenant_isolation_policy
    on ores_trading_fx_vanilla_option_instruments_tbl;

create policy fx_vanilla_option_instruments_tenant_isolation_policy on ores_trading_fx_vanilla_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists fx_vanilla_option_instruments_party_isolation_policy
    on ores_trading_fx_vanilla_option_instruments_tbl;

create policy fx_vanilla_option_instruments_party_isolation_policy
on ores_trading_fx_vanilla_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Barrier Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_barrier_option_instruments_tbl enable row level security;

drop policy if exists fx_barrier_option_instruments_tenant_isolation_policy
    on ores_trading_fx_barrier_option_instruments_tbl;

create policy fx_barrier_option_instruments_tenant_isolation_policy on ores_trading_fx_barrier_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists fx_barrier_option_instruments_party_isolation_policy
    on ores_trading_fx_barrier_option_instruments_tbl;

create policy fx_barrier_option_instruments_party_isolation_policy
on ores_trading_fx_barrier_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Digital Option Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_digital_option_instruments_tbl enable row level security;

drop policy if exists fx_digital_option_instruments_tenant_isolation_policy
    on ores_trading_fx_digital_option_instruments_tbl;

create policy fx_digital_option_instruments_tenant_isolation_policy on ores_trading_fx_digital_option_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists fx_digital_option_instruments_party_isolation_policy
    on ores_trading_fx_digital_option_instruments_tbl;

create policy fx_digital_option_instruments_party_isolation_policy
on ores_trading_fx_digital_option_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Asian Forward Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_asian_forward_instruments_tbl enable row level security;

drop policy if exists fx_asian_forward_instruments_tenant_isolation_policy
    on ores_trading_fx_asian_forward_instruments_tbl;

create policy fx_asian_forward_instruments_tenant_isolation_policy on ores_trading_fx_asian_forward_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists fx_asian_forward_instruments_party_isolation_policy
    on ores_trading_fx_asian_forward_instruments_tbl;

create policy fx_asian_forward_instruments_party_isolation_policy
on ores_trading_fx_asian_forward_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Accumulator Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_accumulator_instruments_tbl enable row level security;

drop policy if exists fx_accumulator_instruments_tenant_isolation_policy
    on ores_trading_fx_accumulator_instruments_tbl;

create policy fx_accumulator_instruments_tenant_isolation_policy on ores_trading_fx_accumulator_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists fx_accumulator_instruments_party_isolation_policy
    on ores_trading_fx_accumulator_instruments_tbl;

create policy fx_accumulator_instruments_party_isolation_policy
on ores_trading_fx_accumulator_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX Variance Swap Instruments
-- -----------------------------------------------------------------------------
alter table ores_trading_fx_variance_swap_instruments_tbl enable row level security;

drop policy if exists fx_variance_swap_instruments_tenant_isolation_policy
    on ores_trading_fx_variance_swap_instruments_tbl;

create policy fx_variance_swap_instruments_tenant_isolation_policy on ores_trading_fx_variance_swap_instruments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists fx_variance_swap_instruments_party_isolation_policy
    on ores_trading_fx_variance_swap_instruments_tbl;

create policy fx_variance_swap_instruments_party_isolation_policy
on ores_trading_fx_variance_swap_instruments_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);
