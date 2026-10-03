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
 * One-shot migration: the equity, commodity and bond future volatility sections
 *
 * Adds the tables that hold the equity, commodity and bond future volatility
 * entries of a curve configuration, and gives the volatility configurations
 * the columns of the Constant, Curve, DeltaSurface and ProxySurface kinds and
 * whether each is written inside a VolatilityConfig element. A quote may now
 * belong to a volatility curve's list.
 *
 * Run after refdata_curve_rate_vols_migration.sql. Every statement adds a
 * column if it is missing, or creates or replaces, so a database that already
 * holds these objects is left as it is.
 */

begin;

alter table ores_refdata_curve_volatility_configs_tbl
    add column if not exists "is_wrapped" boolean not null default false,
    add column if not exists "quote" text,
    add column if not exists "interpolation" text,
    add column if not exists "enforce_monotone_variance" boolean,
    add column if not exists "delta_type" text,
    add column if not exists "atm_type" text,
    add column if not exists "atm_delta_type" text,
    add column if not exists "put_deltas" text,
    add column if not exists "call_deltas" text,
    add column if not exists "future_price_correction" text,
    add column if not exists "proxy_volatility_curve" text,
    add column if not exists "fx_volatility_curve" text,
    add column if not exists "correlation_curve" text,
    add column if not exists "cds_volatility_curve" text;

alter table ores_refdata_curve_quotes_tbl
    drop constraint if exists ores_refdata_curve_quotes_tbl_quote_list_check;
alter table ores_refdata_curve_quotes_tbl
    add constraint ores_refdata_curve_quotes_tbl_quote_list_check
    check (("quote_list" is null or "quote_list" in
            ('BasisQuotes', 'OffPeakQuotes', 'PeakQuotes', 'Curve', 'VolatilityConfig/Curve')));

\ir ../create/refdata/refdata_curve_volatility_configs_create.sql
\ir ../create/refdata/refdata_equity_volatilities_create.sql
\ir ../create/refdata/refdata_equity_volatilities_notify_trigger_create.sql
\ir ../create/refdata/refdata_commodity_volatilities_create.sql
\ir ../create/refdata/refdata_commodity_volatilities_notify_trigger_create.sql
\ir ../create/refdata/refdata_bond_future_volatilities_create.sql
\ir ../create/refdata/refdata_bond_future_volatilities_notify_trigger_create.sql
\ir ../create/refdata/refdata_rls_policies_create.sql

\ir ../populate/iam/iam_permissions_populate.sql

commit;
