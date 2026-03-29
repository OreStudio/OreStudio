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
-- Instruments (Rates) Extensions (Phase 7)
--
-- Adds nullable columns to support ForwardRateAgreement, BalanceGuaranteedSwap,
-- CallableSwap, KnockOutSwap, RiskParticipationAgreement, and InflationSwap
-- trade types. All columns are nullable so that existing rows are unaffected.
-- =============================================================================

alter table "ores_trading_instruments_tbl"
    add column if not exists "fra_fixing_date"      date             null,
    add column if not exists "fra_settlement_date"  date             null,
    add column if not exists "lockout_days"         integer          null,
    add column if not exists "callable_dates_json"  text             null,
    add column if not exists "rpa_counterparty"     text             null,
    add column if not exists "inflation_index_code" text             null,
    add column if not exists "base_cpi"             numeric(28, 10)  null;
