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
 * ORE Series Key Shapes Population Script
 *
 * The key grammar of ORE market data, one row per series type. Every key
 * follows TYPE/METRIC/[QUALIFIER...]/[POINT_ID]; qualifier_depth counts the
 * segments after the metric that identify the series and stay stable across
 * market dates, and every remaining segment is the point.
 *
 * The first thirty-five rows are the shape table that used to be compiled
 * into ores.ore.core; each description is the comment that stood above the
 * entry. The last eight are the types the examples corpus carries but that
 * table never knew, so their keys folded whole into the qualifier and every
 * distinct key became its own single-observation series.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- ORE Series Key Shapes ---'

insert into ores_ore_series_key_shapes_tbl (
    tenant_id, series_type, version, qualifier_depth, has_point_dimension,
    default_point, description,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    -- ─── FX ──────────────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'FX', 0, 2, false, 'SPOT',
     'FX/RATE/ccy1/ccy2 -- spot, whose single point is the real tenor SPOT.',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'FXFWD', 0, 2, true, '',
     'FXFWD/RATE/ccy1/ccy2/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'FX_OPTION', 0, 2, true, '',
     'FX_OPTION/metric/ccy1/ccy2/expiry/delta_or_strike',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),

    -- ─── RATES: curves ───────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'DISCOUNT', 0, 2, true, '',
     'DISCOUNT/RATE/ccy/curve_id/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'ZERO', 0, 3, true, '',
     'ZERO/RATE/ccy/curve_id/day_count/tenor (day_count = A365, ActAct, ...)',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'MM', 0, 2, true, '',
     'MM/RATE/ccy/index_tenor/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'MM_FUTURE', 0, 2, true, '',
     'MM_FUTURE/RATE/ccy/expiry/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'FRA', 0, 2, true, '',
     'FRA/RATE/ccy/start_tenor/length',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'IMM_FRA', 0, 2, true, '',
     'IMM_FRA/RATE/ccy/imm_date/length',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'IR_SWAP', 0, 3, true, '',
     'IR_SWAP/RATE/ccy/settle/index_tenor/maturity',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),

    -- ─── RATES: spreads ──────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'BASIS_SWAP', 0, 3, true, '',
     'BASIS_SWAP/BASIS_SPREAD/long_tenor/short_tenor/ccy/maturity',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'BMA_SWAP', 0, 2, true, '',
     'BMA_SWAP/RATIO/ccy/index_tenor/maturity',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'CC_BASIS_SWAP', 0, 4, true, '',
     'CC_BASIS_SWAP/BASIS_SPREAD/ccy1/tenor1/ccy2/tenor2/maturity',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'CC_FIX_FLOAT_SWAP', 0, 4, true, '',
     'CC_FIX_FLOAT_SWAP/SPREAD/ccy1/tenor1/ccy2/tenor2/maturity',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),

    -- ─── RATES: vol surfaces ─────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'SWAPTION', 0, 1, true, '',
     'SWAPTION/metric/ccy/expiry/swap_tenor/strike',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'CAPFLOOR', 0, 1, true, '',
     'CAPFLOOR/metric/ccy/maturity/index_tenor/.../strike',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),

    -- ─── CREDIT ──────────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'HAZARD_RATE', 0, 3, true, '',
     'HAZARD_RATE/RATE/entity/seniority/ccy/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'CDS', 0, 3, true, '',
     'CDS/SPREAD/entity/seniority/ccy/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'CDS_INDEX', 0, 2, true, '',
     'CDS_INDEX/SPREAD/index_family/index_term/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'INDEX_CDS_OPTION', 0, 2, true, '',
     'INDEX_CDS_OPTION/metric/index_family/index_term/expiry/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'RECOVERY_RATE', 0, 3, false, '',
     'RECOVERY_RATE/RATE/entity/seniority/ccy -- one rate per entity, no tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'RATING', 0, 1, true, '',
     'RATING/TRANSITION_PROBABILITY/agency/from_grade/to_grade/tenor -- the agency names the series; the grade pair and the tenor form the point.',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'INDEX_CDS_TRANCHE', 0, 1, true, '',
     'INDEX_CDS_TRANCHE/BASE_CORRELATION/index_id/tenor/detachment_point',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'CPR', 0, 1, false, '',
     'CPR/RATE/ISIN -- one prepayment rate per security, no surface',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),

    -- ─── EQUITY ──────────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'EQUITY', 0, 1, false, 'SPOT',
     'EQUITY/PRICE/name -- spot',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'EQUITY_FWD', 0, 1, true, '',
     'EQUITY_FWD/PRICE/name/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'EQUITY_DIVIDEND', 0, 1, true, '',
     'EQUITY_DIVIDEND/RATE/name/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'EQUITY_OPTION', 0, 2, true, '',
     'EQUITY_OPTION/metric/name/ccy/expiry/strike',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),

    -- ─── COMMODITY ───────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'COMMODITY', 0, 1, false, 'SPOT',
     'COMMODITY/PRICE/name -- spot',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'COMMODITY_FWD', 0, 2, true, '',
     'COMMODITY_FWD/PRICE/name/ccy/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'COMMODITY_OPTION', 0, 2, true, '',
     'COMMODITY_OPTION/metric/name/ccy/expiry/strike',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'OI_FUTURE', 0, 1, true, '',
     'OI_FUTURE/PRICE/ccy/contract_month/exchange:code/tenor -- the overnight-index future of a listed contract month.',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'SHAPE_PROFILE', 0, 1, true, '',
     'SHAPE_PROFILE/SHAPE_FACTOR/profile_name/... -- a power price shape; one series per named profile, and the segments after it form the point.',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),

    -- ─── INFLATION ───────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'ZC_INFLATIONSWAP', 0, 1, true, '',
     'ZC_INFLATIONSWAP/RATE/index/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'YY_INFLATIONSWAP', 0, 1, true, '',
     'YY_INFLATIONSWAP/RATE/index/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'ZC_INFLATIONCAPFLOOR', 0, 1, true, '',
     'ZC_INFLATIONCAPFLOOR/metric/index/maturity/.../strike',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'YY_INFLATIONCAPFLOOR', 0, 1, true, '',
     'YY_INFLATIONCAPFLOOR/metric/index/maturity/.../strike',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'SEASONALITY', 0, 1, true, '',
     'SEASONALITY/RATE/index/month_id',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),

    -- ─── BOND ────────────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'BOND', 0, 2, true, '',
     'BOND/PRICE/issuer/seniority/tenor and BOND/YIELD_SPREAD/issuer/seniority/tenor',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'BOND_OPTION', 0, 1, true, '',
     'BOND_OPTION/RATE_LNVOL/underlying/expiry/bond_tenor/strike -- the same shape as SWAPTION, over a bond instead of a swap.',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),

    -- ─── CROSS_ASSET ─────────────────────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'CORRELATION', 0, 2, true, '',
     'CORRELATION/RATE/operand1/operand2/expiry/strike -- the two operands hold fixed positions, so the surface coordinate after them is the point.',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),

    -- ─── WRAPPERS AND SYNTHETIC KEYS ─────────────────────────────────────────
    (ores_utility_system_tenant_id_fn(), 'GENERIC-MD', 0, 1, true, '',
     'GENERIC-MD/inner_type/inner_metric/... -- a wrapper, so the inner metric names the series and the rest is the point.',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes'),
    (ores_utility_system_tenant_id_fn(), 'FIXING', 0, 1, false, '',
     'FIXING/index_name -- a synthetic key the import builds for an index fixing, which carries no point of its own.',
     current_user, current_user, 'system.initial_load', 'Initial population of ORE series key shapes')
on conflict (tenant_id, series_type)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'ore_series_key_shapes' as entity, count(*) as count
from ores_ore_series_key_shapes_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
