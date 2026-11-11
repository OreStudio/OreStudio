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
 * The EUR/USD FX Volatility Surface Blueprint
 *
 * Seeds the first surface recipe, for the system party, the only party the derived
 * source's quotes are subscribed for. The blueprint fits SSVI, joins its expiries in
 * total variance, and writes its grid to the fx_vol_surface series that
 * marketdata_market_series_fx_vol_surface_populate.sql seeds.
 *
 * It has one pillar for each expiry and label the derived source quotes from the
 * fixings it holds: 1W to 1Y. The source lists 2Y, but quotes it only from two years
 * of fixings, which the seeded history does not yet reach. Every
 * pillar has one source, the pair's derived series, which holds every quote: the
 * pillar's expiry and label find the quote inside it. The grid stays inside the
 * pillars' expiries, because the calibration refuses a grid expiry outside them.
 *
 * This script is idempotent.
 */

\echo '--- EUR/USD FX Volatility Surface Blueprint ---'

insert into ores_refdata_fx_vol_surface_blueprints_tbl (
    id, tenant_id, version, output_series_id, party_id, name,
    base_currency_code, quote_currency_code, fx_option_convention_id,
    smile_model, expiry_interpolation, output_expiries, output_strikes,
    modified_by, performed_by, change_reason_code, change_commentary
)
select
    'c7b2d9e4-3a61-4e85-b0f7-8e1d6c4a2f01'::uuid,
    ores_utility_system_tenant_id_fn(),
    0,
    'a4e1c6d2-7b35-4f08-9c1e-5d2f8a7b3c02'::uuid,
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'EURUSD_SSVI', 'EUR', 'USD', 'EUR-USD-FXOPTION',
    'ssvi', 'linear_total_variance',
    '1W,1M,2M,3M,6M,9M,1Y',
    'DEL/Spot/Put/10,DEL/Spot/Put/25,ATM,DEL/Spot/Call/25,DEL/Spot/Call/10',
    current_user, current_user, 'system.initial_load',
    'Initial population of the EUR/USD surface blueprint'
where not exists (
    select 1 from ores_refdata_fx_vol_surface_blueprints_tbl b
    where b.tenant_id = ores_utility_system_tenant_id_fn()
      and b.id = 'c7b2d9e4-3a61-4e85-b0f7-8e1d6c4a2f01'
      and b.valid_to = ores_utility_infinity_timestamp_fn()
);

create temporary table t_eurusd_surface_pillars (
    id uuid, expiry_code text, strike_label text
) on commit drop;

insert into t_eurusd_surface_pillars (id, expiry_code, strike_label)
select md5('fx_vol_surface_pillar|EURUSD_SSVI|' || e.code || '|' || l.label)::uuid, e.code, l.label
from (values ('1W'), ('1M'), ('2M'), ('3M'), ('6M'), ('9M'), ('1Y')) as e (code)
cross join (values ('ATM'), ('25RR'), ('25BF'), ('10RR'), ('10BF')) as l (label);

insert into ores_refdata_fx_vol_surface_pillars_tbl (
    id, tenant_id, version, party_id, blueprint_id, expiry_code, strike_label,
    modified_by, performed_by, change_reason_code, change_commentary
)
select t.id, ores_utility_system_tenant_id_fn(), 0,
       ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
       'c7b2d9e4-3a61-4e85-b0f7-8e1d6c4a2f01'::uuid, t.expiry_code, t.strike_label,
       current_user, current_user, 'system.initial_load',
       'Initial population of the EUR/USD surface pillars'
from t_eurusd_surface_pillars t
where not exists (
    select 1 from ores_refdata_fx_vol_surface_pillars_tbl p
    where p.tenant_id = ores_utility_system_tenant_id_fn()
      and p.id = t.id
      and p.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_refdata_fx_vol_surface_pillar_sources_tbl (
    id, tenant_id, version, party_id, pillar_id, market_series_id, contributor_code, weight,
    modified_by, performed_by, change_reason_code, change_commentary
)
select md5('fx_vol_surface_pillar_source|derived|' || t.id::text)::uuid,
       ores_utility_system_tenant_id_fn(), 0,
       ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
       t.id, 'a4e1c6d2-7b35-4f08-9c1e-5d2f8a7b3c01'::uuid, 'derived', 1.0,
       current_user, current_user, 'system.initial_load',
       'Initial population of the EUR/USD surface pillar sources'
from t_eurusd_surface_pillars t
where not exists (
    select 1 from ores_refdata_fx_vol_surface_pillar_sources_tbl s
    where s.tenant_id = ores_utility_system_tenant_id_fn()
      and s.pillar_id = t.id
      and s.market_series_id = 'a4e1c6d2-7b35-4f08-9c1e-5d2f8a7b3c01'
      and s.valid_to = ores_utility_infinity_timestamp_fn()
);
