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
 * Derived Term Index Configs Population Script
 *
 * The term indices the generated trades book that no publisher quotes in the
 * run, each declared as a curve config that states its rate from its
 * currency's overnight curve rather than from a process of its own:
 *
 *   USD SOFR 1M      compounded over the overnight curve
 *   COP IBR 3M       compounded over the overnight curve
 *   JPY EYTIBOR 3M   TONA plus a stated spread
 *   KRW GENERIC 3M   KOFR plus a stated spread
 *   PLN WIBOR 6M     POLONIA plus a stated spread
 *
 * A compounded index is the overnight rate both curves already agree on, so it
 * states no spread. A spread index states the points and who states them: the
 * fixing of a term index that no one publishes is a market convention, and a
 * spread without its source is a number nobody can check.
 *
 * Each row hangs under the same generation config (the dataset container) and
 * party as its currency's overnight config, so it is one collection with the
 * curve it reads. The container must already exist: this script runs after the
 * uniform demo, which seeds those curves, and it skips a currency with no
 * overnight config rather than inventing one.
 *
 * They start with the service, as the overnight curves they read do: a derived
 * index that needed a person to start it would answer on no fresh provision.
 *
 * This script is idempotent: a row already present on its natural key is left
 * alone, so a rerun never resets a config a person has edited.
 */

\echo '--- Derived provider and collection ---'

-- A derived index is a collection of its own, so it has a provider and a
-- container like every other generated feed. The provider is the one the
-- parties already subscribe to; the container hangs under the same dataset and
-- party as the demo collection it refines, so one publish carries both.
insert into ores_synthetic_market_data_providers_tbl (
    code, tenant_id, version, name, kind, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select 'derived', ores_utility_system_tenant_id_fn(), 0, 'Derived', 'reference',
       'Market data the platform computes from data it already holds: the term indices no publisher quotes in the run, stated from their currency''s overnight curve.',
       40, current_user, current_user, 'system.initial_load',
       'Initial population of the derived provider', now(),
       ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_synthetic_market_data_providers_tbl p
    where p.tenant_id = ores_utility_system_tenant_id_fn()
      and p.code = 'derived'
      and p.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_synthetic_market_data_generation_configs_tbl (
    id, tenant_id, version, party_id, name, description, enabled, dataset_id,
    provider_code, modified_by, performed_by, change_reason_code,
    change_commentary, valid_from, valid_to
)
select gen_random_uuid(), d.tenant_id, 0, d.party_id, 'Derived',
       'The term indices the run states from its overnight curves, for the trades that float on them.',
       true, d.dataset_id, 'derived', current_user, current_user,
       'system.initial_load', 'Initial population of the derived collection',
       now(), ores_utility_infinity_timestamp_fn()
from ores_synthetic_market_data_generation_configs_tbl d
where d.tenant_id = ores_utility_system_tenant_id_fn()
  and d.provider_code = 'demo'
  and d.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_synthetic_market_data_generation_configs_tbl c
      where c.tenant_id = ores_utility_system_tenant_id_fn()
        and c.provider_code = 'derived'
        and c.valid_to = ores_utility_infinity_timestamp_fn()
  )
limit 1;

\echo '--- Derived Term Index Configs ---'

insert into ores_synthetic_ir_curve_generation_configs_tbl (
    tenant_id, id, version, party_id, config_id, folder_id,
    currency_code, index_family, tenor, role, process_type,
    ticks_per_hour, enabled, auto_start, price_source,
    rate_source, derived_spread_basis_points, derived_spread_source,
    description, fixed_leg_payment_frequency_code, source_name,
    asset_class_code, arrival_mode, branching_ratio, decay, sensitivity,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select
    ores_utility_system_tenant_id_fn(),
    gen_random_uuid(),
    0,
    overnight.party_id,
    derived_container.id,
    overnight.folder_id,
    d.currency_code,
    d.index_family,
    d.tenor,
    'projection',
    'VASICEK',
    60,
    true,
    true,
    'fixed',
    d.rate_source,
    d.spread_basis_points,
    d.spread_source,
    d.description,
    'Quarterly',
    'synthetic.derived.' || d.currency_code || '.' || d.index_family || '.' || d.tenor,
    'interest_rates',
    'periodic',
    0.0,
    1.0,
    1.0,
    current_user, current_user,
    'system.initial_load',
    'Initial population of derived term index configs',
    now(),
    ores_utility_infinity_timestamp_fn()
from (values
    ('USD', 'sofr',     '1M', 'compounded_overnight',   0.0,
     null::text,
     'Term SOFR at 1M: the compounded overnight SOFR of its curve over the term, as the bond leg of the ACCOUS Ascot fixes.'),
    ('COP', 'ibr',      '3M', 'compounded_overnight',   0.0,
     null::text,
     'Term IBR at 3M: the compounded overnight IBR of its curve over the term, as the ACCOHK cross-currency legs fix.'),
    ('JPY', 'eytibor',  '3M', 'overnight_plus_spread',  10.0,
     'Market convention: EYTIBOR 3M quoted over TONA, at the spread the yen cross-currency legs of ACCOHK are booked at.',
     'EYTIBOR at 3M: TONA plus its stated spread, as the ACCOHK cross-currency legs fix.'),
    ('KRW', 'generic',  '3M', 'overnight_plus_spread',  15.0,
     'Market convention: the generic 3M interbank rate of the won quoted over KOFR.',
     'The generic KRW term index at 3M: KOFR plus its stated spread, as the ACCOHK cross-currency legs fix.'),
    ('PLN', 'wibor',    '6M', 'overnight_plus_spread',  20.0,
     'Market convention: WIBOR 6M quoted over POLONIA.',
     'WIBOR at 6M: POLONIA plus its stated spread, as the ACCOHK cross-currency legs fix.')
) as d(currency_code, index_family, tenor, rate_source, spread_basis_points, spread_source,
       description)
join lateral (
    select c.id
    from ores_synthetic_market_data_generation_configs_tbl c
    where c.tenant_id = ores_utility_system_tenant_id_fn()
      and c.provider_code = 'derived'
      and c.enabled
      and c.valid_to = ores_utility_infinity_timestamp_fn()
    limit 1
) as derived_container on true
join lateral (
    select c.party_id, c.config_id, c.folder_id
    from ores_synthetic_ir_curve_generation_configs_tbl c
    where c.tenant_id = ores_utility_system_tenant_id_fn()
      and c.currency_code = d.currency_code
      and c.tenor = ''
      and c.role <> 'projection'
      and c.valid_to = ores_utility_infinity_timestamp_fn()
    order by c.valid_from
    limit 1
) as overnight on true
where not exists (
    select 1
    from ores_synthetic_ir_curve_generation_configs_tbl existing
    where existing.tenant_id = ores_utility_system_tenant_id_fn()
      and existing.party_id = overnight.party_id
      and existing.config_id = overnight.config_id
      and existing.currency_code = d.currency_code
      and existing.index_family = d.index_family
      and existing.tenor = d.tenor
      and existing.role = 'projection'
      and existing.valid_to = ores_utility_infinity_timestamp_fn()
);

-- Summary
select 'synthetic_ir_curve_derived_configs' as entity, count(*) as count
from ores_synthetic_ir_curve_generation_configs_tbl
where rate_source <> 'process'
  and valid_to = ores_utility_infinity_timestamp_fn();
