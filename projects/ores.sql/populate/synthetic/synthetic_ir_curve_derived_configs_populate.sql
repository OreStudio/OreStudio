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
 * Derived Term Index Dataset
 *
 * Registers the derived feeds dataset and stages the term indices the
 * generated trades book that no publisher quotes in the run. Each index is a
 * curve config that states its rate from its currency's overnight curve rather
 * than from a process of its own:
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
 * The rows are staged as artefact rows of the dataset, as every other generated
 * feed is, and the dataset publish writes them into the system tenant's system
 * party under the Derived provider: one collection of its own, holding the five
 * term indices beside the overnight curves they read. The rows carry no process
 * parameters of their own -- a derived index reads a curve, it does not run one
 * -- but the entity requires the four columns of a Vasicek shape, so they hold
 * the levels of the overnight curve of the same currency and are otherwise
 * ignored by the feed.
 *
 * The indices start with the service, as the overnight curves they read do: a
 * derived index that needed a person to start it would answer on no fresh
 * provision.
 *
 * This script is idempotent: it replaces the dataset's artefact rows.
 */

\echo '--- Derived dataset ---'

-- A provider reaches a screen because it is a dataset, as every other generated
-- collection is: the dashboard lists the synthetic datasets and their providers.
do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.feeds.derived',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic Feeds: Derived',
        'The term indices the platform states from its own overnight curves: no publisher quotes them in the run, and the trades that float on them would otherwise fix on nothing.',
        'ORESTUDIO',
        'Derived feeds for the Synthetic data collections bundle',
        current_date,
        'Internal Use Only',
        'synthetic_feeds');
end $$;

do $$
begin
    if not exists (
        select 1 from ores_dq_dataset_bundle_members_tbl m
        where m.tenant_id = ores_utility_system_tenant_id_fn()
          and m.bundle_code = 'system_core'
          and m.dataset_code = 'synthetic.feeds.derived'
          and m.valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        perform ores_dq_dataset_bundle_members_upsert_fn(
            ores_utility_system_tenant_id_fn(), 'system_core', 'synthetic.feeds.derived', 18);
    end if;
end $$;

\echo '--- Derived Term Index Configs ---'

do $$
declare
    v_dataset_id uuid;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = ores_utility_system_tenant_id_fn()
      and code = 'synthetic.feeds.derived'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset synthetic.feeds.derived not found.';
    end if;

    delete from ores_dq_ir_curve_generation_configs_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_ir_curve_generation_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled, auto_start,
        currency_code, index_family, tenor, process_type,
        kappa, theta, sigma, initial_rate,
        ticks_per_hour, fixed_leg_payment_frequency_code,
        price_source, rate_source, derived_spread_basis_points, derived_spread_source,
        provider_code
    )
    select
        v_dataset_id, ores_utility_system_tenant_id_fn(), gen_random_uuid(), 1,
        'Derived Term Index: ' || d.currency_code || '-' || upper(d.index_family)
            || ' ' || d.tenor,
        d.description,
        true, true,
        d.currency_code, d.index_family, d.tenor, 'VASICEK',
        0.5, d.overnight_level, 0.01, d.overnight_level,
        60, 'Quarterly',
        'fixed', d.rate_source, d.spread_basis_points, d.spread_source,
        'derived'
    from (values
        ('USD', 'sofr',     '1M', 'compounded_overnight', 0.0, 0.0400,
         null::text,
         'Term SOFR at 1M: the compounded overnight SOFR of its curve over the term, as the bond leg of the ACCOUS Ascot fixes.'),
        ('COP', 'ibr',      '3M', 'compounded_overnight', 0.0, 0.0900,
         null::text,
         'Term IBR at 3M: the compounded overnight IBR of its curve over the term, as the ACCOHK cross-currency legs fix.'),
        ('JPY', 'eytibor',  '3M', 'overnight_plus_spread', 10.0, 0.0025,
         'Market convention: EYTIBOR 3M quoted over TONA, at the spread the yen cross-currency legs of ACCOHK are booked at.',
         'EYTIBOR at 3M: TONA plus its stated spread, as the ACCOHK cross-currency legs fix.'),
        ('KRW', 'generic',  '3M', 'overnight_plus_spread', 15.0, 0.0280,
         'Market convention: the generic 3M interbank rate of the won quoted over KOFR.',
         'The generic KRW term index at 3M: KOFR plus its stated spread, as the ACCOHK cross-currency legs fix.'),
        ('PLN', 'wibor',    '6M', 'overnight_plus_spread', 20.0, 0.0550,
         'Market convention: WIBOR 6M quoted over POLONIA.',
         'WIBOR at 6M: POLONIA plus its stated spread, as the ACCOHK cross-currency legs fix.')
    ) as d(currency_code, index_family, tenor, rate_source, spread_basis_points,
           overnight_level, spread_source, description);
end $$;

-- Summary
select 'dq_ir_curve_generation_configs_artefact' as entity, count(*) as count
from ores_dq_ir_curve_generation_configs_artefact_tbl a
join ores_dq_datasets_tbl d on d.id = a.dataset_id
where d.code = 'synthetic.feeds.derived'
  and d.valid_to = ores_utility_infinity_timestamp_fn();
