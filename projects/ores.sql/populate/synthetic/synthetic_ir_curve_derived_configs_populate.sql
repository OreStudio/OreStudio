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

