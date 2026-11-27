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
 * The Term Indices the Run Computes
 *
 * Registers one feeds dataset for each term index the generated trades book
 * floats on and no vendor quotes in the run, and stages the index as a curve
 * config that states its rate from its currency's overnight curve rather than
 * from a process of its own:
 *
 *   USD SOFR 1M      compounded over the overnight curve, under CME Group
 *   COP IBR 3M       compounded over the overnight curve, under Banco de la República
 *   PLN WIBOR 6M     POLONIA plus a stated spread, under GPW Benchmark
 *
 * One dataset for each index, because a published collection stands for one
 * provider and the provider is the administrator's: the index is the
 * administrator's item, and that the run computed the value is the series'
 * derivation_kind, not a provider named after the mechanism. The datasets are
 * named after the administrators, as the provider feeds datasets are, and each
 * is a member of the system_core bundle, so the system provisioning step
 * publishes it for the system party.
 *
 * A compounded index is the overnight rate both curves already agree on, so it
 * states no spread. A spread index states the points and who states them: the
 * fixing of a term index that no one publishes is a market convention, and a
 * spread without its source is a number nobody can check.
 *
 * A computed index reads a curve, it does not run one, so it has no process of
 * its own; the entity still requires the four columns of a Vasicek shape, and
 * the feed never reads them. Theta and the initial rate hold the level of the
 * same currency's overnight curve, so a row a person opens shows plausible
 * numbers; kappa and sigma are the placeholders the shape needs, and no reader
 * of a computed index reads either.
 *
 * The indices need no one to start them: each is offered to the administrator's
 * simulated source, the run's subscription demands the fixing address, and the
 * node runs from the first tick.
 *
 * This script is idempotent: it replaces each dataset's artefact rows.
 */

\echo '--- Computed Term Index Datasets ---'

do $$
declare
    v_index record;
begin
    for v_index in
        select * from (values
            ('synthetic.feeds.cme', 'Synthetic Feeds: CME Group',
             'The Term SOFR the platform computes: the compounded overnight SOFR of its curve over one month, stated under the administrator that owns the index.', 18),
            ('synthetic.feeds.banrep', 'Synthetic Feeds: Banco de la República',
             'The Term IBR the platform computes: the compounded overnight IBR of its curve over three months, stated under the administrator that owns the index.', 19),
            ('synthetic.feeds.gpw', 'Synthetic Feeds: GPW Benchmark',
             'The WIBOR the platform computes: POLONIA plus its stated spread over six months, stated under the administrator that owns the index.', 20)
        ) as t(code, title, description, ordinal)
    loop
        perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
            v_index.code,
            'Synthetic Market Data',
            'Trading',
            'Reference Data',
            'NONE',
            'Primary',
            'Synthetic',
            'Raw',
            'OreStudio Code Generation Methodology',
            v_index.title,
            v_index.description,
            'ORESTUDIO',
            'Computed term indices the system collections hold',
            current_date,
            'Internal Use Only',
            'synthetic_feeds');

        if not exists (
            select 1 from ores_dq_dataset_bundle_members_tbl m
            where m.tenant_id = ores_utility_system_tenant_id_fn()
              and m.bundle_code = 'system_core'
              and m.dataset_code = v_index.code
              and m.valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            perform ores_dq_dataset_bundle_members_upsert_fn(
                ores_utility_system_tenant_id_fn(), 'system_core', v_index.code, v_index.ordinal);
        end if;
    end loop;
end $$;

\echo '--- Computed Term Index Configs ---'

do $$
declare
    v_index record;
    v_dataset_id uuid;
begin
    for v_index in
        select * from (values
            ('synthetic.feeds.cme', 'cme', 'USD', 'sofr', '1M',
             'compounded_overnight', 0.0::double precision, 0.0400::double precision,
             null::text,
             'Term SOFR at 1M: the compounded overnight SOFR of its curve over the term, as the bond leg of the ACCOUS Ascot fixes.'),
            ('synthetic.feeds.banrep', 'banrep', 'COP', 'ibr', '3M',
             'compounded_overnight', 0.0::double precision, 0.0900::double precision,
             null::text,
             'Term IBR at 3M: the compounded overnight IBR of its curve over the term, as the ACCOHK cross-currency legs fix.'),
            ('synthetic.feeds.gpw', 'gpw', 'PLN', 'wibor', '6M',
             'overnight_plus_spread', 20.0::double precision, 0.0550::double precision,
             'Market convention: WIBOR 6M quoted over POLONIA.',
             'WIBOR at 6M: POLONIA plus its stated spread, as the ACCOHK cross-currency legs fix.')
        ) as t(dataset_code, provider_code, currency_code, index_family, tenor,
               rate_source, spread_basis_points, overnight_level, spread_source, description)
    loop
        select id into v_dataset_id
        from ores_dq_datasets_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = v_index.dataset_code
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_dataset_id is null then
            raise exception 'Dataset % not found.', v_index.dataset_code;
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
        ) values (
            v_dataset_id, ores_utility_system_tenant_id_fn(), gen_random_uuid(), 1,
            'Computed Term Index: ' || v_index.currency_code || '-'
                || upper(v_index.index_family) || ' ' || v_index.tenor,
            v_index.description,
            true, true,
            v_index.currency_code, v_index.index_family, v_index.tenor, 'VASICEK',
            0.5, v_index.overnight_level, 0.01, v_index.overnight_level,
            60, 'Quarterly',
            'fixed', v_index.rate_source, v_index.spread_basis_points, v_index.spread_source,
            v_index.provider_code
        );
    end loop;
end $$;

-- Summary
select 'dq_ir_curve_generation_configs_artefact' as entity, count(*) as count
from ores_dq_ir_curve_generation_configs_artefact_tbl a
join ores_dq_datasets_tbl d on d.id = a.dataset_id
where d.code in ('synthetic.feeds.cme', 'synthetic.feeds.banrep', 'synthetic.feeds.gpw')
  and d.valid_to = ores_utility_infinity_timestamp_fn();
