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
 * pgTAP tests for the refdata.conventions dataset.
 *
 * - The dataset is registered with its own methodology.
 * - It is published to the system party and to every party.
 * - Each dataset whose rows name a convention depends on it.
 * - It holds the index conventions ORE defines in code, those its examples
 *   state, and the ones OreStudio adds, with the terms the methodology states.
 * - No index convention is seeded straight into the system tenant: the
 *   dataset is the only route, so the system tenant holds none until its
 *   bundle is published.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/refdata_conventions_dataset_test.sql
 */

begin;

select plan(7);

select results_eq(
    $$select m.name from ores_dq_datasets_tbl d
      join ores_dq_methodologies_tbl m
        on m.id = d.methodology_id
       and m.valid_to = ores_utility_infinity_timestamp_fn()
      where d.code = 'refdata.conventions'
        and d.valid_to = ores_utility_infinity_timestamp_fn()$$,
    $$values ('Convention Curation Methodology')$$,
    'the conventions dataset names its curation methodology'
);

select results_eq(
    $$select bundle_code from ores_dq_dataset_bundle_members_tbl
      where dataset_code = 'refdata.conventions'
        and valid_to = ores_utility_infinity_timestamp_fn()
      order by 1$$,
    $$values ('party_essentials'), ('system_core')$$,
    'the conventions dataset is published to the system party and to every party'
);

select is_empty(
    $$select d.code from ores_dq_datasets_tbl d
      where d.valid_to = ores_utility_infinity_timestamp_fn()
        and (d.code in ('refdata.fx_vol_surface_blueprints', 'ore.report_definitions')
             or d.code ~ '^(acme\.[a-z_]+|northwind)\.(curve_configuration|todays_market|pricing_engines|engine_settings|report_definitions)$')
        and not exists (
            select 1 from ores_dq_dataset_dependencies_tbl x
            where x.dataset_code = d.code
              and x.dependency_code = 'refdata.conventions'
              and x.valid_to = ores_utility_infinity_timestamp_fn())$$,
    'each dataset whose rows name a convention depends on the conventions dataset'
);

select results_eq(
    $$select a.id || ' ' || a.fixing_calendar || ' ' || a.day_count_fraction
      from ores_dq_ibor_index_conventions_artefact_tbl a
      join ores_dq_datasets_tbl d
        on d.id = a.dataset_id
       and d.code = 'refdata.conventions'
       and d.valid_to = ores_utility_infinity_timestamp_fn()
      where a.id in ('EUR-EURIBOR', 'GBP-LIBOR', 'AED-EIBOR-3M')
      order by 1$$,
    $$values ('AED-EIBOR-3M AED ACT/360'),
             ('EUR-EURIBOR TARGET ACT/360'),
             ('GBP-LIBOR London stock exchange ACT/365.FIXED')$$,
    'the dataset holds the Ibor conventions of the ORE engine and of the ORE examples'
);

select results_eq(
    $$select a.id || ' ' || a.day_count_fraction || ' ' || coalesce(a.oresmd_uri, '<none>')
      from ores_dq_overnight_index_conventions_artefact_tbl a
      join ores_dq_datasets_tbl d
        on d.id = a.dataset_id
       and d.code = 'refdata.conventions'
       and d.valid_to = ores_utility_infinity_timestamp_fn()
      where a.id in ('DKK-DESTR', 'ZAR-ZARONIA', 'EUR-ESTR', 'EUR-ESTER', 'USD-SOFR')
      order by 1$$,
    $$values ('DKK-DESTR ACT/360 oresmd://ir/DKK?type=fixing&index=ibor&name=DESTR'),
             ('EUR-ESTER ACT/360 oresmd://ir/EUR?type=fixing&index=ibor&name=ESTER'),
             ('EUR-ESTR ACT/360 oresmd://ir/EUR?type=fixing&index=ibor&name=ESTER'),
             ('USD-SOFR ACT/360 oresmd://ir/USD?type=fixing&index=ibor&name=SOFR'),
             ('ZAR-ZARONIA ACT/365.FIXED oresmd://ir/ZAR?type=fixing&index=ibor&name=ZARONIA')$$,
    'the overnight conventions take the terms the methodology states, and an OreStudio id its own address'
);

select is_empty(
    $$select id from ores_refdata_ibor_index_conventions_tbl
      where tenant_id = ores_utility_system_tenant_id_fn()
        and valid_to = ores_utility_infinity_timestamp_fn()
      union all
      select id from ores_refdata_overnight_index_conventions_tbl
      where tenant_id = ores_utility_system_tenant_id_fn()
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    'no index convention is seeded straight into the system tenant'
);

select is(
    (select count(*)::int from ores_dq_datasets_tbl
     where code = 'ore.conventions' and valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'the ore.conventions dataset is gone'
);

select * from finish();
rollback;
