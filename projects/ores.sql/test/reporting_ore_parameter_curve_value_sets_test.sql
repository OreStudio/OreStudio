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
 * pgTAP tests for the value sets the curve configuration leaves are linked to.
 *
 * A curve leaf is documented in ore.parameters with schema curveconfig.xsd,
 * and its link names the set its values belong to as the code
 * scope:name. The set is one of the curve sets, drawn from a refdata catalogue,
 * from a list an ORE parser states, or from the leaf's own type.
 * - The curve leaves have links
 * - Every curve link names a value set that exists
 * - Every documented curve leaf has a link
 * - Every curve set has at least one leaf
 * - Every catalogue a curve set draws on is registered
 *
 * Run with: pg_prove -d <database> test/reporting_ore_parameter_curve_value_sets_test.sql
 */

begin;

select plan(5);

create temporary table t_links on commit drop as
select l.code, l.scope, l.name, l.value_set_code
from ores_reporting_ore_parameter_setting_value_sets_tbl l
where l.valid_to = ores_utility_infinity_timestamp_fn()
  and l.tenant_id = ores_utility_system_tenant_id_fn()
  and l.value_set_code like 'curve.%';

create temporary table t_sets on commit drop as
select s.code, s.source_kind, s.catalogue_code
from ores_reporting_ore_parameter_value_sets_tbl s
where s.valid_to = ores_utility_infinity_timestamp_fn()
  and s.tenant_id = ores_utility_system_tenant_id_fn()
  and s.code like 'curve.%';

create temporary table t_documented on commit drop as
select d.scope, d.name
from ores_dq_ore_parameters_artefact_tbl d
where d.dataset_id = (select id from ores_dq_datasets_tbl
                      where code = 'ore.parameters'
                        and valid_to = ores_utility_infinity_timestamp_fn())
  and d.schema_name = 'curveconfig.xsd';

select cmp_ok(
    (select count(*)::int from t_links), '>', 0,
    'the curve leaves have links');

select is_empty(
    $$select code, value_set_code from t_links
      where value_set_code not in (select code from t_sets)$$,
    'every curve link names a set that exists');

select is_empty(
    $$select scope from t_documented
      where scope || ':' || name not in (select code from t_links)$$,
    'every documented curve leaf has a link');

select is_empty(
    $$select code from t_sets
      where code not in (select value_set_code from t_links)$$,
    'every curve set has at least one leaf');

select is_empty(
    $$select s.code, s.catalogue_code from t_sets s
      where s.source_kind = 'catalogue'
        and not exists (
            select 1 from ores_reporting_ore_parameter_value_set_catalogues_tbl c
            where c.code = s.catalogue_code
              and c.valid_to = ores_utility_infinity_timestamp_fn()
              and c.tenant_id = ores_utility_system_tenant_id_fn())$$,
    'every catalogue a curve set draws on is registered');

select * from finish();

rollback;
