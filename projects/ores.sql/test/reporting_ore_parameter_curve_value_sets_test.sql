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
 * - Every link a curve leaf has names a value set that exists
 * - Every documented curve leaf has a link
 * - Every vocabulary a curve leaf uses is a curve vocabulary
 * - Every curve set is named by a setting
 * - Every catalogue a curve set draws on is registered
 * - A leaf whose type the schema enumerates takes the values the schema states
 * - The set lists every value the schema enumerates for it
 * - A value the schema does not enumerate is refused
 *
 * Run with: pg_prove -d <database> test/reporting_ore_parameter_curve_value_sets_test.sql
 */

begin;

select plan(10);

create temporary table t_documented on commit drop as
select d.scope, d.name
from ores_dq_ore_parameters_artefact_tbl d
where d.dataset_id = (select id from ores_dq_datasets_tbl
                      where code = 'ore.parameters'
                        and valid_to = ores_utility_infinity_timestamp_fn())
  and d.schema_name = 'curveconfig.xsd';

-- The links a curve leaf holds, whatever set it names. Reading them by the
-- leaf rather than by the set prefix is what lets the fourth test below state
-- the rule instead of assuming it.
create temporary table t_links on commit drop as
select l.code, l.scope, l.name, l.value_set_code
from ores_reporting_ore_parameter_setting_value_sets_tbl l
join t_documented d on l.code = d.scope || ':' || d.name
where l.valid_to = ores_utility_infinity_timestamp_fn()
  and l.tenant_id = ores_utility_system_tenant_id_fn();

-- Every link a curve setting holds, whether or not the contract documents the
-- leaf. A curve leaf the ORE source reads takes a set and the contract does not
-- describe it, so reading only the documented leaves would call a used set an
-- orphan.
create temporary table t_curve_links on commit drop as
select l.code, l.value_set_code
from ores_reporting_ore_parameter_setting_value_sets_tbl l
where l.valid_to = ores_utility_infinity_timestamp_fn()
  and l.tenant_id = ores_utility_system_tenant_id_fn()
  and (starts_with(l.value_set_code, 'curve.')
       or exists (select 1 from t_links d where d.value_set_code = l.value_set_code));

-- The curve sets, whether or not a leaf uses them, plus any set a curve leaf
-- wrongly uses. Reading only the referenced sets would make the orphan test
-- vacuous, and reading only the prefix would hide a shared set.
create temporary table t_sets on commit drop as
select s.code, s.source_kind, s.catalogue_code
from ores_reporting_ore_parameter_value_sets_tbl s
where s.valid_to = ores_utility_infinity_timestamp_fn()
  and s.tenant_id = ores_utility_system_tenant_id_fn()
  and (starts_with(s.code, 'curve.')
       or exists (select 1 from t_links l where l.value_set_code = s.code));

select cmp_ok(
    (select count(*)::int from t_links), '>', 0,
    'the curve leaves have links');

select is_empty(
    $$select l.code, l.value_set_code from t_links l
      where not exists (select 1 from t_sets s where s.code = l.value_set_code)$$,
    'every curve link names a set that exists');

select is_empty(
    $$select d.scope, d.name from t_documented d
      where not exists (select 1 from t_links l
                        where l.code = d.scope || ':' || d.name)$$,
    'every documented curve leaf has a link');

-- A curve leaf may take the one set that states free text, because a value no
-- vocabulary governs is governed by nothing in whichever schema it is written.
-- Every set that does name a vocabulary must be the curve family's.
select is_empty(
    $$select l.code, l.value_set_code
        from t_links l
        join ores_reporting_ore_parameter_value_sets_tbl s
          on s.code = l.value_set_code
         and s.valid_to = ores_utility_infinity_timestamp_fn()
         and s.tenant_id = ores_utility_system_tenant_id_fn()
       where not starts_with(l.value_set_code, 'curve.')
         and s.source_kind <> 'text'$$,
    'every vocabulary a curve leaf uses is a curve vocabulary');

select is_empty(
    $$select s.code from t_sets s
      where not exists (select 1 from t_curve_links l
                        where l.value_set_code = s.code)$$,
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

-- The schema enumerates the values of a simple type, so the rule is a set of
-- them rather than a shape one value must have.
select is(
    (select value_set_code
       from ores_reporting_ore_parameter_vocabularies_tbl
      where xpath = '/CurveConfiguration/Correlations/Correlation/Dimension'
        and valid_to = ores_utility_infinity_timestamp_fn()),
    'curve.dimension_type',
    'a leaf whose type the schema enumerates takes the values the schema states');

select is(
    (select count(*)::int
       from ores_reporting_ore_parameter_value_set_values_tbl
      where value_set_code = 'curve.dimension_type'
        and valid_to = ores_utility_infinity_timestamp_fn()),
    4,
    'the set lists every value the schema enumerates for it');

select is(
    ores_reporting_value_in_set_fn('curve.dimension_type', 'Smile'),
    true,
    'a value the schema enumerates is in the set');

select is(
    ores_reporting_value_in_set_fn('curve.dimension_type', 'Sideways'),
    false,
    'a value the schema does not enumerate is refused');

select * from finish();

rollback;
