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
 * pgTAP tests for the Setup parameter definitions of the engine settings.
 *
 * A screen reads the text of a setting from its parameter definition, and the
 * link between the two is the name: the column n_threads of a part is the Setup
 * parameter nThreads.
 * - Every setting column of a part has a Setup definition
 * - Every Setup definition has a setting column
 * - Every definition carries the text a screen shows
 * - No definition has been removed from ORE
 *
 * Run with: pg_prove -d <database> test/reporting_setup_parameter_definitions_test.sql
 */

begin;

select plan(4);

create temporary table t_columns on commit drop as
select c.table_name, c.column_name
from information_schema.columns c
where c.table_name in (
        'ores_reporting_engine_settings_fixings_tbl',
        'ores_reporting_engine_settings_building_tbl',
        'ores_reporting_engine_settings_execution_tbl')
  and c.column_name not in (
        'id', 'tenant_id', 'party_id', 'engine_settings_id', 'version',
        'modified_by', 'performed_by', 'change_reason_code', 'change_commentary',
        'valid_from', 'valid_to');

create temporary table t_definitions on commit drop as
select d.name,
       lower(regexp_replace(d.name, '([A-Z])', '_\1', 'g')) as column_name,
       d.label, d.help, d.description, d.default_text, d.format, d.introduced_in,
       d.removed_in
from ores_reporting_parameter_definitions_tbl d
where d.scope = 'Setup' and d.valid_to = ores_utility_infinity_timestamp_fn();

select is_empty(
    $$select column_name from t_columns
      except select column_name from t_definitions$$,
    'every setting column of a part has a Setup parameter definition');

select is_empty(
    $$select column_name from t_definitions
      except select column_name from t_columns$$,
    'every Setup parameter definition has a setting column');

select is_empty(
    $$select name from t_definitions
      where coalesce(label, '') = '' or coalesce(help, '') = ''
         or coalesce(description, '') = '' or coalesce(default_text, '') = ''
         or coalesce(format, '') = '' or coalesce(introduced_in, '') = ''$$,
    'every Setup parameter definition has the text a screen shows');

select is_empty(
    $$select name from t_definitions where removed_in is not null$$,
    'no Setup parameter of the engine settings has been removed from ORE');

select * from finish();
rollback;
