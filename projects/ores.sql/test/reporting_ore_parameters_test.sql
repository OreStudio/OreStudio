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
 * pgTAP tests for the keys of the ore parameters.
 *
 * A row documents one leaf of an ORE XML document. The schema name and the
 * XPath say which leaf, and for ore.xsd the scope, subtype and name say the
 * same thing.
 * - Every ore parameter has a schema name and an XPath
 * - A schema name and an XPath name one row only
 * - For ore.xsd the XPath follows from the scope, subtype and name
 *
 * Run with: pg_prove -d <database> test/reporting_ore_parameters_test.sql
 */

begin;

select plan(3);

create temporary table t_parameters on commit drop as
select d.scope, d.subtype, d.name, d.schema_name, d.xpath
from ores_reporting_ore_parameters_tbl d
where d.valid_to = ores_utility_infinity_timestamp_fn();

select is_empty(
    $$select name from t_parameters
      where coalesce(schema_name, '') = '' or coalesce(xpath, '') = ''$$,
    'every ore parameter has a schema name and an XPath');

select is_empty(
    $$select schema_name, xpath from t_parameters
      group by schema_name, xpath having count(*) > 1$$,
    'a schema name and an XPath name one ore parameter only');

select is_empty(
    $$select name from t_parameters
      where schema_name = 'ore.xsd'
        and xpath is distinct from case scope
              when 'Setup' then format('/ORE/Setup/Parameter[@name=''%s'']', name)
              when 'analytic' then format(
                  '/ORE/Analytics/Analytic[@type=''%s'']/Parameter[@name=''%s'']',
                  subtype, name)
            end$$,
    'the XPath of an ore.xsd parameter follows from its scope, subtype and name');

select * from finish();
rollback;
