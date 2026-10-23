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
 * pgTAP tests for the keys of the ore parameters in the staged dataset ore.parameters.
 *
 * A row documents one leaf of an ORE XML document. The schema name and the
 * XPath say which leaf, and for ore.xsd the scope, subtype and name say the
 * same thing.
 * - Every ore parameter has a schema name and an XPath
 * - A schema name and an XPath name one row only
 * - For ore.xsd the XPath follows from the scope, subtype and name
 * - Every ore parameter has the documentation a screen shows
 * - No ore parameter was removed in the release that introduced it
 *
 * Run with: pg_prove -d <database> test/reporting_ore_parameters_test.sql
 */

begin;

select plan(5);

create temporary table t_parameters on commit drop as
select d.scope, d.subtype, d.name, d.schema_name, d.xpath, d.label, d.help,
       d.description, d.default_text, d.format, d.introduced_in, d.removed_in
from ores_dq_ore_parameters_artefact_tbl d
where d.dataset_id = (select id from ores_dq_datasets_tbl
                      where code = 'ore.parameters'
                        and valid_to = ores_utility_infinity_timestamp_fn());

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
              when 'Logging' then format('/ORE/Logging/Parameter[@name=''%s'']', name)
              when 'Markets' then format('/ORE/Markets/Parameter[@name=''%s'']', name)
              when 'analytic' then format(
                  '/ORE/Analytics/Analytic[@type=''%s'']/Parameter[@name=''%s'']',
                  subtype, name)
            end$$,
    'the XPath of an ore.xsd parameter follows from its scope, subtype and name');

select is_empty(
    $$select scope, subtype, name from t_parameters
      where btrim(label) = '' or btrim(help) = '' or btrim(description) = ''
         or btrim(default_text) = '' or btrim(format) = '' or btrim(introduced_in) = ''
         or btrim(coalesce(removed_in, 'x')) = ''$$,
    'every ore parameter has the documentation a screen shows');

select is_empty(
    $$select scope, subtype, name from t_parameters
      where removed_in is not null and removed_in = introduced_in$$,
    'no ore parameter was removed in the release that introduced it');

select * from finish();
rollback;
