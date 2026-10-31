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
 * Every parameter of the contract with the value set it states.
 *
 * One row per (scope, name), whatever product type holds it, because the allowed
 * values belong to the setting and not to one product's copy of it. A reader
 * sees the type, the set, how many values the document writes, the catalogue it
 * draws on and, for an enumerated set, its choices in order. The check that no
 * parameter lacks a set reads this view.
 */

create or replace view ores_reporting_ore_parameter_settings_v as
select p.scope,
       p.name,
       p.ore_parameter_value_domain_code as domain,
       l.value_set_code,
       s.source_kind,
       s.cardinality,
       s.catalogue_code,
       s.minimum,
       s.maximum,
       s.pattern,
       (
           select array_agg(v.value order by v.display_order)
             from ores_reporting_ore_parameter_value_set_values_tbl v
            where v.value_set_code = s.code
              and v.is_active
              and v.valid_to = ores_utility_infinity_timestamp_fn()
       ) as choices
  from (
           select distinct scope, name, ore_parameter_value_domain_code
             from ores_reporting_ore_parameters_tbl
            where valid_to = ores_utility_infinity_timestamp_fn()
              and scope in ('ModelParameters', 'EngineParameters', 'GlobalParameters')
       ) p
  left join ores_reporting_ore_parameter_setting_value_sets_tbl l
    on l.scope = p.scope
   and l.name = p.name
   and l.valid_to = ores_utility_infinity_timestamp_fn()
  left join ores_reporting_ore_parameter_value_sets_tbl s
    on s.code = l.value_set_code
   and s.valid_to = ores_utility_infinity_timestamp_fn();
