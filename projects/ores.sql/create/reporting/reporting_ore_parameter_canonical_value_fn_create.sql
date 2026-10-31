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
 * The form a configuration holds for one value of one set.
 *
 * ORE reads one fact several ways. NONE, INDIFF, NotApplicable and Unadjusted
 * are one business day convention, and a document may write any of them; the
 * store holds one. A value row names that form in =canonical_value= and is
 * empty when the row is the form, so this function answers with the row's own
 * canonical value, or with the value itself when the set states no other
 * rendering of it.
 *
 * The caller has already decided whether the value is in the set: a rendering
 * outside it is refused there, naming the set. This function states only what
 * is stored, so the two questions stay separate and a set that accepts a value
 * because it states no rule for it stores the value as it arrived.
 *
 * The sets are shared rows, so the answer does not depend on the tenant that
 * asks. A tenant that states its own row for one value is read after the
 * shared one, so the shared statement wins where both exist.
 */
create or replace function ores_reporting_canonical_value_fn(
    p_set_code text,
    p_value text
) returns text
security definer
set search_path = public, pg_temp
stable
as $$
declare
    v_canonical text;
begin
    if p_value is null or p_value = '' then
        return p_value;
    end if;

    select coalesce(v.canonical_value, v.value)
      into v_canonical
      from ores_reporting_ore_parameter_value_set_values_tbl v
     where v.value_set_code = p_set_code
       and v.value = p_value
       and v.valid_to = ores_utility_infinity_timestamp_fn()
     order by (v.tenant_id = ores_utility_system_tenant_id_fn()) desc,
              v.valid_from desc
     limit 1;

    return coalesce(v_canonical, p_value);
end;
$$ language plpgsql;

/**
 * Every value in a guarded column that is not the form its set names.
 *
 * The guards store the canonical form, so this answers nothing on a database the
 * guards wrote. It is a statement about the store rather than about the guard: a
 * row that arrived by a path where the trigger was disabled, a bulk load, or a
 * seed that ran before the guard existed is named here with its table, its column
 * and its set.
 *
 * The columns are the map's children whose set states a rendering, and only the
 * live version of each row is read: a row written before the rule existed is
 * history, and history is what the change window is for.
 */
create or replace function ores_reporting_canonical_value_violations_fn()
returns table (table_name text, column_name text, value_set_code text, value text)
security definer
set search_path = public, pg_temp
stable
as $$
declare
    v_column record;
begin
    for v_column in
        select distinct k.table_name, k.column_name, m.value_set_code
          from ores_reporting_ore_element_map_columns_tbl k
          join ores_reporting_ore_parameter_vocabularies_tbl m
            on m.code = k.element_map_code
           and m.tenant_id = ores_utility_system_tenant_id_fn()
           and m.valid_to = ores_utility_infinity_timestamp_fn()
         where k.tenant_id = ores_utility_system_tenant_id_fn()
           and k.valid_to = ores_utility_infinity_timestamp_fn()
           and exists (
               select 1
                 from ores_reporting_ore_parameter_value_set_values_tbl v
                where v.value_set_code = m.value_set_code
                  and v.canonical_value is not null
                  and v.valid_to = ores_utility_infinity_timestamp_fn())
         order by k.table_name, k.column_name
    loop
        return query execute format(
            'select %L::text, %L::text, %L::text, %I::text from %I '
            'where %I is not null and valid_to = ores_utility_infinity_timestamp_fn() '
            '  and %I <> ores_reporting_canonical_value_fn(%L, %I)',
            v_column.table_name, v_column.column_name, v_column.value_set_code,
            v_column.column_name, v_column.table_name,
            v_column.column_name, v_column.column_name, v_column.value_set_code,
            v_column.column_name);
    end loop;
end;
$$ language plpgsql;
