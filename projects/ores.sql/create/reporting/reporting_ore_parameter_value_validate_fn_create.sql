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
 * Refuses a parameter value outside the set the setting names.
 *
 * One enforcement point, in the database, because every client writes through
 * it. The setting is keyed by (scope, name), which is the parameter row's own
 * scope and name, so no product type is involved and every product type gets
 * the same answer. An enumerated set reads its values, a catalogue set reads
 * the registered view, a range compares, a regex matches, and a boolean is the
 * built-in pair. A setting with no set is free text and passes.
 */

create or replace function ores_reporting_validate_parameter_value_fn()
returns trigger
security definer
set search_path = public, pg_temp
as $$
declare
    v_scope text;
    v_set_code text;
    v_kind text;
    v_relation text;
    v_ok boolean := false;
    v_part text;
begin
    v_scope := case NEW.parameter_scope
        when 'model' then 'ModelParameters'
        when 'engine' then 'EngineParameters'
        when 'global' then 'GlobalParameters'
        else NEW.parameter_scope
    end;

    select l.value_set_code, s.source_kind, c.relation
      into v_set_code, v_kind, v_relation
      from ores_reporting_ore_parameter_setting_value_sets_tbl l
      join ores_reporting_ore_parameter_value_sets_tbl s
        on s.code = l.value_set_code
       and s.valid_to = ores_utility_infinity_timestamp_fn()
      left join ores_reporting_ore_parameter_value_set_catalogues_tbl c
        on c.code = s.catalogue_code
       and c.valid_to = ores_utility_infinity_timestamp_fn()
     where l.code = v_scope || ':' || NEW.parameter_name
       and l.valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        return NEW;
    end if;

    if v_kind = 'enumerated' then
        select exists (
            select 1
              from ores_reporting_ore_parameter_value_set_values_tbl v
             where v.value_set_code = v_set_code
               and v.value = NEW.parameter_value
               and v.is_active
               and v.valid_to = ores_utility_infinity_timestamp_fn())
          into v_ok;
    elsif v_kind = 'boolean' then
        /*
         * A boolean has two spellings in this tree. ORE's XML states it as Y or
         * N and the projection stores what the document said, so a document
         * that reads Y writes Y; the screens state true or false. Both are the
         * same value. The check accepts either and stores it as it arrived,
         * because normalising it would change the document an export writes
         * back and break a roundtrip that is meant to be lossless.
         */
        v_ok := lower(NEW.parameter_value) in ('true', 'false', 'y', 'n');
    elsif v_kind = 'range' then
        begin
            select NEW.parameter_value::numeric >= coalesce(s.minimum, '-Infinity'::numeric)
               and NEW.parameter_value::numeric <= coalesce(s.maximum, 'Infinity'::numeric)
              into v_ok
              from ores_reporting_ore_parameter_value_sets_tbl s
             where s.code = v_set_code
               and s.valid_to = ores_utility_infinity_timestamp_fn();
        exception when others then
            v_ok := false;
        end;
    elsif v_kind = 'regex' then
        select NEW.parameter_value ~ s.pattern
          into v_ok
          from ores_reporting_ore_parameter_value_sets_tbl s
         where s.code = v_set_code
           and s.valid_to = ores_utility_infinity_timestamp_fn();
    elsif v_kind in ('integer', 'number') then
        /*
         * A numeric parameter may state one number or a list of them, such as
         * the discretisation grid a replicating engine reads. Each comma
         * separated part is checked and an empty part is a typo, so a list is
         * as guarded as a single value rather than waved through.
         */
        v_ok := true;
        foreach v_part in array string_to_array(NEW.parameter_value, ',') loop
            if v_part is null or btrim(v_part) = '' then
                v_ok := false;
            elsif v_kind = 'integer' then
                if btrim(v_part) !~ '^-?[0-9]+$' then
                    v_ok := false;
                end if;
            else
                begin
                    perform btrim(v_part)::numeric;
                exception when others then
                    v_ok := false;
                end;
            end if;
        end loop;
    elsif v_kind in ('period', 'text') then
        v_ok := true;
    elsif v_kind = 'catalogue' then
        if v_relation is null or to_regclass(v_relation) is null then
            raise exception 'The value set % names no readable catalogue relation. Rename it or fix the registry.', v_set_code;
        end if;
        execute format(
            'select exists (select 1 from %I where value = $1 and is_active)', v_relation)
          into v_ok using NEW.parameter_value;
    end if;

    if not coalesce(v_ok, false) then
        raise exception 'Invalid value % for %.%: the value is not in set %.',
            NEW.parameter_value, v_scope, NEW.parameter_name, v_set_code;
    end if;
    return NEW;
end;
$$ language plpgsql;
