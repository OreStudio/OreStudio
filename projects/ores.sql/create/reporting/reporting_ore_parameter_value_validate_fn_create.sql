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

/**
 * Whether a number lies within the bounds its set states.
 *
 * A set states a bound only when it has one, so an absent bound admits every
 * number. A set code the tables do not hold admits every number, the same way
 * ores_reporting_value_in_set_fn admits every value.
 */
create or replace function ores_reporting_number_in_bounds_fn(p_set_code text, p_value numeric)
returns boolean
security definer
set search_path = public, pg_temp
stable
as $$
declare
    v_ok boolean := true;
begin
    select p_value >= coalesce(s.minimum, '-Infinity'::numeric)
       and p_value <= coalesce(s.maximum, 'Infinity'::numeric)
      into v_ok
      from ores_reporting_ore_parameter_value_sets_tbl s
     where s.code = p_set_code
       and s.valid_to = ores_utility_infinity_timestamp_fn();

    return coalesce(v_ok, true);
end;
$$ language plpgsql;

/**
 * Whether a value lies in a value set: the one rule of every check of an ORE
 * leaf's value, so a parameter and a convention term get the same answer from
 * the same set. A set code the tables do not hold admits every value.
 */
create or replace function ores_reporting_value_in_set_item_fn(
    p_set_code text, p_kind text, p_relation text, p_value text)
returns boolean
security definer
set search_path = public, pg_temp
stable
as $$
declare
    v_ok boolean := false;
begin
    if p_kind = 'enumerated' then
        select exists (
            select 1
              from ores_reporting_ore_parameter_value_set_values_tbl v
             where v.value_set_code = p_set_code
               and v.value = p_value
               and v.is_active
               and v.valid_to = ores_utility_infinity_timestamp_fn())
          into v_ok;
    elsif p_kind = 'boolean' then
        /*
         * A boolean has two spellings in this tree. ORE's XML states it as Y or
         * N and the projection stores what the document said, so a document
         * that reads Y writes Y; the screens state true or false. Both are the
         * same value. The check accepts either and stores it as it arrived,
         * because normalising it would change the document an export writes
         * back and break a roundtrip that is meant to be lossless.
         */
        v_ok := lower(p_value) in ('true', 'false', 'y', 'n');
    elsif p_kind = 'range' then
        begin
            v_ok := ores_reporting_number_in_bounds_fn(p_set_code, p_value::numeric);
        exception when others then
            v_ok := false;
        end;
    elsif p_kind = 'regex' then
        select p_value ~ s.pattern
          into v_ok
          from ores_reporting_ore_parameter_value_sets_tbl s
         where s.code = p_set_code
           and s.valid_to = ores_utility_infinity_timestamp_fn();
    elsif p_kind = 'integer' then
        v_ok := p_value ~ '^-?[0-9]+$'
            and ores_reporting_number_in_bounds_fn(p_set_code, p_value::numeric);
    elsif p_kind = 'number' then
        begin
            v_ok := ores_reporting_number_in_bounds_fn(p_set_code, p_value::numeric);
        exception when others then
            v_ok := false;
        end;
    elsif p_kind in ('period', 'text') then
        v_ok := true;
    elsif p_kind = 'catalogue' then
        if p_relation is null or to_regclass(p_relation) is null then
            raise exception 'The value set % names no readable catalogue relation. Rename it or fix the registry.', p_set_code;
        end if;
        execute format(
            'select exists (select 1 from %I where value = $1 and is_active)', p_relation)
          into v_ok using p_value;
    end if;

    return coalesce(v_ok, false);
end;
$$ language plpgsql;

/**
 * Whether a value lies in a value set: the one rule of every check of an ORE
 * leaf's value, so a parameter and a convention term get the same answer from
 * the same set. A set code the tables do not hold admits every value.
 *
 * The set states what one value may be and how many the document writes. A set
 * of cardinality many holds a comma separated list, and every item is held to
 * the set's own rule, so a list is as guarded as a single value and an empty
 * item, which is a trailing or doubled comma, is a typo rather than a value.
 */
create or replace function ores_reporting_value_in_set_fn(p_set_code text, p_value text)
returns boolean
security definer
set search_path = public, pg_temp
stable
as $$
declare
    v_kind text;
    v_cardinality text;
    v_relation text;
    v_ok boolean := true;
    v_part text;
begin
    select s.source_kind, s.cardinality, c.relation
      into v_kind, v_cardinality, v_relation
      from ores_reporting_ore_parameter_value_sets_tbl s
      left join ores_reporting_ore_parameter_value_set_catalogues_tbl c
        on c.code = s.catalogue_code
       and c.valid_to = ores_utility_infinity_timestamp_fn()
     where s.code = p_set_code
       and s.valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        return true;
    end if;

    if v_cardinality <> 'many' then
        return ores_reporting_value_in_set_item_fn(p_set_code, v_kind, v_relation, p_value);
    end if;

    foreach v_part in array string_to_array(p_value, ',') loop
        if v_part is null or btrim(v_part) = '' then
            v_ok := false;
        elsif not ores_reporting_value_in_set_item_fn(
                p_set_code, v_kind, v_relation, btrim(v_part)) then
            v_ok := false;
        end if;
    end loop;

    return v_ok;
end;
$$ language plpgsql;

/**
 * What a setting's value set says about one value: whether the setting names a
 * set at all, which set, what kind it states, and whether the value is in it.
 *
 * The caller states a setting by the code the link table uses, such as
 * Setup:nThreads. The lookup and the verdict are one security definer function
 * for the same reason the trigger is one: the value sets are shared rows, so a
 * tenant-scoped caller that read the tables itself would meet row-level
 * security, see nothing, and read a rule that does not exist. A setting that
 * names no set answers found = false rather than raising, because the caller
 * turns that into a refusal it can state.
 */
create or replace function ores_reporting_setting_value_verdict_fn(p_code text, p_value text)
returns table (found boolean, value_set_code text, source_kind text, is_in_set boolean)
security definer
set search_path = public, pg_temp
stable
as $$
declare
    v_set_code text;
    v_kind text;
begin
    select l.value_set_code, s.source_kind
      into v_set_code, v_kind
      from ores_reporting_ore_parameter_setting_value_sets_tbl l
      join ores_reporting_ore_parameter_value_sets_tbl s
        on s.code = l.value_set_code
       and s.valid_to = ores_utility_infinity_timestamp_fn()
     where l.code = p_code
       and l.valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        return query select false, null::text, null::text, null::boolean;
        return;
    end if;

    return query select true, v_set_code, v_kind,
                        ores_reporting_value_in_set_fn(v_set_code, p_value);
end;
$$ language plpgsql;

create or replace function ores_reporting_validate_parameter_value_fn()
returns trigger
security definer
set search_path = public, pg_temp
as $$
declare
    v_scope text;
    v_set_code text;
begin
    v_scope := case NEW.parameter_scope
        when 'model' then 'ModelParameters'
        when 'engine' then 'EngineParameters'
        when 'global' then 'GlobalParameters'
        else NEW.parameter_scope
    end;

    select l.value_set_code
      into v_set_code
      from ores_reporting_ore_parameter_setting_value_sets_tbl l
      join ores_reporting_ore_parameter_value_sets_tbl s
        on s.code = l.value_set_code
       and s.valid_to = ores_utility_infinity_timestamp_fn()
     where l.code = v_scope || ':' || NEW.parameter_name
       and l.valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        return NEW;
    end if;

    if not ores_reporting_value_in_set_fn(v_set_code, NEW.parameter_value) then
        raise exception 'Invalid value % for %.%: the value is not in set %.',
            NEW.parameter_value, v_scope, NEW.parameter_name, v_set_code;
    end if;
    return NEW;
end;
$$ language plpgsql;
