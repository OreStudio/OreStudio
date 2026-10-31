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
 * Refuses an engine run setting value its value set does not accept, and stores
 * the form the set names for one it accepts.
 *
 * The part tables hold one column per setting, so a rule written as a column
 * check would state the bound or the list a second time, beside the set the
 * parameter path reads. Each part table carries this guard instead: it reads
 * the group's settings, takes each stated column and asks the verdict function
 * the parameter trigger asks. One function, one rule, for a direct write, the
 * save under a sign-off and the ORE import.
 *
 * A column the group does not state is left alone, which keeps the guard from
 * knowing the table it is on. The row is rewritten through its JSON form because
 * a trigger cannot name a column it read from a table.
 */

create or replace function ores_reporting_validate_engine_settings_part_fn()
returns trigger
security definer
set search_path = public, pg_temp
as $$
declare
    v_setting record;
    v_code text;
    v_value text;
    v_found boolean;
    v_in_set boolean;
    v_set_code text;
    v_canonical text;
    v_stored jsonb := '{}'::jsonb;
begin
    for v_setting in
        select s.column_name, s.ore_name
          from ores_reporting_engine_settings_group_settings_tbl s
         where s.group_code = TG_ARGV[0]
           and s.valid_to = ores_utility_infinity_timestamp_fn()
         order by s.position
    loop
        execute format('select ($1).%I::text', v_setting.column_name) into v_value using NEW;
        continue when v_value is null;

        v_code := 'Setup:' || v_setting.ore_name;
        select v.found, v.is_in_set, v.value_set_code
          into v_found, v_in_set, v_set_code
          from ores_reporting_setting_value_verdict_fn(v_code, v_value) v;

        if not coalesce(v_found and v_in_set, false) then
            raise exception 'Invalid value % for %.%, the value is not in its value set.',
                v_value, TG_ARGV[0], v_setting.column_name;
        end if;

        v_canonical := ores_reporting_canonical_value_fn(v_set_code, v_value);
        if v_canonical is distinct from v_value then
            v_stored := v_stored || jsonb_build_object(v_setting.column_name, v_canonical);
        end if;
    end loop;

    if v_stored <> '{}'::jsonb then
        NEW := jsonb_populate_record(NEW, v_stored);
    end if;

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_reporting_engine_settings_fixings_value_guard_trg
before insert or update on "ores_reporting_engine_settings_fixings_tbl"
for each row execute function ores_reporting_validate_engine_settings_part_fn('fixings');

create or replace trigger ores_reporting_engine_settings_building_value_guard_trg
before insert or update on "ores_reporting_engine_settings_building_tbl"
for each row execute function ores_reporting_validate_engine_settings_part_fn('building');

create or replace trigger ores_reporting_engine_settings_execution_value_guard_trg
before insert or update on "ores_reporting_engine_settings_execution_tbl"
for each row execute function ores_reporting_validate_engine_settings_part_fn('execution');
