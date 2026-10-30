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
 * Refuses an engine run setting value its value set does not accept.
 *
 * The part tables hold one column per setting, so a rule written as a column
 * check would state the bound or the list a second time, beside the set the
 * parameter path reads. Each part table carries this guard instead: it reads
 * the group's settings, takes each stated column and asks the verdict function
 * the parameter trigger asks. One function, one rule, for a direct write, the
 * save under a sign-off and the ORE import.
 *
 * A column the group does not state is left alone, which keeps the guard from
 * knowing the table it is on.
 */

create or replace function ores_reporting_validate_engine_settings_part_fn()
returns trigger
security definer
set search_path = public, pg_temp
as $$
declare
    v_setting record;
    v_value text;
    v_found boolean;
    v_in_set boolean;
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

        select v.found, v.is_in_set
          into v_found, v_in_set
          from ores_reporting_setting_value_verdict_fn(
                   'Setup:' || v_setting.ore_name, v_value) v;

        if not coalesce(v_found and v_in_set, false) then
            raise exception 'Invalid value % for %.%, the value is not in its value set.',
                v_value, TG_ARGV[0], v_setting.column_name;
        end if;
    end loop;

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
