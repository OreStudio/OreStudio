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

/*
 * Corrections to scripts of ORE's library that ORE itself cannot parse. The
 * seed states the library as ORE ships it; each correction here writes a new
 * version of one variant. A correction applies when its text occurs once in
 * the current code, does nothing once the code holds its replacement, and
 * fails otherwise, so a library that changes the script fails the load and
 * the correction is reviewed.
 */

\echo '--- Script library corrections ---'

create or replace function pg_temp.correct_script_variant(
    p_script_name text, p_purpose text, p_text text, p_replacement text, p_reason text)
returns void as $$
declare
    current_row ores_trading_script_variants_tbl%rowtype;
    occurrences integer;
begin
    select * into current_row
    from ores_trading_script_variants_tbl
    where tenant_id = ores_utility_system_tenant_id_fn()
      and script_name = p_script_name and purpose = p_purpose
      and valid_to = ores_utility_infinity_timestamp_fn();
    if not found then
        raise exception 'The correction of % (%) finds no script variant.', p_script_name, p_purpose;
    end if;
    occurrences := (length(current_row.code) - length(replace(current_row.code, p_text, '')))
                   / length(p_text);
    if occurrences = 0 and position(p_replacement in current_row.code) > 0 then
        return;
    end if;
    if occurrences != 1 then
        raise exception 'The correction of % (%) finds its text % times; if ORE''s library '
                        'changed the script, review the correction.',
                        p_script_name, p_purpose, occurrences;
    end if;
    insert into ores_trading_script_variants_tbl (
        script_name, tenant_id, version, purpose, code, npv_variable,
        modified_by, change_reason_code, change_commentary)
    values (
        current_row.script_name, current_row.tenant_id, current_row.version,
        current_row.purpose, replace(current_row.code, p_text, p_replacement),
        current_row.npv_variable, current_user, 'system.initial_load', p_reason);
end;
$$ language plpgsql;

-- ORE's parser stops at the statement before END, which has no semicolon.
do $$
begin
    perform pg_temp.correct_script_variant(
        'WorstOfBasketSwap', 'default',
        'couponAccumulation = couponAccumulation + (1-allAssetsTriggered)' || chr(10),
        'couponAccumulation = couponAccumulation + (1-allAssetsTriggered);' || chr(10),
        'Correct a statement ORE''s library states with no semicolon');
end;
$$;
