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
 * The script a scripted trade was booked on
 *
 * A trade that states its script inline records the SHA-256 of the text. A
 * trade that names a library script records the tenant that holds it and its
 * version, so the trade runs that version whatever the library holds later. A
 * row that states a version is checked against the store. A row that states
 * none keeps the version of the trade's latest row when that row names the same
 * script, so an amendment does not move the trade onto a newer script, and
 * otherwise takes the current version: the trade's own tenant's script of that
 * name first, else the system tenant's, which holds ORE's library. A name no
 * library holds is recorded with no version.
 *
 * The trigger fires after the generated insert trigger, which validates the
 * tenant and closes the trade's current row, so the latest row is the one that
 * row closed.
 */
create or replace function ores_trading_scripted_instruments_script_pin_fn()
returns trigger as $$
declare
    v_tenant_id uuid;
    v_version integer;
begin
    if coalesce(NEW.script_body, '') <> '' then
        NEW.script_hash := encode(sha256(convert_to(NEW.script_body, 'UTF8')), 'hex');
        NEW.script_tenant_id := null;
        NEW.script_version := null;
        return NEW;
    end if;
    NEW.script_hash := null;

    if NEW.script_version is not null then
        if NEW.script_tenant_id is null
           or NEW.script_tenant_id not in (NEW.tenant_id, ores_utility_system_tenant_id_fn())
           or not exists (
               select 1 from ores_trading_scripts_tbl s
               where s.tenant_id = NEW.script_tenant_id
                 and s.name = NEW.script_name
                 and s.version = NEW.script_version) then
            raise exception 'Invalid script version: % version % in tenant %. No such script.',
                NEW.script_name, NEW.script_version, NEW.script_tenant_id
                using errcode = '23503';
        end if;
        return NEW;
    end if;

    select i.script_tenant_id, i.script_version into v_tenant_id, v_version
    from ores_trading_scripted_instruments_tbl i
    where i.tenant_id = NEW.tenant_id
      and i.trade_id = NEW.trade_id
    order by i.valid_from desc, i.version desc
    limit 1;
    if found and v_version is not null and exists (
        select 1 from ores_trading_scripts_tbl s
        where s.tenant_id = v_tenant_id
          and s.name = NEW.script_name
          and s.version = v_version) then
        NEW.script_tenant_id := v_tenant_id;
        NEW.script_version := v_version;
        return NEW;
    end if;

    select s.tenant_id, s.version into v_tenant_id, v_version
    from ores_trading_scripts_tbl s
    where s.name = NEW.script_name
      and s.tenant_id in (NEW.tenant_id, ores_utility_system_tenant_id_fn())
      and s.valid_to = ores_utility_infinity_timestamp_fn()
    order by (s.tenant_id = NEW.tenant_id) desc
    limit 1;
    NEW.script_tenant_id := v_tenant_id;
    NEW.script_version := v_version;
    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_trading_scripted_instruments_script_pin_trg
before insert on "ores_trading_scripted_instruments_tbl"
for each row execute function ores_trading_scripted_instruments_script_pin_fn();
