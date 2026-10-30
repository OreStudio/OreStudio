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
 * Session Settings Validation
 *
 * A session's settings are a JSON object whose keys and types are the adapter's
 * configuration schema. The trigger refuses a key the schema does not state, a
 * missing required key, and a value of the wrong type, so a session cannot be
 * saved in a shape its adapter cannot read.
 *
 * The schema is the catalogue's, in the system tenant, which every tenant
 * reads. The trigger is security definer so that read does not depend on the
 * caller's tenant context.
 */

create or replace function ores_marketdata_validate_session_settings_fn()
returns trigger as $$
declare
    v_schema text;
    v_schema_json jsonb;
    v_settings jsonb;
    v_key text;
    v_type text;
    v_required boolean;
    v_value jsonb;
begin
    if NEW.settings is null or btrim(NEW.settings) = '' then
        NEW.settings := '{}';
    end if;

    begin
        v_settings := NEW.settings::jsonb;
    exception when others then
        raise exception 'Invalid session settings: not valid JSON.'
            using errcode = '23514';
    end;

    if jsonb_typeof(v_settings) <> 'object' then
        raise exception 'Invalid session settings: the settings must be a JSON object.'
            using errcode = '23514';
    end if;

    select configuration_schema into v_schema
    from ores_marketdata_adapters_tbl
    where tenant_id = ores_utility_system_tenant_id_fn()
      and code = NEW.adapter_code
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_schema is null or btrim(v_schema) = '' then
        v_schema := '{}';
    end if;

    begin
        v_schema_json := v_schema::jsonb;
    exception when others then
        raise exception 'Invalid adapter schema for %: not valid JSON.', NEW.adapter_code
            using errcode = '23514';
    end;

    for v_key in select jsonb_object_keys(v_settings)
    loop
        if not (v_schema_json ? v_key) then
            raise exception 'Invalid session settings: "%" is not a setting of adapter %.',
                v_key, NEW.adapter_code using errcode = '23514';
        end if;
    end loop;

    for v_key, v_type in
        select key, value ->> 'type' from jsonb_each(v_schema_json)
    loop
        v_required := coalesce((v_schema_json -> v_key ->> 'required')::boolean, false);

        if v_required and not (v_settings ? v_key) then
            raise exception 'Invalid session settings: "%" is required by adapter %.',
                v_key, NEW.adapter_code using errcode = '23514';
        end if;

        if v_settings ? v_key then
            v_value := v_settings -> v_key;

            if v_type = 'int' then
                if jsonb_typeof(v_value) <> 'number'
                   or (v_value #>> '{}')::numeric <> trunc((v_value #>> '{}')::numeric) then
                    raise exception 'Invalid session settings: "%" must be an integer.',
                        v_key using errcode = '23514';
                end if;
            elsif v_type = 'bool' then
                if jsonb_typeof(v_value) <> 'boolean' then
                    raise exception 'Invalid session settings: "%" must be a boolean.',
                        v_key using errcode = '23514';
                end if;
            elsif v_type = 'url' then
                if jsonb_typeof(v_value) <> 'string'
                   or (v_value #>> '{}') !~ '^[a-z][a-z0-9+.-]*://' then
                    raise exception 'Invalid session settings: "%" must be a URL.', v_key
                        using errcode = '23514';
                end if;
            elsif v_type = 'secret_ref' then
                if jsonb_typeof(v_value) <> 'string'
                   or btrim(v_value #>> '{}') = '' then
                    raise exception 'Invalid session settings: "%" must name a credential.',
                        v_key using errcode = '23514';
                end if;
            elsif jsonb_typeof(v_value) <> 'string' then
                raise exception 'Invalid session settings: "%" must be a string.', v_key
                    using errcode = '23514';
            end if;
        end if;
    end loop;

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_sessions_settings_validate_trg
before insert or update of settings, adapter_code on ores_marketdata_sessions_tbl
for each row execute function ores_marketdata_validate_session_settings_fn();
