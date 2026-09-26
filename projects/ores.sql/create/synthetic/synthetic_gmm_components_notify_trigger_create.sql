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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_notify_trigger.mustache
 * To modify, update the template and regenerate.
 */

create or replace function ores_synthetic_gmm_components_notify_fn()
returns trigger as $$
declare
    notification_payload jsonb;
    change_action text;
    changed_version integer := 0;
    changed_id uuid;
    changed_key jsonb;
    changed_tenant_id text;
begin
    if TG_OP = 'DELETE' then
        change_action := 'deleted';
        changed_id := OLD.id;
        changed_version := OLD.version;
        changed_tenant_id := OLD.tenant_id::text;
    elsif TG_OP = 'UPDATE' then
        -- A versioned table's update is the internal close of the current
        -- row; the insert that follows it carries the change. Announcing
        -- both would report one change twice, so the close announces
        -- nothing.
        return null;
    else
        -- The first version of a row is a create; every later one is an
        -- update, because the row it replaces was already there.
        if NEW.version <= 1 then
            change_action := 'created';
        else
            change_action := 'updated';
        end if;
        changed_version := NEW.version;
        changed_id := NEW.id;
        changed_tenant_id := NEW.tenant_id::text;
    end if;

    changed_key := jsonb_build_object('id', changed_id);

    notification_payload := jsonb_build_object(
        'event_id', gen_random_uuid()::text,
        'entity', 'ores.synthetic.gmm_component',
        'key', changed_key::text,
        'action', change_action,
        'version', changed_version,
        'occurred_at', ores_utility_iso8601_timestamp_fn(clock_timestamp()),
        'correlation_id', nullif(current_setting('ores.request.correlation_id', true), ''),
        'tenant_id', changed_tenant_id
    );

    perform pg_notify('ores_synthetic_gmm_components', notification_payload::text);

    return null;
end;
$$ language plpgsql;

create or replace trigger ores_synthetic_gmm_components_notify_trg
after insert or update or delete on ores_synthetic_gmm_components_tbl
for each row execute function ores_synthetic_gmm_components_notify_fn();
