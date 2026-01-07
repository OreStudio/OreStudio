/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
set schema 'ores';

-- Trigger function to send notifications on feature flag changes
create or replace function ores.notify_feature_flags_changes()
returns trigger as $$
declare
    notification_payload jsonb;
    entity_name text := 'ores.variability.feature_flag';
    change_timestamp timestamptz := NOW();
    changed_flag_name text;
begin
    -- Get the name of the changed flag
    if TG_OP = 'DELETE' then
        changed_flag_name := OLD.name;
    else
        changed_flag_name := NEW.name;
    end if;

    -- Construct the JSON payload with entity_ids
    notification_payload := jsonb_build_object(
        'entity', entity_name,
        'timestamp', to_char(change_timestamp, 'YYYY-MM-DD"T"HH24:MI:SS.MS"Z"'),
        'entity_ids', jsonb_build_array(changed_flag_name)
    );

    -- Notify on the 'ores_feature_flags' channel
    perform pg_notify('ores_feature_flags', notification_payload::text);

    return null; -- AFTER triggers can return NULL
end;
$$ language plpgsql;

-- Trigger to fire after insert, update, or delete on the feature_flags table
create or replace trigger feature_flags_change_notify_trigger
after insert or update or delete on ores.feature_flags
for each row execute function ores.notify_feature_flags_changes();
