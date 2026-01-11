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
-- Trigger function to send notifications on change reason changes
create or replace function ores.notify_change_reason_changes()
returns trigger as $$
declare
    notification_payload jsonb;
    entity_name text := 'ores.iam.change_reason';
    change_timestamp timestamptz := NOW();
    changed_code text;
begin
    -- Get the code of the changed record
    if TG_OP = 'DELETE' then
        changed_code := OLD.code;
    else
        changed_code := NEW.code;
    end if;

    -- Construct the JSON payload with entity_ids
    notification_payload := jsonb_build_object(
        'entity', entity_name,
        'timestamp', to_char(change_timestamp, 'YYYY-MM-DD"T"HH24:MI:SS.MS"Z"'),
        'entity_ids', jsonb_build_array(changed_code)
    );

    -- Notify on the 'ores_change_reasons' channel
    perform pg_notify('ores_change_reasons', notification_payload::text);

    return null; -- AFTER triggers can return NULL
end;
$$ language plpgsql;

-- Trigger to fire after insert, update, or delete on the change_reasons table
create or replace trigger change_reason_change_notify_trigger
after insert or update or delete on ores.change_reasons
for each row execute function ores.notify_change_reason_changes();
