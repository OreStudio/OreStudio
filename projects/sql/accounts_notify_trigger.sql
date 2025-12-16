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
-- Trigger function to send notifications on account changes
create or replace function oresdb.notify_account_changes()
returns trigger as $$
declare
    notification_payload jsonb;
    entity_name text := 'ores.accounts.account';
    change_timestamp timestamptz := NOW();
begin
    -- Construct the JSON payload
    notification_payload := jsonb_build_object(
        'entity', entity_name,
        'timestamp', to_char(change_timestamp, 'YYYY-MM-DD"T"HH24:MI:SS.MS"Z"')
    );

    -- Notify on the 'ores_accounts' channel
    perform pg_notify('ores_accounts', notification_payload::text);

    return null; -- AFTER triggers can return NULL
end;
$$ language plpgsql;

-- trigger to fire after insert, update, or delete on the accounts table
create or replace trigger account_change_notify_trigger
after insert or update or delete on oresdb.accounts
for each row execute function oresdb.notify_account_changes();
