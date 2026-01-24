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
create or replace function ores.dq_data_domains_notify_fn()
returns trigger as $$
declare
    notification_payload jsonb;
    entity_name text := 'ores.dq.data_domain';
    change_timestamp timestamptz := NOW();
    changed_name text;
begin
    if TG_OP = 'DELETE' then
        changed_name := OLD.name;
    else
        changed_name := NEW.name;
    end if;

    notification_payload := jsonb_build_object(
        'entity', entity_name,
        'timestamp', to_char(change_timestamp, 'YYYY-MM-DD HH24:MI:SS'),
        'entity_ids', jsonb_build_array(changed_name)
    );

    perform pg_notify('ores_data_domains', notification_payload::text);

    return null;
end;
$$ language plpgsql;

create or replace trigger dq_data_domains_notify_trg
after insert or update or delete on ores.dq_data_domains_tbl
for each row execute function ores.dq_data_domains_notify_fn();
