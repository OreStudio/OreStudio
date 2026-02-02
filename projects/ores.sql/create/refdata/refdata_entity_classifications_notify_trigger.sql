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

create or replace function public.refdata_entity_classifications_notify_fn()
returns trigger as $$
declare
    notification_payload jsonb;
    entity_name text := 'ores.refdata.entity_classification';
    change_timestamp timestamptz := NOW();
    changed_code text;
begin
    if TG_OP = 'DELETE' then
        changed_code := OLD.code;
    else
        changed_code := NEW.code;
    end if;

    notification_payload := jsonb_build_object(
        'entity', entity_name,
        'timestamp', to_char(change_timestamp, 'YYYY-MM-DD HH24:MI:SS'),
        'entity_ids', jsonb_build_array(changed_code)
    );

    perform pg_notify('ores_entity_classifications', notification_payload::text);

    return null;
end;
$$ language plpgsql;

create or replace trigger refdata_entity_classifications_notify_trg
after insert or update or delete on public.refdata_entity_classifications_tbl
for each row execute function public.refdata_entity_classifications_notify_fn();
