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

-- A link is closed by ending its validity and never deleted, so the close is an
-- update. A close that a newer version of the same link follows is a replacement,
-- which the insert announces; a close with no successor is the link going away.
-- The successor does not exist when the close runs, so the close is judged at
-- the end of the transaction.
create or replace function ores_reporting_report_type_configuration_types_notify_fn()
returns trigger as $$
declare
    notification_payload jsonb;
    change_action text;
    changed_version integer := 0;
    changed_report_type_code text;
    changed_configuration_type_code text;
    changed_tenant_id text;
    -- A party is a key column of the junction when there is one, so it is
    -- already declared and set with the key.
begin
    if TG_OP = 'UPDATE' then
        if OLD.valid_to <> ores_utility_infinity_timestamp_fn()
           or NEW.valid_to = ores_utility_infinity_timestamp_fn() then
            return null;
        end if;
        -- A newer version of the link, open or closed, answers for it: an open
        -- one replaced this version, and a closed one is announced when its
        -- own close is judged, so a link closed twice in one transaction is
        -- told once.
        if exists (
            select 1 from ores_reporting_report_type_configuration_types_tbl
            where tenant_id = OLD.tenant_id
              and report_type_code = OLD.report_type_code
              and configuration_type_code = OLD.configuration_type_code
              and valid_from > OLD.valid_from
        ) then
            return null;
        end if;
        change_action := 'deleted';
        changed_version := OLD.version;
        changed_report_type_code := OLD.report_type_code::text;
        changed_configuration_type_code := OLD.configuration_type_code::text;
        changed_tenant_id := OLD.tenant_id::text;
    else
        if NEW.version <= 1 then
            change_action := 'created';
        else
            change_action := 'updated';
        end if;
        changed_version := NEW.version;
        changed_report_type_code := NEW.report_type_code::text;
        changed_configuration_type_code := NEW.configuration_type_code::text;
        changed_tenant_id := NEW.tenant_id::text;
    end if;

    notification_payload := jsonb_build_object(
        'event_id', gen_random_uuid()::text,
        'entity', 'ores.reporting.report_type_configuration_type',
        'key', jsonb_build_object('report_type_code', changed_report_type_code, 'configuration_type_code', changed_configuration_type_code)::text,
        'action', change_action,
        'version', changed_version,
        'occurred_at', ores_utility_iso8601_timestamp_fn(clock_timestamp()),
        'correlation_id', nullif(current_setting('ores.request.correlation_id', true), ''),
        'tenant_id', changed_tenant_id
    );

    perform pg_notify('ores_reporting_report_type_configuration_types', notification_payload::text);

    return null;
end;
$$ language plpgsql;

create or replace trigger ores_reporting_report_type_configuration_types_notify_trg
after insert on ores_reporting_report_type_configuration_types_tbl
for each row execute function ores_reporting_report_type_configuration_types_notify_fn();

drop trigger if exists ores_reporting_report_type_configuration_types_notify_close_trg on ores_reporting_report_type_configuration_types_tbl;
create constraint trigger ores_reporting_report_type_configuration_types_notify_close_trg
after update on ores_reporting_report_type_configuration_types_tbl
deferrable initially deferred
for each row execute function ores_reporting_report_type_configuration_types_notify_fn();
