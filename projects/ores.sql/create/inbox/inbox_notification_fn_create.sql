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
 * Raising notifications and a person's read state, each in one statement.
 *
 * Raising writes the notification, the values its message names, a recipient
 * row for each person told and that person's in-application delivery. The
 * delivery is the in-application copy: it is delivered once the recipient row
 * exists, and mail, when it comes, adds its own deliveries. The caller resolves
 * the audience; a permission audience is resolved by the service, which holds
 * the permission matching rules.
 *
 * Marking read and clearing write a new version of each recipient row they
 * touch, so the history shows when a person read or cleared a notification.
 *
 * All three run as the caller, so row-level security scopes them to the
 * caller's tenant.
 */

create or replace function ores_inbox_raise_notification_fn(
    p_kind_code text,
    p_link_route text,
    p_link_id text,
    p_audience_permission_code text,
    p_raised_by uuid,
    p_actor text,
    p_argument_names text[],
    p_argument_values text[],
    p_recipient_ids uuid[]
) returns uuid
as $$
declare
    v_id uuid := gen_random_uuid();
    v_tenant_id uuid := ores_iam_current_tenant_id_fn();
begin
    insert into ores_inbox_notifications_tbl (
        id, tenant_id, version, kind_code, raised_by, raised_at, link_route, link_id,
        audience_permission_code, modified_by, performed_by, change_reason_code, change_commentary)
    values (
        v_id, v_tenant_id, 0, p_kind_code, p_raised_by, clock_timestamp(), p_link_route,
        nullif(p_link_id, ''), nullif(p_audience_permission_code, ''),
        p_actor, p_actor, 'system.new_record', '');

    insert into ores_inbox_notification_arguments_tbl (
        tenant_id, notification_id, name, value, version,
        modified_by, performed_by, change_reason_code, change_commentary)
    select v_tenant_id, v_id, a.name, a.value, 0, p_actor, p_actor, 'system.new_record', ''
    from unnest(coalesce(p_argument_names, '{}'), coalesce(p_argument_values, '{}')) as a(name, value);

    insert into ores_inbox_notification_recipients_tbl (
        tenant_id, notification_id, account_id, version,
        modified_by, performed_by, change_reason_code, change_commentary)
    select v_tenant_id, v_id, r.account_id, 0, p_actor, p_actor, 'system.new_record', ''
    from (select distinct unnest(coalesce(p_recipient_ids, '{}')) as account_id) r;

    insert into ores_inbox_notification_deliveries_tbl (
        id, tenant_id, version, notification_id, account_id, channel_code, attempted_at, outcome,
        modified_by, performed_by, change_reason_code, change_commentary)
    select gen_random_uuid(), v_tenant_id, 0, v_id, r.account_id, 'in_app', clock_timestamp(),
        'delivered', p_actor, p_actor, 'system.new_record', ''
    from (select distinct unnest(coalesce(p_recipient_ids, '{}')) as account_id) r;

    return v_id;
end;
$$ language plpgsql;

create or replace function ores_inbox_mark_notifications_read_fn(
    p_account_id uuid,
    p_notification_ids uuid[],
    p_actor text
) returns integer
as $$
declare
    v_count integer;
begin
    insert into ores_inbox_notification_recipients_tbl (
        tenant_id, notification_id, account_id, version, read_at, cleared_at,
        modified_by, performed_by, change_reason_code, change_commentary)
    select r.tenant_id, r.notification_id, r.account_id, r.version, clock_timestamp(), r.cleared_at,
        p_actor, p_actor, 'system.update', 'Read'
    from ores_inbox_notification_recipients_tbl r
    where r.account_id = p_account_id
      and r.valid_to = ores_utility_infinity_timestamp_fn()
      and r.read_at is null
      and r.cleared_at is null
      and (p_notification_ids is null or cardinality(p_notification_ids) = 0
           or r.notification_id = any(p_notification_ids));
    get diagnostics v_count = row_count;
    return v_count;
end;
$$ language plpgsql;

create or replace function ores_inbox_clear_notifications_fn(
    p_account_id uuid,
    p_notification_ids uuid[],
    p_actor text
) returns integer
as $$
declare
    v_count integer;
begin
    insert into ores_inbox_notification_recipients_tbl (
        tenant_id, notification_id, account_id, version, read_at, cleared_at,
        modified_by, performed_by, change_reason_code, change_commentary)
    select r.tenant_id, r.notification_id, r.account_id, r.version,
        coalesce(r.read_at, clock_timestamp()), clock_timestamp(),
        p_actor, p_actor, 'system.update', 'Cleared'
    from ores_inbox_notification_recipients_tbl r
    where r.account_id = p_account_id
      and r.valid_to = ores_utility_infinity_timestamp_fn()
      and r.cleared_at is null
      and ((p_notification_ids is not null and cardinality(p_notification_ids) > 0
            and r.notification_id = any(p_notification_ids))
           or ((p_notification_ids is null or cardinality(p_notification_ids) = 0)
               and r.read_at is not null));
    get diagnostics v_count = row_count;
    return v_count;
end;
$$ language plpgsql;
