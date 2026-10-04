\set ON_ERROR_STOP off
select 'notification kinds' as t, count(*) from ores_inbox_notification_kinds_tbl
union all select 'channels', count(*) from ores_inbox_notification_channels_tbl
union all select 'delivery outcomes', count(*) from ores_inbox_delivery_outcome_types_tbl;

begin;
create temp table probe_accounts as
select id, username from ores_iam_accounts_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
order by username limit 2;

\echo '--- a notification raised to a permission is accepted'
insert into ores_inbox_notifications_tbl (
    id, tenant_id, version, kind_code, raised_by, raised_at, link_route, link_id,
    audience_permission_code, modified_by, performed_by, change_reason_code, change_commentary)
select '22222222-2222-2222-2222-222222222222', ores_utility_system_tenant_id_fn(), 0,
    'inbox.approval_waiting', id, now(), 'requests', '11111111-1111-1111-1111-111111111111',
    'iam::roles:assign', current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username limit 1;

\echo '--- an unknown kind is refused'
savepoint s0;
insert into ores_inbox_notifications_tbl (
    id, tenant_id, version, kind_code, raised_by, raised_at, link_route,
    modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    'no.such_kind', id, now(), 'requests',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username limit 1;
rollback to s0;

\echo '--- an unknown audience permission is refused'
savepoint s1;
insert into ores_inbox_notifications_tbl (
    id, tenant_id, version, kind_code, raised_by, raised_at, link_route,
    audience_permission_code, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    'inbox.approval_waiting', id, now(), 'requests', 'no::such:permission',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username limit 1;
rollback to s1;

\echo '--- an argument and a recipient are accepted'
insert into ores_inbox_notification_arguments_tbl (
    tenant_id, notification_id, name, value, version,
    modified_by, performed_by, change_reason_code, change_commentary)
values (ores_utility_system_tenant_id_fn(), '22222222-2222-2222-2222-222222222222',
    'role', 'Trading', 0, current_user, current_user, 'system.initial_load', 'probe');
insert into ores_inbox_notification_recipients_tbl (
    tenant_id, notification_id, account_id, version,
    modified_by, performed_by, change_reason_code, change_commentary)
select ores_utility_system_tenant_id_fn(), '22222222-2222-2222-2222-222222222222',
    id, 0, current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;

\echo '--- a delivery to an unknown channel is refused'
savepoint s2;
insert into ores_inbox_notification_deliveries_tbl (
    id, tenant_id, version, notification_id, account_id, channel_code,
    attempted_at, outcome, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    '22222222-2222-2222-2222-222222222222', id, 'pigeon', now(), 'delivered',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;
rollback to s2;

\echo '--- an unknown outcome is refused'
savepoint s3;
insert into ores_inbox_notification_deliveries_tbl (
    id, tenant_id, version, notification_id, account_id, channel_code,
    attempted_at, outcome, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    '22222222-2222-2222-2222-222222222222', id, 'in_app', now(), 'lost',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;
rollback to s3;

\echo '--- a failure without a reason is refused'
savepoint s4;
insert into ores_inbox_notification_deliveries_tbl (
    id, tenant_id, version, notification_id, account_id, channel_code,
    attempted_at, outcome, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    '22222222-2222-2222-2222-222222222222', id, 'mail', now(), 'failed',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;
rollback to s4;

\echo '--- a failure with a reason and a delivery are accepted'
insert into ores_inbox_notification_deliveries_tbl (
    id, tenant_id, version, notification_id, account_id, channel_code,
    attempted_at, outcome, failure_reason, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    '22222222-2222-2222-2222-222222222222', id, 'mail', now(), 'failed', 'mailbox full',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;
insert into ores_inbox_notification_deliveries_tbl (
    id, tenant_id, version, notification_id, account_id, channel_code,
    attempted_at, outcome, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    '22222222-2222-2222-2222-222222222222', id, 'in_app', now(), 'delivered',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;

\echo '--- one preference per account, kind and channel'
insert into ores_inbox_notification_preferences_tbl (
    tenant_id, account_id, kind_code, channel_code, version, enabled,
    modified_by, performed_by, change_reason_code, change_commentary)
select ores_utility_system_tenant_id_fn(), id, 'inbox.approval_waiting', 'mail', 0, false,
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;
savepoint s5;
insert into ores_inbox_notification_preferences_tbl (
    tenant_id, account_id, kind_code, channel_code, version, enabled,
    modified_by, performed_by, change_reason_code, change_commentary)
select ores_utility_system_tenant_id_fn(), id, 'inbox.approval_waiting', 'mail', 0, true,
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;
rollback to s5;

select channel_code, outcome, failure_reason from ores_inbox_notification_deliveries_tbl order by channel_code;
rollback;
