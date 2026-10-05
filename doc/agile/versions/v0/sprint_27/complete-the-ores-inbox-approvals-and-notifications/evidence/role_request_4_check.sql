\pset pager off
\echo '--- Ashley''s roles: Trading granted by the approver, with the request as its reason'
select r.name, ar.assigned_by, ar.change_reason_code, ar.change_commentary
from ores_iam_account_roles_tbl ar
join ores_iam_roles_tbl r on r.id = ar.role_id and r.tenant_id = ar.tenant_id
 and r.valid_to = ores_utility_infinity_timestamp_fn()
where ar.account_id = '3862114e-b7e4-41d0-8986-c85e91dbca62'
  and ar.valid_to = ores_utility_infinity_timestamp_fn()
order by r.name;

\echo '--- the request detail IAM recorded'
select g.request_id, g.account_id, r.role_id, r.applied_at is not null as applied
from ores_iam_role_grant_requests_tbl g
join ores_iam_role_grant_request_roles_tbl r on r.request_id = g.request_id
 and r.valid_to = ores_utility_infinity_timestamp_fn()
where g.valid_to = ores_utility_infinity_timestamp_fn();

\echo '--- who was told what'
select n.kind_code, a.username as recipient, rc.read_at is null as unread,
       (select string_agg(arg.name || '=' || arg.value, '; ' order by arg.name)
          from ores_inbox_notification_arguments_tbl arg
         where arg.notification_id = n.id
           and arg.valid_to = ores_utility_infinity_timestamp_fn()) as arguments
from ores_inbox_notifications_tbl n
join ores_inbox_notification_recipients_tbl rc on rc.notification_id = n.id
 and rc.valid_to = ores_utility_infinity_timestamp_fn()
join ores_iam_accounts_tbl a on a.id = rc.account_id
 and a.valid_to = ores_utility_infinity_timestamp_fn()
where n.valid_to = ores_utility_infinity_timestamp_fn()
order by n.raised_at, a.username;

\echo '--- nothing left to grant'
select count(*) as unapplied from ores_iam_unapplied_role_grants_fn();
