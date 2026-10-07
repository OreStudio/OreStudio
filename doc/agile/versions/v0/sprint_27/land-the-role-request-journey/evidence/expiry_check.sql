\pset pager off

-- Where the request stands now.
select r.id, r.version, r.state_code, r.expires_at,
       r.change_reason_code, r.change_commentary
from ores_inbox_approval_requests_tbl r
where r.kind_code = 'iam.role_grant'
  and r.valid_to = ores_utility_infinity_timestamp_fn()
order by r.requested_at desc
limit 1;

-- What the person who asked was told, and the values its sentence names.
select n.kind_code, k.message_key, n.raised_at, n.read_at,
       (select string_agg(a.name || '=' || a.value, ', ' order by a.name)
        from ores_inbox_notification_arguments_tbl a
        where a.notification_id = n.id
          and a.valid_to = ores_utility_infinity_timestamp_fn()) as arguments
from ores_inbox_notifications_tbl n
join ores_inbox_notification_kinds_tbl k
  on k.code = n.kind_code
 and k.tenant_id = ores_utility_system_tenant_id_fn()
 and k.valid_to = ores_utility_infinity_timestamp_fn()
where n.kind_code = 'inbox.approval_expired'
  and n.valid_to = ores_utility_infinity_timestamp_fn()
order by n.raised_at desc
limit 3;

-- An expired request is not open, so the administrator's queue no longer
-- offers it.
select count(*) as open_role_requests
from ores_inbox_approval_requests_tbl
where kind_code = 'iam.role_grant'
  and state_code in ('waiting', 'held')
  and valid_to = ores_utility_infinity_timestamp_fn();
