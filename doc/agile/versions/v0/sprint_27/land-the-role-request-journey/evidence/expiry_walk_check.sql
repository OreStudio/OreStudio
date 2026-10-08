\pset pager off

-- Read the walk by the request id the ask printed, not by the newest role
-- request: the lifecycle suite raises requests of its own, and the sweep
-- reaches every tenant, so after a suite run the newest role request in the
-- table is the suite's rather than this walk's.

-- Where the request stands now.
select r.id, r.version, r.state_code, r.expires_at,
       r.change_reason_code, r.change_commentary
from ores_inbox_approval_requests_tbl r
where r.id = '01a118da-98a7-7c2a-a803-e00de8c00001'::uuid
  and r.valid_to = ores_utility_infinity_timestamp_fn();

-- What the person who asked was told, and the values its sentence names.
-- A notification is read per recipient, so whether it was read is on the
-- recipient rather than on the notice.
select n.kind_code, k.message_key, n.raised_at,
       (select string_agg(a.name || '=' || a.value, ', ' order by a.name)
        from ores_inbox_notification_arguments_tbl a
        where a.notification_id = n.id
          and a.valid_to = ores_utility_infinity_timestamp_fn()) as arguments
from ores_inbox_notifications_tbl n
join ores_inbox_notification_kinds_tbl k
  on k.code = n.kind_code
 and k.tenant_id = ores_utility_system_tenant_id_fn()
 and k.valid_to = ores_utility_infinity_timestamp_fn()
join ores_inbox_notification_recipients_tbl rec
  on rec.notification_id = n.id
 and rec.valid_to = ores_utility_infinity_timestamp_fn()
where n.kind_code = 'inbox.approval_expired'
  and n.link_id = '01a118da-98a7-7c2a-a803-e00de8c00001'
  and n.valid_to = ores_utility_infinity_timestamp_fn();

-- An expired request is not open, so the administrator's queue no longer
-- offers it.
select count(*) as open_role_requests
from ores_inbox_approval_requests_tbl
where kind_code = 'iam.role_grant'
  and state_code in ('waiting', 'held')
  and valid_to = ores_utility_infinity_timestamp_fn();

-- The firings themselves.
select d.job_name, d.schedule_expression, i.status, i.triggered_at, i.error_message
from ores_scheduler_job_instances_tbl i
join ores_scheduler_job_definitions_tbl d on d.id = i.job_definition_id
where d.job_name = 'ores.inbox.approval_expiry'
order by i.triggered_at desc
limit 5;
