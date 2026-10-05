\set ON_ERROR_STOP off
select 'states' as t, count(*) from ores_inbox_approval_request_states_tbl
union all select 'decision types', count(*) from ores_inbox_approval_decision_types_tbl
union all select 'kinds', count(*) from ores_inbox_approval_kinds_tbl
union all select 'inbox permissions', count(*) from ores_iam_permissions_tbl where code like 'inbox::%' and valid_to = ores_utility_infinity_timestamp_fn();

begin;
create temp table probe_accounts as
select id, username from ores_iam_accounts_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
order by username limit 3;
select * from probe_accounts;

insert into ores_inbox_approval_requests_tbl (
    id, tenant_id, version, kind_code, state_code, requested_by, requested_at,
    reason, expires_at, modified_by, performed_by, change_reason_code, change_commentary)
select '11111111-1111-1111-1111-111111111111', ores_utility_system_tenant_id_fn(), 0,
    'iam.role_grant', 'waiting', id, now(), 'probe', null,
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username limit 1;

\echo '--- a bad kind is refused'
savepoint s0;
insert into ores_inbox_approval_requests_tbl (
    id, tenant_id, version, kind_code, state_code, requested_by, requested_at,
    reason, modified_by, performed_by, change_reason_code, change_commentary)
select '11111111-1111-1111-1111-111111111112', ores_utility_system_tenant_id_fn(), 0,
    'no.such_kind', 'waiting', id, now(), 'probe',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username limit 1;
rollback to s0;

\echo '--- the asker deciding is refused'
savepoint s1;
insert into ores_inbox_approval_decisions_tbl (
    id, tenant_id, version, request_id, decision_code, decided_by, decided_at,
    comment, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    '11111111-1111-1111-1111-111111111111', 'approve', id, now(), '',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username limit 1;
rollback to s1;

\echo '--- someone else withdrawing is refused'
savepoint s2;
insert into ores_inbox_approval_decisions_tbl (
    id, tenant_id, version, request_id, decision_code, decided_by, decided_at,
    comment, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    '11111111-1111-1111-1111-111111111111', 'withdraw', id, now(), '',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;
rollback to s2;

\echo '--- another person approving is accepted'
insert into ores_inbox_approval_decisions_tbl (
    id, tenant_id, version, request_id, decision_code, decided_by, decided_at,
    comment, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    '11111111-1111-1111-1111-111111111111', 'approve', id, now(), '',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;

\echo '--- the same person approving twice is refused'
savepoint s3;
insert into ores_inbox_approval_decisions_tbl (
    id, tenant_id, version, request_id, decision_code, decided_by, decided_at,
    comment, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    '11111111-1111-1111-1111-111111111111', 'approve', id, now(), '',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;
rollback to s3;

\echo '--- a decision on a request that does not exist is refused'
savepoint s4;
insert into ores_inbox_approval_decisions_tbl (
    id, tenant_id, version, request_id, decision_code, decided_by, decided_at,
    comment, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    '99999999-9999-9999-9999-999999999999', 'approve', id, now(), '',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username offset 1 limit 1;
rollback to s4;

\echo '--- an update that changes the decider is refused'
savepoint s5;
update ores_inbox_approval_decisions_tbl
set decided_by = (select id from probe_accounts order by username limit 1)
where request_id = '11111111-1111-1111-1111-111111111111'
  and decision_code = 'approve';
rollback to s5;

\echo '--- the asker withdrawing is accepted'
insert into ores_inbox_approval_decisions_tbl (
    id, tenant_id, version, request_id, decision_code, decided_by, decided_at,
    comment, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
    '11111111-1111-1111-1111-111111111111', 'withdraw', id, now(), '',
    current_user, current_user, 'system.initial_load', 'probe'
from probe_accounts order by username limit 1;

select decision_code, decided_by from ores_inbox_approval_decisions_tbl;
rollback;
