\pset pager off

-- Move the newest waiting role request's deadline behind us, which is what
-- waiting the fortnight comes to. The write is the ordinary one: the row gains
-- a version and the close-and-insert rule keeps the old one.
do $$
declare
    v ores_inbox_approval_requests_tbl%rowtype;
begin
    select * into v
    from ores_inbox_approval_requests_tbl
    where kind_code = 'iam.role_grant'
      and state_code = 'waiting'
      and valid_to = ores_utility_infinity_timestamp_fn()
    order by requested_at desc
    limit 1
    for update;

    if not found then
        raise exception 'No waiting role request to backdate.';
    end if;

    v.expires_at := clock_timestamp() - interval '1 minute';
    v.change_reason_code := 'system.test';
    v.change_commentary := 'Backdated so the sweep has something to close';
    insert into ores_inbox_approval_requests_tbl select v.*;

    raise notice 'Backdated request %', v.id;
end $$;

select id, state_code, expires_at, version
from ores_inbox_approval_requests_tbl
where kind_code = 'iam.role_grant'
  and valid_to = ores_utility_infinity_timestamp_fn()
order by requested_at desc
limit 1;
