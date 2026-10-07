\pset pager off
select r.name, r.is_requestable
from ores_iam_roles_tbl r
join ores_iam_tenants_tbl t on t.id = r.tenant_id
 and t.code = 'acme_corporation'
 and t.valid_to = ores_utility_infinity_timestamp_fn()
where r.valid_to = ores_utility_infinity_timestamp_fn()
order by r.is_requestable, r.name;

select r.name, r.id
from ores_iam_roles_tbl r
join ores_iam_tenants_tbl t on t.id = r.tenant_id
 and t.code = 'acme_corporation'
 and t.valid_to = ores_utility_infinity_timestamp_fn()
where r.valid_to = ores_utility_infinity_timestamp_fn()
 and r.name in ('IamService', 'Operations', 'Trading')
order by r.name;
