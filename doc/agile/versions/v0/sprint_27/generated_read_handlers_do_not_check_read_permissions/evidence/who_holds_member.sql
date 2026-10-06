\pset pager off
\echo '--- every person account in Acme holds Member; no service account does'
select a.account_type,
       count(*) as accounts,
       count(*) filter (where exists (
           select 1 from ores_iam_account_roles_tbl ar
           join ores_iam_roles_tbl r on r.id = ar.role_id and r.tenant_id = ar.tenant_id
            and r.valid_to = ores_utility_infinity_timestamp_fn()
           where ar.account_id = a.id and r.name = 'Member'
             and ar.valid_to = ores_utility_infinity_timestamp_fn())) as holding_member
from ores_iam_accounts_tbl a
join ores_iam_tenants_tbl t on t.id = a.tenant_id and t.code = 'acme_corporation'
 and t.valid_to = ores_utility_infinity_timestamp_fn()
where a.valid_to = ores_utility_infinity_timestamp_fn()
group by a.account_type
order by a.account_type;

\echo '--- the two people the checks sign in as'
select a.id, a.username,
       string_agg(r.name, ', ' order by r.name) as roles
from ores_iam_accounts_tbl a
join ores_iam_tenants_tbl t on t.id = a.tenant_id and t.code = 'acme_corporation'
 and t.valid_to = ores_utility_infinity_timestamp_fn()
join ores_iam_account_roles_tbl ar on ar.account_id = a.id
 and ar.valid_to = ores_utility_infinity_timestamp_fn()
join ores_iam_roles_tbl r on r.id = ar.role_id and r.tenant_id = ar.tenant_id
 and r.valid_to = ores_utility_infinity_timestamp_fn()
where a.username in ('ashley.moore', 'chi.wing.cheung')
  and a.valid_to = ores_utility_infinity_timestamp_fn()
group by a.id, a.username
order by a.username;

\echo '--- Member in Acme, and how many codes it grants'
select r.name, count(rp.permission_id) as codes
from ores_iam_roles_tbl r
join ores_iam_tenants_tbl t on t.id = r.tenant_id and t.code = 'acme_corporation'
 and t.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_iam_role_permissions_tbl rp on rp.role_id = r.id and rp.tenant_id = r.tenant_id
 and rp.valid_to = ores_utility_infinity_timestamp_fn()
where r.name = 'Member' and r.valid_to = ores_utility_infinity_timestamp_fn()
group by r.name;
