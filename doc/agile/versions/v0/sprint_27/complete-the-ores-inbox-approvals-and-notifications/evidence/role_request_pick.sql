\pset pager off
select t.id as tenant_id, t.code from ores_iam_tenants_tbl t
where t.code = 'acme_corporation' and t.valid_to = ores_utility_infinity_timestamp_fn();

select a.id, a.username, a.account_type,
       (select string_agg(r.name, ',' order by r.name)
          from ores_iam_account_roles_tbl ar
          join ores_iam_roles_tbl r on r.id = ar.role_id and r.tenant_id = ar.tenant_id
           and r.valid_to = ores_utility_infinity_timestamp_fn()
         where ar.account_id = a.id and ar.valid_to = ores_utility_infinity_timestamp_fn()) as roles,
       (select count(*) from ores_iam_account_parties_tbl ap
         where ap.account_id = a.id and ap.valid_to = ores_utility_infinity_timestamp_fn()) as parties
from ores_iam_accounts_tbl a
join ores_iam_tenants_tbl t on t.id = a.tenant_id and t.code = 'acme_corporation'
 and t.valid_to = ores_utility_infinity_timestamp_fn()
where a.valid_to = ores_utility_infinity_timestamp_fn()
order by a.account_type, a.username
limit 8;

select r.id, r.name from ores_iam_roles_tbl r
join ores_iam_tenants_tbl t on t.id = r.tenant_id and t.code = 'acme_corporation'
 and t.valid_to = ores_utility_infinity_timestamp_fn()
where r.valid_to = ores_utility_infinity_timestamp_fn()
order by r.name;
