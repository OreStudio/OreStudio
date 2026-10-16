\set ON_ERROR_STOP on

begin;

select set_config('app.current_service', 'ores_bright_faraday_iam_service', true);
select set_config('app.current_actor', '', true);

-- Two probe tenants: one takes the publisher as HEAD states it, one takes the
-- edited publisher, so both readings come from a single database state.
insert into ores_iam_tenants_tbl (
    id, version, code, name, type, description, hostname, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    ('aaaa0000-0000-4000-8000-000000000001', 0, 'wire_probe_before',
     'Wire Probe Before', 'evaluation', 'scoped probe', 'wire_probe_before', 'active',
     'super_admin', 'super_admin', 'system.test', 'scoped read of default party'),
    ('aaaa0000-0000-4000-8000-000000000002', 0, 'wire_probe_after',
     'Wire Probe After', 'evaluation', 'scoped probe', 'wire_probe_after', 'active',
     'super_admin', 'super_admin', 'system.test', 'scoped read of default party');

insert into ores_refdata_parties_tbl (
    id, tenant_id, version, short_code, full_name, codename, party_category,
    party_type, parent_party_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    ('bbbb0000-0000-4000-8000-000000000001', 'aaaa0000-0000-4000-8000-000000000001',
     0, 'ACMEUK', 'ACME Corporation UK plc', 'wire_probe_uk_0001', 'Internal', 'Corporate',
     null, 'GBLO', 'Active', 'super_admin', 'super_admin', 'system.test', 'scoped probe party'),
    ('bbbb0000-0000-4000-8000-000000000003', 'aaaa0000-0000-4000-8000-000000000002',
     0, 'ACMCOR', 'Acme Corporation Plc', 'wire_probe_grp_0001', 'Internal', 'Corporate',
     null, 'GBLO', 'Active', 'super_admin', 'super_admin', 'system.test', 'scoped probe party'),
    ('bbbb0000-0000-4000-8000-000000000002', 'aaaa0000-0000-4000-8000-000000000002',
     0, 'ACMEUK', 'ACME Corporation UK plc', 'wire_probe_uk_0002', 'Internal', 'Corporate',
     'bbbb0000-0000-4000-8000-000000000003', 'GBLO', 'Active',
     'super_admin', 'super_admin', 'system.test', 'scoped probe party'),
    ('bbbb0000-0000-4000-8000-000000000004', 'aaaa0000-0000-4000-8000-000000000002',
     0, 'ACCOUS', 'ACME Corporation US Inc', 'wire_probe_us_0001', 'Internal', 'Corporate',
     'bbbb0000-0000-4000-8000-000000000003', 'USNY', 'Active',
     'super_admin', 'super_admin', 'system.test', 'scoped probe party'),
    ('bbbb0000-0000-4000-8000-000000000005', 'aaaa0000-0000-4000-8000-000000000002',
     0, 'ACCOHK', 'ACME Corporation HK Ltd', 'wire_probe_hk_0001', 'Internal', 'Corporate',
     'bbbb0000-0000-4000-8000-000000000003', 'HKHK', 'Active',
     'super_admin', 'super_admin', 'system.test', 'scoped probe party');

create temporary table t_ds on commit drop as
select id from ores_dq_datasets_tbl
where code = 'acme.acme_uk.accounts'
  and valid_to = ores_utility_infinity_timestamp_fn();

-- Every ACME staff dataset the recreate staged, with the party its bundle is
-- published against. This is the tenant's whole staff, all four entities.
create temporary table t_staff on commit drop as
select d.code as dataset, d.id as dataset_id, q.party_id, q.party_code
from ores_dq_datasets_tbl d
join (values
    ('acme.acme_group.accounts', 'bbbb0000-0000-4000-8000-000000000003'::uuid, 'ACMCOR'),
    ('acme.acme_uk.accounts',    'bbbb0000-0000-4000-8000-000000000002'::uuid, 'ACCOUK'),
    ('acme.acme_us.accounts',    'bbbb0000-0000-4000-8000-000000000004'::uuid, 'ACCOUS'),
    ('acme.acme_hk.accounts',    'bbbb0000-0000-4000-8000-000000000005'::uuid, 'ACCOHK')
) as q(code, party_id, party_code) on q.code = d.code
where d.valid_to = ores_utility_infinity_timestamp_fn();

\echo ''
\echo '=== BEFORE: the publisher as HEAD states it ==='
select set_config('app.current_tenant_id', 'aaaa0000-0000-4000-8000-000000000001', true);
\i wire_staff_orig_publish_fn.sql

select action, record_count from
    ores_iam_publish_accounts_from_dq_fn(
        (select id from t_ds), 'aaaa0000-0000-4000-8000-000000000001', 'insert',
        '{"party_id": "bbbb0000-0000-4000-8000-000000000001"}'::jsonb);

select count(*) as accounts,
       count(default_party_id) as with_default_party,
       count(distinct default_party_id) as distinct_defaults
from ores_iam_accounts_tbl
where tenant_id = 'aaaa0000-0000-4000-8000-000000000001'
  and valid_to = ores_utility_infinity_timestamp_fn();

\echo ''
\echo '=== AFTER: the edited publisher ==='
select set_config('app.current_tenant_id', 'aaaa0000-0000-4000-8000-000000000002', true);
\i /home/marco/Development/OreStudio/ores_dev_bright_faraday/projects/ores.sql/create/iam/iam_publish_from_dq_create.sql

-- The tenant's RBAC vocabulary, so the publish assigns its own roles rather
-- than silently skipping them.
insert into ores_iam_roles_tbl (
    id, tenant_id, version, name, description,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), 'aaaa0000-0000-4000-8000-000000000002', 0, name, description,
       'super_admin', 'super_admin', 'system.test', 'scoped probe role'
from ores_iam_roles_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();

-- The four entities, each in the order load_staff publishes them.
do $$
declare r record;
begin
    for r in select s.party_code, s.party_id, d.id as dataset_id
             from (values
                 ('ACMCOR', 'bbbb0000-0000-4000-8000-000000000003'::uuid, 'acme.acme_group.accounts'),
                 ('ACCOUK', 'bbbb0000-0000-4000-8000-000000000002'::uuid, 'acme.acme_uk.accounts'),
                 ('ACCOUS', 'bbbb0000-0000-4000-8000-000000000004'::uuid, 'acme.acme_us.accounts'),
                 ('ACCOHK', 'bbbb0000-0000-4000-8000-000000000005'::uuid, 'acme.acme_hk.accounts')
             ) as s(party_code, party_id, code)
             join ores_dq_datasets_tbl d on d.code = s.code
              and d.valid_to = ores_utility_infinity_timestamp_fn()
             order by s.party_code
    loop
        perform ores_iam_publish_accounts_from_dq_fn(
            r.dataset_id, 'aaaa0000-0000-4000-8000-000000000002', 'insert',
            jsonb_build_object('party_id', r.party_id::text));
    end loop;
end $$;

select count(*) as accounts,
       count(default_party_id) as with_default_party,
       count(distinct default_party_id) as distinct_defaults
from ores_iam_accounts_tbl
where tenant_id = 'aaaa0000-0000-4000-8000-000000000002'
  and valid_to = ores_utility_infinity_timestamp_fn();

\echo ''
\echo '=== AFTER: every account, grouped by the party it resolves to ==='
select p.short_code as party, count(*) as accounts
from ores_iam_accounts_tbl a
join ores_refdata_parties_tbl p
  on p.id = a.default_party_id
 and p.tenant_id = a.tenant_id
 and p.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = 'aaaa0000-0000-4000-8000-000000000002'
  and a.valid_to = ores_utility_infinity_timestamp_fn()
group by 1
order by 1;

\echo ''
\echo '=== AFTER: the Trading role holders, by party (the population''s writers) ==='
select p.short_code as party, count(*) as trading_staff
from ores_iam_accounts_tbl a
join ores_refdata_parties_tbl p
  on p.id = a.default_party_id
 and p.tenant_id = a.tenant_id
 and p.valid_to = ores_utility_infinity_timestamp_fn()
where a.tenant_id = 'aaaa0000-0000-4000-8000-000000000002'
  and a.valid_to = ores_utility_infinity_timestamp_fn()
  and exists (
      select 1 from ores_iam_account_roles_tbl ar
      join ores_iam_roles_tbl rl on rl.id = ar.role_id
      where ar.account_id = a.id
        and ar.tenant_id = a.tenant_id
        and ar.valid_to = ores_utility_infinity_timestamp_fn()
        and rl.name = 'Trading')
group by 1
order by 1;

rollback;
