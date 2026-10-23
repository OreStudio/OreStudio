-- Copies the system tenant's ore_parameter rows and their value domains into
-- one tenant, so that tenant's screens can read the parameter documentation.
-- Safe to run twice: a row the tenant already holds is left alone.
--
-- Usage: compass sql -- -v target_code=acme_corporation -f <this file>
--
-- This is a test aid. The permanent fix is a provisioning step that copies the
-- rows, or a read of them under the system tenant.

\if :{?target_code}
\else
\set target_code acme_corporation
\endif

begin;

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp table src_domains on commit drop as
select * from ores_reporting_ore_parameter_value_domains_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

create temp table src_params on commit drop as
select * from ores_reporting_ore_parameters_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

select id as target_id from ores_iam_tenants_tbl
where code = :'target_code' and valid_to = ores_utility_infinity_timestamp_fn() \gset

select set_config('app.current_tenant_id', :'target_id', true);

insert into ores_reporting_ore_parameter_value_domains_tbl
    (code, tenant_id, version, name, storage_kind, referenced_entity,
     modified_by, performed_by, change_reason_code, change_commentary,
     valid_from, valid_to)
select s.code, :'target_id'::uuid, 0, s.name, s.storage_kind, s.referenced_entity,
       s.modified_by, s.performed_by, s.change_reason_code, s.change_commentary,
       s.valid_from, s.valid_to
from src_domains s
where not exists (
    select 1 from ores_reporting_ore_parameter_value_domains_tbl t
    where t.tenant_id = :'target_id'::uuid and t.code = s.code
      and t.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_reporting_ore_parameters_tbl
    (id, tenant_id, version, scope, subtype, name, schema_name, xpath, position,
     ore_parameter_value_domain_code, is_required, label, help, description,
     default_text, format, introduced_in, removed_in, modified_by, performed_by,
     change_reason_code, change_commentary, valid_from, valid_to)
select s.id, :'target_id'::uuid, 0, s.scope, s.subtype, s.name, s.schema_name, s.xpath,
       s.position, s.ore_parameter_value_domain_code, s.is_required, s.label, s.help,
       s.description, s.default_text, s.format, s.introduced_in, s.removed_in,
       s.modified_by, s.performed_by, s.change_reason_code, s.change_commentary,
       s.valid_from, s.valid_to
from src_params s
where not exists (
    select 1 from ores_reporting_ore_parameters_tbl t
    where t.tenant_id = :'target_id'::uuid and t.id = s.id
      and t.valid_to = ores_utility_infinity_timestamp_fn());

select count(*) as parameters_in_tenant from ores_reporting_ore_parameters_tbl
where tenant_id = :'target_id'::uuid and valid_to = ores_utility_infinity_timestamp_fn();

commit;
