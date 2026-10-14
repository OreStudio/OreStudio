/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * pgTAP tests for the kinds of report instance.
 *
 * A report instance is either a production run or a package build:
 * - The lookup holds the two kinds
 * - An instance that names no kind is a production instance
 * - An instance of a kind that is not in the lookup is refused
 * - A package instance is accepted, beside a production instance of the same definition
 *
 * The concurrency check, ores_reporting_in_flight_instance_fn, counts production instances
 * only. It is restricted to the reporting service, which a test user cannot assume, so
 * that rule is proved end to end against the running service and not here.
 *
 * Run with: pg_prove -d <database> test/reporting_report_instance_kinds_test.sql
 */

begin;

select plan(5);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       '00000000-0000-0000-0000-0000000b1001'::uuid as definition_id,
       '00000000-0000-0000-0000-0000000b1002'::uuid as default_kind_id,
       '00000000-0000-0000-0000-0000000b1003'::uuid as package_id,
       '00000000-0000-0000-0000-0000000b1004'::uuid as production_id,
       (select s.id from ores_dq_fsm_states_tbl s
         join ores_dq_fsm_machines_tbl m on m.id = s.machine_id
          and m.valid_to = ores_utility_infinity_timestamp_fn()
        where m.name = 'report_instance_lifecycle' and s.name = 'pending'
          and s.tenant_id = ores_utility_system_tenant_id_fn()
          and s.valid_to = ores_utility_infinity_timestamp_fn()) as pending_id,
       (select s.id from ores_dq_fsm_states_tbl s
         join ores_dq_fsm_machines_tbl m on m.id = s.machine_id
          and m.valid_to = ores_utility_infinity_timestamp_fn()
        where m.name = 'report_instance_lifecycle' and s.name = 'failed'
          and s.tenant_id = ores_utility_system_tenant_id_fn()
          and s.valid_to = ores_utility_infinity_timestamp_fn()) as failed_id;

insert into ores_reporting_report_definitions_tbl (
    id, tenant_id, version, name, party_id, description, report_type,
    schedule_expression, concurrency_policy, pre_processing, post_processing,
    is_official, modified_by, performed_by, change_reason_code, change_commentary
)
select c.definition_id, ores_utility_system_tenant_id_fn(), 0,
    'KINDS-FIXTURE-DEFINITION', c.party_id, 'Instance kinds pgTAP fixture', 'risk',
    '0 0 * * *', 'skip', 'execute', 'execute',
    false, current_user, current_user, 'system.test', 'Instance kinds pgTAP fixture'
from t_ctx c;

create or replace function pg_temp.add_instance(
    p_id uuid, p_name text, p_kind text, p_state uuid) returns void as $$
    insert into ores_reporting_report_instances_tbl (
        id, tenant_id, version, party_id, name, description, definition_id, kind,
        fsm_state_id, trigger_run_id, output_message, output_storage_key,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_id, ores_utility_system_tenant_id_fn(), 0, c.party_id, p_name,
           'Instance kinds pgTAP fixture', c.definition_id, p_kind, p_state, 0, '', '',
           current_user, current_user, 'system.test', 'Instance kinds pgTAP fixture'
    from t_ctx c;
$$ language sql;

select results_eq(
    $$select code from ores_reporting_report_instance_kinds_tbl
      where tenant_id = ores_utility_system_tenant_id_fn()
        and valid_to = ores_utility_infinity_timestamp_fn()
      order by display_order$$,
    $$values ('production'::text), ('package'::text)$$,
    'the lookup holds the production and the package kinds');

-- An instance that names no kind takes the default. It starts in the failed state, which
-- the machine lets an instance start in.
insert into ores_reporting_report_instances_tbl (
    id, tenant_id, version, party_id, name, description, definition_id,
    fsm_state_id, trigger_run_id, output_message, output_storage_key,
    modified_by, performed_by, change_reason_code, change_commentary)
select c.default_kind_id, ores_utility_system_tenant_id_fn(), 0, c.party_id,
       'KINDS-FIXTURE-DEFAULT', 'Instance kinds pgTAP fixture', c.definition_id,
       c.failed_id, 0, '', '',
       current_user, current_user, 'system.test', 'Instance kinds pgTAP fixture'
from t_ctx c;

select is(
    (select kind from ores_reporting_report_instances_tbl
     where id = (select default_kind_id from t_ctx)
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'production',
    'an instance that names no kind is a production instance');

select throws_ok(
    $$select pg_temp.add_instance('00000000-0000-0000-0000-0000000b10ff'::uuid,
        'KINDS-FIXTURE-REHEARSAL', 'rehearsal',
        (select pending_id from t_ctx))$$,
    '23503', null,
    'an instance of a kind that is not in the lookup is refused');

select lives_ok(
    $$select pg_temp.add_instance((select package_id from t_ctx), 'KINDS-FIXTURE-PACKAGE',
        'package', (select pending_id from t_ctx))$$,
    'a package instance is accepted');

select lives_ok(
    $$select pg_temp.add_instance((select production_id from t_ctx), 'KINDS-FIXTURE-PRODUCTION',
        'production', (select pending_id from t_ctx))$$,
    'a production instance is accepted beside the package instance');

select * from finish();
rollback;
