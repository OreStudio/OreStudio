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
 * A curve value is checked against the set of the ORE leaf its column holds, by
 * the trigger the column guard generator writes on every curve family table.
 *
 * The fixture is a yield curve because its columns cover both kinds of leaf: an
 * enumerated one, currency and day counter, and a free text one.
 */

begin;

select plan(8);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- The trigger reads the parameter contract, which the system provisioning step
-- publishes; a fresh database has not run it, so the test publishes it here.
select * from ores_reporting_publish_ore_parameter_value_domains_from_dq_fn(
    (select id from ores_dq_datasets_tbl
      where code = 'ore.parameter_value_domains'
        and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

select * from ores_reporting_publish_ore_parameters_from_dq_fn(
    (select id from ores_dq_datasets_tbl
      where code = 'ore.parameters'
        and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

-- A currency column is checked against the currency catalogue, which the
-- publishing step fills; a fresh database has published nothing, so the test
-- publishes the ISO codes here.
select * from ores_refdata_publish_currencies_from_dq_fn(
    (select id from ores_dq_datasets_tbl
      where code = 'iso.currencies'
        and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

insert into ores_refdata_parties_tbl (
    id, tenant_id, version, full_name, short_code, party_category, party_type,
    parent_party_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-00000000c0e5'::uuid,
    ores_utility_system_tenant_id_fn(), 0, 'pgTAP curve columns party', 'PGTAP-CURVE-COLS',
    'Operational', 'Bank', null, 'WRLD', 'Active',
    current_user, current_user, 'system.test', 'pgTAP curve columns fixture'
);

select set_config('app.visible_party_ids', '{a0000000-0000-0000-0000-00000000c0e5}', true);

insert into ores_refdata_curve_configurations_tbl (
    id, tenant_id, version, party_id, name, description, configuration_id,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-00000000c0e6'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'a0000000-0000-0000-0000-00000000c0e5'::uuid, 'pgTAP curve columns configuration',
    null, null, current_user, current_user, 'system.test', 'pgTAP curve columns fixture'
);

insert into ores_refdata_curve_definitions_tbl (
    id, tenant_id, version, party_id, curve_configuration_id, section_code,
    curve_id, description, position,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-00000000c0e7'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'a0000000-0000-0000-0000-00000000c0e5'::uuid,
    'a0000000-0000-0000-0000-00000000c0e6'::uuid,
    'YieldCurves', 'PGTAP-GBP-CURVE', null, 0,
    current_user, current_user, 'system.test', 'pgTAP curve columns fixture'
);

create or replace function pg_temp.yield_curve(p_currency text, p_day_counter text,
                                               p_report_pillar_dates text)
returns void language sql as $$
    insert into ores_refdata_yield_curve_configs_tbl (
        id, tenant_id, version, party_id, curve_definition_id, currency,
        discount_curve, day_counter, has_report, report_pillar_dates,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
        'a0000000-0000-0000-0000-00000000c0e5'::uuid,
        'a0000000-0000-0000-0000-00000000c0e7'::uuid,
        p_currency, 'PGTAP-GBP-DISCOUNT', p_day_counter, false, p_report_pillar_dates,
        current_user, current_user, 'system.test', 'pgTAP curve columns fixture'
    );
$$;

select lives_ok(
    $$select pg_temp.yield_curve('GBP', 'ACT/365', '2030-01-01')$$,
    'a curve whose values are in their sets is written');

select throws_like(
    $$select pg_temp.yield_curve('XYZ', 'ACT/365', null)$$,
    '%Invalid value XYZ for yield_curve_config.currency: the value is not in set curve.currency.%',
    'a currency outside the catalogue is refused, naming the column and its set');

select throws_like(
    $$select pg_temp.yield_curve('GBP', 'ACT/999', null)$$,
    '%Invalid value ACT/999 for yield_curve_config.day_counter%',
    'a day counter outside the catalogue is refused');

select lives_ok(
    $$select pg_temp.yield_curve('GBP', 'ACT/365', 'not a date, and the map does not name it')$$,
    'a column the map does not name stays free text');

select throws_like(
    $$update ores_refdata_yield_curve_configs_tbl
         set currency = 'XYZ'
       where currency = 'GBP'$$,
    '%Invalid value XYZ for yield_curve_config.currency%',
    'an update of a curve column is checked as an insert is');

select ok(
    not exists (
        select 1
          from (select distinct table_name from ores_refdata_curve_configuration_columns_tbl
                 where valid_to = ores_utility_infinity_timestamp_fn()) m
         where not exists (
                select 1
                  from pg_trigger t
                  join pg_proc p on p.oid = t.tgfoid
                 where t.tgrelid = m.table_name::regclass
                   and p.proname = 'ores_refdata_validate_curve_values_fn')
    ),
    'every table the map names has its guard trigger');

select ok(
    not exists (
        select 1
          from pg_trigger t
          join pg_proc p on p.oid = t.tgfoid
          join pg_class c on c.oid = t.tgrelid
         where p.proname = 'ores_refdata_validate_curve_values_fn'
           and not t.tgisinternal
           and not exists (
                select 1 from ores_refdata_curve_configuration_columns_tbl k
                 where k.table_name = c.relname
                   and k.valid_to = ores_utility_infinity_timestamp_fn())
    ),
    'no curve table is guarded without a row in the map');

select is(
    (select l.value_set_code
       from ores_refdata_curve_configuration_columns_tbl k
       join ores_reporting_ore_parameters_tbl p
         on p.schema_name = k.schema_name
        and p.xpath = k.xpath
        and p.tenant_id = ores_utility_system_tenant_id_fn()
        and p.valid_to = ores_utility_infinity_timestamp_fn()
       join ores_reporting_ore_parameter_setting_value_sets_tbl l
         on l.code = p.scope || ':' || p.name
        and l.valid_to = ores_utility_infinity_timestamp_fn()
      where k.code = 'yield_curve_config|currency'
        and k.tenant_id = ores_utility_system_tenant_id_fn()),
    'curve.currency',
    'a mapped column resolves to the set its leaf links to');

select * from finish();

rollback;
