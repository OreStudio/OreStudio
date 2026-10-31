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
 * pgTAP tests for the form a write stores.
 *
 * A value set states which renderings ORE accepts and which one a configuration
 * holds. The guards check a value against the set and then store the form the
 * set names, so a document that says NONE and one that says Unadjusted leave one
 * row. Two tables here: the canonical function on its own, and the two guards
 * that state a setting's value, one of which writes a column it reads from a
 * table rather than a column of its own.
 * - The canonical form of a rendering is the form its set names
 * - A canonical form is its own canonical form
 * - A value the set states nothing about is left as it arrived
 * - A setting whose value is a rendering stores the canonical form
 * - A run setting whose value is a rendering stores the canonical form
 * - A run setting whose value is the canonical form stores it unchanged
 * - No guarded column of the store holds a rendering of the fact it states
 *
 * Run with: pg_prove -d <database> test/reporting_ore_parameter_canonical_store_test.sql
 */

begin;

select plan(7);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    ores_reporting_canonical_value_fn('convention.business_day_convention', 'INDIFF'),
    'Unadjusted',
    'the canonical form of a rendering is the form its set names');

select is(
    ores_reporting_canonical_value_fn('convention.day_counter', 'ACT/360'),
    'ACT/360',
    'a canonical form is its own canonical form');

select is(
    ores_reporting_canonical_value_fn('text', 'anything at all'),
    'anything at all',
    'a value the set states nothing about is left as it arrived');

insert into ores_refdata_parties_tbl (
    id, tenant_id, version, full_name, short_code, party_category, party_type,
    parent_party_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-00000000c0f1'::uuid,
    ores_utility_system_tenant_id_fn(), 0, 'pgTAP canonical store party',
    'PGTAP-CANON', 'Operational', 'Bank', null, 'WRLD', 'Active',
    current_user, current_user, 'system.test', 'pgTAP canonical store fixture'
);

select set_config('app.visible_party_ids', '{a0000000-0000-0000-0000-00000000c0f1}', true);

/*
 * The two sets below hold no rendering in the seed, because no ORE setting they
 * govern is written more than one way. The rule is the set's, not the setting's,
 * so the fixture states a rendering for each and the guard is asked to store it.
 */
insert into ores_reporting_ore_parameter_value_set_values_tbl (
    code, tenant_id, version, value_set_code, value, label, description,
    display_order, is_active, canonical_value,
    modified_by, change_reason_code, change_commentary
) values
    ('bootstrap_mode:NONE', ores_utility_system_tenant_id_fn(), 0, 'bootstrap_mode',
     'NONE', 'None', 'Another rendering of Alternating, which a configuration holds.',
     90, true, 'Alternating',
     current_user, 'system.test', 'pgTAP canonical store fixture'),
    ('setup.observation_model:NONE', ores_utility_system_tenant_id_fn(), 0,
     'setup.observation_model', 'NONE', 'None',
     'Another rendering of None, which a configuration holds.',
     90, true, 'None',
     current_user, 'system.test', 'pgTAP canonical store fixture');

insert into ores_analytics_pricing_model_configs_tbl (
    id, tenant_id, version, party_id, name, description,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-00000000c0f2'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'a0000000-0000-0000-0000-00000000c0f1'::uuid, 'pgTAP canonical store config', null,
    current_user, current_user, 'system.test', 'pgTAP canonical store fixture'
);

insert into ores_analytics_pricing_model_product_parameters_tbl (
    id, tenant_id, version, party_id, pricing_model_config_id,
    parameter_scope, parameter_name, parameter_value, position,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-00000000c0f3'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'a0000000-0000-0000-0000-00000000c0f1'::uuid,
    'a0000000-0000-0000-0000-00000000c0f2'::uuid,
    'engine', 'Bootstrap.Mode', 'NONE', 0,
    current_user, current_user, 'system.test', 'pgTAP canonical store fixture'
);

select is(
    (select parameter_value
       from ores_analytics_pricing_model_product_parameters_tbl
      where id = 'a0000000-0000-0000-0000-00000000c0f3'::uuid),
    'Alternating',
    'a setting whose value is a rendering stores the canonical form');

insert into ores_reporting_configurations_tbl (
    id, tenant_id, version, party_id, name, configuration_type_code,
    owning_component, modified_by, performed_by, change_reason_code,
    change_commentary
) values (
    'a0000000-0000-0000-0000-00000000c0f6'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'a0000000-0000-0000-0000-00000000c0f1'::uuid, 'pgTAP canonical store configuration',
    'engine_settings', 'reporting',
    current_user, current_user, 'system.test', 'pgTAP canonical store fixture'
);

insert into ores_reporting_engine_settings_tbl (
    id, tenant_id, version, party_id, name, description, configuration_id,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-00000000c0f4'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'a0000000-0000-0000-0000-00000000c0f1'::uuid, 'pgTAP canonical store run', null,
    'a0000000-0000-0000-0000-00000000c0f6'::uuid,
    current_user, current_user, 'system.test', 'pgTAP canonical store fixture'
);

insert into ores_reporting_engine_settings_building_tbl (
    id, tenant_id, version, engine_settings_id, party_id, observation_model,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-00000000c0f5'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'a0000000-0000-0000-0000-00000000c0f4'::uuid,
    'a0000000-0000-0000-0000-00000000c0f1'::uuid, 'NONE',
    current_user, current_user, 'system.test', 'pgTAP canonical store fixture'
);

select is(
    (select observation_model
       from ores_reporting_engine_settings_building_tbl
      where id = 'a0000000-0000-0000-0000-00000000c0f5'::uuid),
    'None',
    'a run setting whose value is a rendering stores the canonical form');

update ores_reporting_engine_settings_building_tbl
   set observation_model = 'None'
 where id = 'a0000000-0000-0000-0000-00000000c0f5'::uuid;

select is(
    (select observation_model
       from ores_reporting_engine_settings_building_tbl
      where id = 'a0000000-0000-0000-0000-00000000c0f5'::uuid),
    'None',
    'a run setting whose value is the canonical form stores it unchanged');

select is_empty(
    $$select table_name, column_name, value_set_code, value
        from ores_reporting_canonical_value_violations_fn()$$,
    'no guarded column of the store holds a rendering of the fact it states');

select * from finish();

rollback;
