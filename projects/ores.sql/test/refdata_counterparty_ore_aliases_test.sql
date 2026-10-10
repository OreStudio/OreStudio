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
 * pgTAP tests for the ORE counterparty aliases and the ORE sample banks.
 *
 * Tests cover:
 * - Every counterparty name the ORE samples use resolves to one counterparty
 * - A counterparty may answer to several ORE names
 * - An ORE name answers to one counterparty per tenant
 * - The LEI counterparty publish adds what a tenant lacks and skips the rest
 * - A published counterparty links to a parent the tenant already holds
 * - The alias publish writes each alias once and skips a LEI the tenant lacks
 * - An alias the tenant already resolves to another counterparty is refused
 *
 * Run with: pg_prove -d <database> test/refdata_counterparty_ore_aliases_test.sql
 */

begin;

select plan(13);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- The cases below read the ORE sample counterparties, which a publish writes.
-- A recreated database carries the datasets and nothing published from them,
-- and the sample names business centres beyond the seeded WRLD, so the
-- fixture publishes the canonical business centres and then runs the first
-- counterparty publish; a case that publishes then sees its own run as the
-- second. The suite rolls it back.
select count(*) from ores_refdata_publish_business_centres_from_dq_fn(
    (select id from ores_dq_datasets_tbl where code = 'fpml.business_center'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_utility_system_tenant_id_fn());

select count(*) from ores_refdata_publish_lei_counterparties_from_dq_fn(
    (select id from ores_dq_datasets_tbl where code = 'ore.sample_counterparties'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_utility_system_tenant_id_fn());

select count(*) from ores_refdata_publish_counterparty_aliases_from_dq_fn(
    (select id from ores_dq_datasets_tbl where code = 'ore.counterparty_aliases'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_utility_system_tenant_id_fn());

create or replace function pg_temp.alias_target(p_alias text)
returns text as $$
    select ci.counterparty_id::text
    from ores_refdata_counterparty_identifiers_tbl ci
    where ci.tenant_id = ores_utility_system_tenant_id_fn()
      and ci.id_scheme = 'ORE'
      and ci.id_value = p_alias
      and ci.valid_to = ores_utility_infinity_timestamp_fn();
$$ language sql;

create or replace function pg_temp.add_alias(p_counterparty uuid, p_alias text)
returns void as $$
    insert into ores_refdata_counterparty_identifiers_tbl (tenant_id, id, version,
        counterparty_id, id_scheme, id_value, modified_by, performed_by,
        change_reason_code, change_commentary)
    values (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0, p_counterparty,
        'ORE', p_alias, current_user, current_user, 'system.new_record', 'test');
$$ language sql;

select is(
    (select count(*)::int from (values ('CPTY_A'), ('CPTY_B'), ('CPTY'), ('CPTY_C'), ('CP'),
        ('DUMMY_CP'), ('EquityOption1'), ('EquityOption2'), ('CPTY_D'), ('ABC'), ('A'),
        ('001B456BCDEFGH67XY89'), ('DUMMY_CPTY'), ('CPTY_1'), ('CPTY_2'), ('CPTY_3'),
        ('CPTY_4'), ('CPTY_5'), ('CPTY_6'), ('CPTY_7'), ('CPTY_8'), ('CPTY_9'), ('CPTY_10'))
        as n(alias)
     where pg_temp.alias_target(n.alias) is not null),
    23,
    'every counterparty name the ORE samples use resolves');

select is(pg_temp.alias_target('CPTY_A'), pg_temp.alias_target('A'),
    'a counterparty may answer to several ORE names');

select is(
    (select count(distinct pg_temp.alias_target(n.alias))::int
     from (values ('CPTY_1'), ('CPTY_2'), ('CPTY_3'), ('CPTY_4'), ('CPTY_5'), ('CPTY_6'),
                  ('CPTY_7'), ('CPTY_8'), ('CPTY_9'), ('CPTY_10')) as n(alias)),
    10,
    'the ten names one example uses together map onto ten counterparties');

select throws_ok(
    $$select pg_temp.add_alias(pg_temp.alias_target('CPTY_B')::uuid, 'CPTY_A')$$,
    '23505', null,
    'an ORE name answers to one counterparty per tenant');

select lives_ok(
    $$select pg_temp.add_alias(pg_temp.alias_target('CPTY_B')::uuid, 'CPTY_B_EXTRA')$$,
    'a counterparty takes another ORE name');

select results_eq(
    $$select action from ores_refdata_publish_lei_counterparties_from_dq_fn(
        (select id from ores_dq_datasets_tbl where code = 'ore.sample_counterparties'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        ores_utility_system_tenant_id_fn())$$,
    array['skipped'],
    'a second publish of the same banks adds nothing');

-- A dataset holding a bank the tenant already has and one of its
-- subsidiaries: the publish adds only the subsidiary, under the bank.
do $$
declare
    v_dataset_id uuid;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.ore_partial_publish', 'ORE', 'Parties', 'Reference Data', 'LEI',
        'Primary', 'Actual', 'Raw', 'GLEIF Golden Copy Extraction',
        'Partial publish test', 'Test dataset.', 'GLEIF', 'Test', current_date,
        'Open Data', 'lei_entities');
    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'test.ore_partial_publish'
      and valid_to = ores_utility_infinity_timestamp_fn();
    insert into ores_dq_lei_entities_artefact_tbl (dataset_id, tenant_id, lei, version, entity_legal_name, entity_entity_category, entity_entity_sub_category, entity_entity_status, entity_legal_form_entity_legal_form_code, entity_legal_form_other_legal_form, entity_legal_jurisdiction, entity_legal_address_first_address_line, entity_legal_address_city, entity_legal_address_region, entity_legal_address_country, entity_legal_address_postal_code, entity_headquarters_address_first_address_line, entity_headquarters_address_city, entity_headquarters_address_region, entity_headquarters_address_country, entity_headquarters_address_postal_code, entity_entity_creation_date, registration_initial_registration_date, registration_last_update_date, registration_next_renewal_date, registration_registration_status, entity_transliterated_name_1, entity_transliterated_name_1_type)
    select v_dataset_id, e.tenant_id, e.lei, e.version, e.entity_legal_name, e.entity_entity_category, e.entity_entity_sub_category, e.entity_entity_status, e.entity_legal_form_entity_legal_form_code, e.entity_legal_form_other_legal_form, e.entity_legal_jurisdiction, e.entity_legal_address_first_address_line, e.entity_legal_address_city, e.entity_legal_address_region, e.entity_legal_address_country, e.entity_legal_address_postal_code, e.entity_headquarters_address_first_address_line, e.entity_headquarters_address_city, e.entity_headquarters_address_region, e.entity_headquarters_address_country, e.entity_headquarters_address_postal_code, e.entity_entity_creation_date, e.registration_initial_registration_date, e.registration_last_update_date, e.registration_next_renewal_date, e.registration_registration_status, e.entity_transliterated_name_1, e.entity_transliterated_name_1_type
    from ores_dq_lei_entities_artefact_tbl e
    join ores_dq_datasets_tbl d on d.id = e.dataset_id
     and d.code = 'gleif.lei_entities.small'
     and d.valid_to = ores_utility_infinity_timestamp_fn()
    where e.lei in ('G5GSEF7VJP5I7OUK5573', '2138002D2Q4SDWONEG28');
end $$;

select is(
    (select record_count::int from ores_refdata_publish_lei_counterparties_from_dq_fn(
        (select id from ores_dq_datasets_tbl where code = 'test.ore_partial_publish'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        ores_utility_system_tenant_id_fn(),
        p_params => '{"relationship_dataset_code": "gleif.lei_relationships.small"}'::jsonb)
     where action = 'inserted'),
    1,
    'a publish adds only the counterparties the tenant lacks');

select is(
    (select c.parent_counterparty_id::text
     from ores_refdata_counterparties_tbl c
     join ores_refdata_counterparty_identifiers_tbl ci
       on ci.tenant_id = c.tenant_id and ci.counterparty_id = c.id
      and ci.id_scheme = 'LEI' and ci.id_value = '2138002D2Q4SDWONEG28'
      and ci.valid_to = ores_utility_infinity_timestamp_fn()
     where c.valid_to = ores_utility_infinity_timestamp_fn()),
    pg_temp.alias_target('CPTY_A'),
    'a new counterparty links to a parent the tenant already holds');

select is(pg_temp.alias_target('CP'), pg_temp.alias_target('CPTY'),
    'CP and CPTY, which share the netting set NS, name one counterparty');

select results_eq(
    $$select action, record_count from ores_refdata_publish_counterparty_aliases_from_dq_fn(
        (select id from ores_dq_datasets_tbl where code = 'ore.counterparty_aliases'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        ores_utility_system_tenant_id_fn())$$,
    $$values ('skipped'::text, 23::bigint)$$,
    'a second publish of the aliases writes none of them again');

do $$
declare
    v_dataset_id uuid;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.ore_alias_without_counterparty', 'ORE', 'Parties', 'Reference Data', 'NONE',
        'Primary', 'Actual', 'Raw', 'GLEIF Golden Copy Extraction',
        'Alias publish test', 'Test dataset.', 'ORE', 'Test', current_date,
        'Open Data', 'counterparty_aliases');
    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'test.ore_alias_without_counterparty'
      and valid_to = ores_utility_infinity_timestamp_fn();
    insert into ores_dq_counterparty_aliases_artefact_tbl (dataset_id, tenant_id, id_value,
        version, id_scheme, lei, description)
    values (v_dataset_id, ores_utility_system_tenant_id_fn(), 'NO_SUCH_BANK', 0, 'ORE',
        '00000000000000000000', 'test');
end $$;

select results_eq(
    $$select action, record_count from ores_refdata_publish_counterparty_aliases_from_dq_fn(
        (select id from ores_dq_datasets_tbl where code = 'test.ore_alias_without_counterparty'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        ores_utility_system_tenant_id_fn())$$,
    $$values ('skipped'::text, 1::bigint)$$,
    'an alias whose LEI the tenant holds no counterparty for is skipped');

do $$
declare
    v_dataset_id uuid;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.ore_alias_staged_twice', 'ORE', 'Parties', 'Reference Data', 'NONE',
        'Primary', 'Actual', 'Raw', 'GLEIF Golden Copy Extraction',
        'Alias staged twice test', 'Test dataset.', 'ORE', 'Test', current_date,
        'Open Data', 'counterparty_aliases');
    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'test.ore_alias_staged_twice'
      and valid_to = ores_utility_infinity_timestamp_fn();
    insert into ores_dq_counterparty_aliases_artefact_tbl (dataset_id, tenant_id, id_value,
        version, id_scheme, lei, description)
    values
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'STAGED_TWICE', 0, 'ORE',
         'G5GSEF7VJP5I7OUK5573', 'test'),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'STAGED_TWICE', 0, 'ORE',
         '7LTWFZYICNSX8D621K86', 'test');
end $$;

select results_eq(
    $$select action, record_count from ores_refdata_publish_counterparty_aliases_from_dq_fn(
        (select id from ores_dq_datasets_tbl where code = 'test.ore_alias_staged_twice'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        ores_utility_system_tenant_id_fn())$$,
    $$values ('inserted'::text, 1::bigint), ('skipped'::text, 1::bigint)$$,
    'a name staged twice in one dataset is written once');

do $$
declare
    v_dataset_id uuid;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.ore_alias_taken', 'ORE', 'Parties', 'Reference Data', 'NONE',
        'Primary', 'Actual', 'Raw', 'GLEIF Golden Copy Extraction',
        'Alias conflict test', 'Test dataset.', 'ORE', 'Test', current_date,
        'Open Data', 'counterparty_aliases');
    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'test.ore_alias_taken'
      and valid_to = ores_utility_infinity_timestamp_fn();
    -- CPTY_A is already the alias of one counterparty, and this dataset names
    -- a different one, so the name cannot be applied over.
    insert into ores_dq_counterparty_aliases_artefact_tbl (dataset_id, tenant_id, id_value,
        version, id_scheme, lei, description)
    values (v_dataset_id, ores_utility_system_tenant_id_fn(), 'CPTY_A', 0, 'ORE',
        '7H6GLXDRUGQFU57RNE97', 'test');
end $$;

select throws_like(
    $$select * from ores_refdata_publish_counterparty_aliases_from_dq_fn(
        (select id from ores_dq_datasets_tbl where code = 'test.ore_alias_taken'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        ores_utility_system_tenant_id_fn())$$,
    '%Alias CPTY_A already belongs to another counterparty%',
    'an alias the tenant already resolves to another counterparty is refused');

select * from finish();

rollback;
