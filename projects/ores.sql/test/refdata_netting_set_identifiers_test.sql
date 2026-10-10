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
 * pgTAP tests for netting set identifiers.
 *
 * Tests cover:
 * - A netting set may answer to several ORE ids
 * - An ORE id answers to one netting set per party
 * - Two parties may each hold a netting set that answers to the same ORE id
 * - Other schemes allow one value on several netting sets
 * - A new version of an alias keeps its name
 * - A deleted alias frees its ORE id for another netting set of the party
 * - An identifier must name an existing netting set and a known scheme
 *
 * Run with: pg_prove -d <database> test/refdata_netting_set_identifiers_test.sql
 */

begin;

select plan(9);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

insert into ores_refdata_parties_tbl (
    id, tenant_id, full_name, short_code, party_category, party_type,
    business_center_code, parent_party_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    ('00000000-0000-0000-0000-0000000a5a01'::uuid, ores_utility_system_tenant_id_fn(),
     'NSI Test One', 'NSITE1', 'Operational', 'CorporateGroup', 'WRLD', null, 'Active',
     current_user, current_user, 'system.test', 'identifier fixture'),
    ('00000000-0000-0000-0000-0000000a5a02'::uuid, ores_utility_system_tenant_id_fn(),
     'NSI Test Two', 'NSITE2', 'Operational', 'Corporate', 'WRLD',
     '00000000-0000-0000-0000-0000000a5a01'::uuid, 'Active',
     current_user, current_user, 'system.test', 'identifier fixture');

select set_config('app.visible_party_ids',
    '{00000000-0000-0000-0000-0000000a5a01,00000000-0000-0000-0000-0000000a5a02}', true);

insert into ores_refdata_netting_sets_tbl (id, tenant_id, version, code, party_id, modified_by,
    performed_by, change_reason_code, change_commentary)
values
    ('00000000-0000-0000-0000-0000000a5000'::uuid, ores_utility_system_tenant_id_fn(), 0,
     'NSITEST-0', '00000000-0000-0000-0000-0000000a5a01'::uuid, current_user, current_user,
     'system.new_record', 'test'),
    ('00000000-0000-0000-0000-0000000a5001'::uuid, ores_utility_system_tenant_id_fn(), 0,
     'NSITEST-1', '00000000-0000-0000-0000-0000000a5a01'::uuid, current_user, current_user,
     'system.new_record', 'test'),
    ('00000000-0000-0000-0000-0000000a5002'::uuid, ores_utility_system_tenant_id_fn(), 0,
     'NSITEST-0', '00000000-0000-0000-0000-0000000a5a02'::uuid, current_user, current_user,
     'system.new_record', 'test');

create or replace function pg_temp.add_identifier(p_netting_set uuid, p_scheme text,
    p_value text)
returns void as $$
    insert into ores_refdata_netting_set_identifiers_tbl (tenant_id, id, version,
        netting_set_id, party_id, id_scheme, id_value, modified_by, performed_by,
        change_reason_code, change_commentary)
    select ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0, p_netting_set,
        coalesce((select ns.party_id from ores_refdata_netting_sets_tbl ns
                  where ns.id = p_netting_set
                    and ns.valid_to = ores_utility_infinity_timestamp_fn()),
                 '00000000-0000-0000-0000-0000000a5a01'::uuid),
        p_scheme, p_value, current_user, current_user, 'system.new_record', 'test';
$$ language sql;

select lives_ok(
    $$select pg_temp.add_identifier('00000000-0000-0000-0000-0000000a5000', 'ORE', 'NSITEST_A');
      select pg_temp.add_identifier('00000000-0000-0000-0000-0000000a5000', 'ORE', 'NSITEST_A_full')$$,
    'a netting set takes several ORE ids');

select throws_ok(
    $$select pg_temp.add_identifier('00000000-0000-0000-0000-0000000a5001', 'ORE', 'NSITEST_A')$$,
    '23505', null,
    'an ORE id answers to one netting set per party');

select lives_ok(
    $$select pg_temp.add_identifier('00000000-0000-0000-0000-0000000a5002', 'ORE', 'NSITEST_A')$$,
    'another party may use the same ORE id for its own netting set');

select lives_ok(
    $$select pg_temp.add_identifier('00000000-0000-0000-0000-0000000a5000', 'INTERNAL', 'NSITEST_SHARED');
      select pg_temp.add_identifier('00000000-0000-0000-0000-0000000a5001', 'INTERNAL', 'NSITEST_SHARED')$$,
    'other schemes allow one value on several netting sets');

select lives_ok(
    $$insert into ores_refdata_netting_set_identifiers_tbl (tenant_id, id, version,
          netting_set_id, party_id, id_scheme, id_value, description, modified_by, performed_by,
          change_reason_code, change_commentary)
      select tenant_id, id, version, netting_set_id, party_id, id_scheme, id_value,
          'Netting set id used by the ORE samples', current_user, current_user,
          'system.new_record', 'test'
      from ores_refdata_netting_set_identifiers_tbl
      where tenant_id = ores_utility_system_tenant_id_fn()
        and party_id = '00000000-0000-0000-0000-0000000a5a01'::uuid
        and id_scheme = 'ORE' and id_value = 'NSITEST_A'
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    'a new version of an alias keeps its name');

select is(
    (select netting_set_id::text from ores_refdata_netting_set_identifiers_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and party_id = '00000000-0000-0000-0000-0000000a5a01'::uuid
       and id_scheme = 'ORE' and id_value = 'NSITEST_A'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    '00000000-0000-0000-0000-0000000a5000',
    'the ORE id resolves to its netting set');

select lives_ok(
    $$delete from ores_refdata_netting_set_identifiers_tbl
      where tenant_id = ores_utility_system_tenant_id_fn()
        and party_id = '00000000-0000-0000-0000-0000000a5a01'::uuid
        and id_scheme = 'ORE' and id_value = 'NSITEST_A';
      select pg_temp.add_identifier('00000000-0000-0000-0000-0000000a5001', 'ORE', 'NSITEST_A')$$,
    'a deleted alias frees its ORE id for another netting set of the party');

select throws_like(
    $$select pg_temp.add_identifier('00000000-0000-0000-0000-0000000a5fff', 'ORE', 'NSITEST_ORPHAN')$$,
    '%No active netting set found%',
    'an identifier must name an existing netting set');

select throws_like(
    $$select pg_temp.add_identifier('00000000-0000-0000-0000-0000000a5000', 'NSITEST_NO_SCHEME', 'X')$$,
    '%NSITEST_NO_SCHEME%',
    'an identifier must use a known scheme');

select * from finish();

rollback;
