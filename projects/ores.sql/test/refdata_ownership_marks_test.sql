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
 * pgTAP tests for the two one-per-owner marks: the primary contact on a party
 * and the authoritative identifier on a counterparty.
 *
 * Both are a boolean column with a partial unique index over the owner, so the
 * store keeps at most one live row per owner. A second row that claims the
 * mark is refused rather than quietly leaving two rows that both claim it, and
 * a row that leaves the mark alone is accepted.
 *
 * Run with: pg_prove -d <database> test/refdata_ownership_marks_test.sql
 */

begin;

select plan(7);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- One party and one counterparty, so each child table has an owner. The suite
-- rolls both back.
insert into ores_refdata_parties_tbl (
    id, tenant_id, version, full_name, short_code, party_category, party_type,
    business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-0000000000f1'::uuid,
    ores_utility_system_tenant_id_fn(), 0, 'Ownership Mark Test Party', 'OMTP',
    'Operational', 'Bank', 'WRLD', 'Active',
    current_user, current_user, 'system.test', 'Ownership mark fixture'
);

insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version, full_name, short_code, party_type, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'b0000000-0000-0000-0000-0000000000f1'::uuid,
    ores_utility_system_tenant_id_fn(), 0, 'Ownership Mark Test Counterparty', 'OMTC',
    'Corporate', 'Active',
    current_user, current_user, 'system.test', 'Ownership mark fixture'
);

create or replace function pg_temp.add_contact(p_party uuid, p_type text, p_primary boolean)
returns void as $$
    insert into ores_refdata_party_contact_informations_tbl (
        tenant_id, id, version, party_id, contact_type, is_primary,
        modified_by, performed_by, change_reason_code, change_commentary)
    values (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0, p_party, p_type,
        p_primary, current_user, current_user, 'system.new_record', 'test');
$$ language sql;

create or replace function pg_temp.add_identifier(p_counterparty uuid, p_scheme text,
    p_value text, p_authoritative boolean)
returns void as $$
    insert into ores_refdata_counterparty_identifiers_tbl (
        tenant_id, id, version, counterparty_id, id_scheme, id_value, is_authoritative,
        modified_by, performed_by, change_reason_code, change_commentary)
    values (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0, p_counterparty,
        p_scheme, p_value, p_authoritative,
        current_user, current_user, 'system.new_record', 'test');
$$ language sql;

-- 1. A party takes a primary contact.
select lives_ok(
    $$select pg_temp.add_contact(
        'a0000000-0000-0000-0000-0000000000f1'::uuid, 'Legal', true)$$,
    'a party takes a primary contact');

-- 2. A second contact cannot claim the mark while the first holds it.
select throws_ok(
    $$select pg_temp.add_contact(
        'a0000000-0000-0000-0000-0000000000f1'::uuid, 'Operations', true)$$,
    '23505', null,
    'a second contact cannot claim the primary mark');

-- 3. A second contact that leaves the mark alone is accepted.
select lives_ok(
    $$select pg_temp.add_contact(
        'a0000000-0000-0000-0000-0000000000f1'::uuid, 'Operations', false)$$,
    'a second contact that leaves the mark alone is accepted');

-- 4. The mark is on the row, so one contact of the party carries it.
select is(
    (select count(*)::int from ores_refdata_party_contact_informations_tbl
     where party_id = 'a0000000-0000-0000-0000-0000000000f1'::uuid
       and is_primary
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'exactly one contact of the party is primary');

-- 5. A counterparty takes an authoritative identifier.
select lives_ok(
    $$select pg_temp.add_identifier(
        'b0000000-0000-0000-0000-0000000000f1'::uuid, 'LEI', 'LEI-OMTC-1', true)$$,
    'a counterparty takes an authoritative identifier');

-- 6. A second identifier cannot claim the mark while the first holds it.
select throws_ok(
    $$select pg_temp.add_identifier(
        'b0000000-0000-0000-0000-0000000000f1'::uuid, 'BIC', 'BIC-OMTC-1', true)$$,
    '23505', null,
    'a second identifier cannot claim the authoritative mark');

-- 7. An identifier that leaves the mark alone is accepted.
select lives_ok(
    $$select pg_temp.add_identifier(
        'b0000000-0000-0000-0000-0000000000f1'::uuid, 'MIC', 'MIC-OMTC-1', false)$$,
    'an identifier that leaves the mark alone is accepted');

select * from finish();

rollback;
