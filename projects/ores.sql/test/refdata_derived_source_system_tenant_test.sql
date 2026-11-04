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
 * The derived source of FX volatility quotes runs in the system tenant: its
 * series carry a derivation kind the database accepts, and a party-scoped
 * publish into the system tenant writes for its system party, so the
 * conventions the source reads land there.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/refdata_derived_source_system_tenant_test.sql
 */

begin;

select plan(5);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select count(*)::int from ores_refdata_derivation_kinds_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code = 'FX_VOL_DERIVATION'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'FX_VOL_DERIVATION is a derivation kind'
);

select isnt(
    ores_refdata_publish_target_party_fn(ores_utility_system_tenant_id_fn(), '{}'::jsonb),
    null,
    'a publish into the system tenant that names no party has a party to write for'
);

select is(
    ores_refdata_publish_target_party_fn(ores_utility_system_tenant_id_fn(), '{}'::jsonb),
    (select id from ores_refdata_read_system_party_fn(ores_utility_system_tenant_id_fn())),
    'that party is the system party'
);

insert into ores_refdata_parties_tbl (
    id, tenant_id, full_name, short_code, party_category, party_type,
    business_center_code, parent_party_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    '00000000-0000-0000-0000-0000000d5001'::uuid, ores_utility_system_tenant_id_fn(),
    'Derived Source Fixture', 'DSFIX', 'Operational', 'CorporateGroup',
    'WRLD', null, 'Active', current_user, current_user,
    'system.test', 'pgTAP root party fixture');

select is(
    ores_refdata_publish_target_party_fn(ores_utility_system_tenant_id_fn(), '{}'::jsonb),
    '00000000-0000-0000-0000-0000000d5001'::uuid,
    'a root party of the system tenant is still preferred to its system party'
);

select is(
    ores_refdata_publish_target_party_fn(gen_random_uuid(), '{}'::jsonb),
    null,
    'a tenant with no root party still has no party to write for'
);

select * from finish();

rollback;
