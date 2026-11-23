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
 * A provider of the system kind is the installation's own: no tenant reads it,
 * and no tenant row may name it. The system tenant reads it like any other.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/marketdata_system_provider_test.sql
 */

begin;

select plan(5);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select kind from ores_marketdata_providers_tbl
     where code = 'latent' and valid_to = ores_utility_infinity_timestamp_fn()),
    'system',
    'the latent provider is seeded, of the system kind'
);

select set_config('app.current_tenant_id', 'a0000000-0000-0000-0000-00000000c0de', true);

select is(
    (select count(*)::int from ores_marketdata_providers_tbl
     where code = 'latent' and valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'a tenant does not read the latent provider'
);

select ok(
    exists (select 1 from ores_marketdata_providers_tbl
            where code = 'bloomberg' and valid_to = ores_utility_infinity_timestamp_fn()),
    'a tenant still reads a vendor of the system tenant'
);

select throws_ok(
    $$select ores_marketdata_validate_provider_fn(
        'a0000000-0000-0000-0000-00000000c0de'::uuid, 'latent')$$,
    '23503',
    null,
    'a tenant row may not name the latent provider'
);

select is(
    ores_marketdata_validate_provider_fn(ores_utility_system_tenant_id_fn(), 'latent'),
    'latent',
    'a system tenant row may name it'
);

select * from finish();
rollback;
