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
 * pgTAP tests for the installation's capacity samples.
 *
 * One function writes the row, so the write is where the figures are decided:
 * - the database's size is the server's own answer, not the caller's
 * - the free space the caller measured is kept, and the path and host with it
 * - a free space the caller could not measure is null, not zero, so a reader
 *   cannot mistake an unreadable disk for a full one
 * - the row belongs to the system tenant, because the database is the
 *   installation's and not a tenant's
 * - a database the server does not hold is recorded with a size of zero and a
 *   note that says so, rather than failing the firing
 *
 * Run with: pg_prove -d <database> test/scheduler_capacity_samples_test.sql
 */

begin;

select plan(6);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

delete from ores_scheduler_capacity_samples_tbl;

select is(
    ores_scheduler_record_capacity_fn(current_database(), 'test-host', '/', 5000000, 9000000),
    pg_database_size(current_database()),
    'the database size is the server''s own answer');

select is(
    (select count(*) from ores_scheduler_capacity_samples_tbl),
    1::bigint,
    'one measurement writes one row');

select is(
    (select disk_free_bytes from ores_scheduler_capacity_samples_tbl order by sampled_at desc limit 1),
    5000000::bigint,
    'the free space the caller measured is kept');

select is(
    (select host_name || ' ' || measured_path from ores_scheduler_capacity_samples_tbl
     order by sampled_at desc limit 1),
    'test-host /',
    'the host and the path the figure describes are kept with it');

select is(
    (select tenant_id from ores_scheduler_capacity_samples_tbl order by sampled_at desc limit 1),
    ores_utility_system_tenant_id_fn(),
    'the measurement belongs to the installation, not to a tenant');

select ores_scheduler_record_capacity_fn('no_such_database', 'test-host', '/', null, null);

select is(
    (select database_bytes || '|' || note from ores_scheduler_capacity_samples_tbl
     where database_name = 'no_such_database'),
    '0|the database ''no_such_database'' is not on this server',
    'a database the server does not hold is recorded with the reason, not a failure');

select * from finish();

rollback;
