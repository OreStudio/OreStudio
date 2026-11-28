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
 * pgTAP tests for the relation-size sampler.
 *
 * The sampler measures every relation and writes one row each, so the series can
 * state what a table is doing:
 * - one row per relation in the public schema, and the count it returns is that
 *   number
 * - every row belongs to the system tenant, because the database is the
 *   installation's
 * - a second firing writes a second point for each relation, which is the series
 * - the sizes are the server's own figure for the relation
 *
 * Run with: pg_prove -d <database> test/scheduler_relation_sizes_test.sql
 */

begin;

select plan(4);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

delete from ores_scheduler_relation_sizes_tbl;

select is(
    ores_scheduler_sample_relation_sizes_fn(),
    (select count(*)::bigint
     from pg_class c
     join pg_namespace n on n.oid = c.relnamespace
     where n.nspname = 'public' and c.relkind in ('r', 'p')),
    'the sampler measures every relation the public schema holds');

select is(
    (select count(*) from ores_scheduler_relation_sizes_tbl
     where tenant_id <> ores_utility_system_tenant_id_fn()),
    0::bigint,
    'every measurement belongs to the installation');

select is(
    (select count(*) from ores_scheduler_relation_sizes_tbl
     where relation_name = 'ores_scheduler_relation_sizes_tbl'),
    1::bigint,
    'the series measures itself, as it measures everything else');

select ores_scheduler_sample_relation_sizes_fn();

select is(
    (select count(*) from ores_scheduler_relation_sizes_tbl
     where relation_name = 'ores_scheduler_relation_sizes_tbl'),
    2::bigint,
    'a second firing writes a second point, which is the series a screen reads');

select * from finish();

rollback;
