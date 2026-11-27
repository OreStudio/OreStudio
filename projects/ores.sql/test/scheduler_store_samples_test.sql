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
 * pgTAP tests for the store sampler.
 *
 * The sampler measures the stores a database can see and writes one sample per
 * store per run:
 * - every database store is measured, with a size greater than zero
 * - a store the database cannot see, such as an object prefix, is left
 *   unmeasured rather than recorded as empty
 * - a second run writes a second point, which is the series a screen plots
 * - the bounds view reports the latest size and the moment it was measured, and
 *   reports nothing for a store it has no sample for
 *
 * Run with: pg_prove -d <database> test/scheduler_store_samples_test.sql
 */

begin;

select plan(6);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

delete from ores_scheduler_retained_store_samples_tbl;

select is(
    (select count(*) from ores_scheduler_sample_retained_stores_fn()),
    6::bigint,
    'the sampler measures every store the database can see');

select is(
    (select count(*) from ores_scheduler_retained_store_samples_tbl
     where size_bytes > 0),
    6::bigint,
    'every sample states a size greater than zero');

select is(
    (select count(*) from ores_scheduler_retained_store_samples_tbl
     where store_code = 'compute.job_inputs'),
    0::bigint,
    'a store the database cannot see is left unmeasured rather than recorded as empty');

select ores_scheduler_sample_retained_stores_fn();

select is(
    (select count(*) from ores_scheduler_retained_store_samples_tbl
     where store_code = 'marketdata.delivery_counts'),
    2::bigint,
    'a second run writes a second point, which is the series the screen plots');

select is(
    (select size_bytes from ores_scheduler_store_bounds_vw
     where code = 'marketdata.delivery_counts'),
    (select size_bytes from ores_scheduler_retained_store_samples_tbl
     where store_code = 'marketdata.delivery_counts'
     order by sampled_at desc
     limit 1),
    'the view reports the size the latest sample measured');

select is(
    (select size_bytes from ores_scheduler_store_bounds_vw
     where code = 'compute.job_inputs'),
    null,
    'the view reports nothing for a store no sample measured');

select * from finish();

rollback;
