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
 * The sampler: one measurement of every store the database can see.
 *
 * A store's size is measured where the store lives. The database measures its own
 * tables; an object prefix and a folder on a compute host are measured by the
 * components that own them, which is why this function visits only the two shapes
 * a relation has. A store it cannot see is left unmeasured rather than recorded
 * as zero, because a zero would read as a store that is empty and that is a
 * different fact.
 *
 * The rows are the series a screen plots: one per store per run, keyed to the
 * store's code so the series survives the table being replaced.
 */

create or replace function ores_scheduler_sample_retained_stores_fn()
returns table (
    "store_code" text,
    "size_bytes" bigint,
    "measure_count" bigint
)
language plpgsql
security definer
as $$
declare
    store record;
    v_relation regclass;
    v_bytes bigint;
    v_count bigint;
begin
    for store in
        select s."code", s."target"
        from "ores_scheduler_retained_stores_tbl" s
        where s."valid_to" = ores_utility_infinity_timestamp_fn()
          and s."tenant_id" = ores_utility_system_tenant_id_fn()
          and s."target_kind" in ('chunked_table', 'table')
        order by s."code"
    loop
        v_relation := to_regclass(store."target");

        if v_relation is null then
            continue;
        end if;

        v_bytes := pg_total_relation_size(v_relation);

        -- The row count is the store's own estimate. Counting a table exactly
        -- would take a scan of every store on every run, which is a cost the
        -- household's own measurement should not add to the household.
        select greatest(coalesce(c.reltuples, 0), 0)::bigint
        into v_count
        from pg_catalog.pg_class c
        where c.oid = v_relation;

        insert into "ores_scheduler_retained_store_samples_tbl"
            ("id", "sampled_at", "tenant_id", "store_code", "size_bytes", "measure_count")
        values
            (gen_random_uuid(), now(), ores_utility_system_tenant_id_fn(),
             store."code", v_bytes, v_count);

        store_code := store."code";
        size_bytes := v_bytes;
        measure_count := v_count;
        return next;
    end loop;
end;
$$;

comment on function ores_scheduler_sample_retained_stores_fn() is
'Measures every store the database can see and writes one sample per store per
 run. A store whose target is not a relation the database holds is skipped rather
 than recorded as empty: an object prefix and a folder on a host are measured by
 the components that own them.';
