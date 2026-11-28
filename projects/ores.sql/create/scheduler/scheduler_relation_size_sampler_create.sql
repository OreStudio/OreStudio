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
 * One firing of the relation-size sampler.
 *
 * Every relation in the public schema, counted with its indexes and its toast, so
 * the figure answers what the table costs the disk. The measurement is the
 * installation's and belongs to the system tenant.
 *
 * The sampler measures everything rather than the head of the list, because a
 * table that has just started to grow is by definition not yet large, and the
 * question the series answers is which table is growing.
 */

create or replace function ores_scheduler_sample_relation_sizes_fn()
returns bigint
language plpgsql
security definer
as $$
declare
    v_count bigint;
begin
    insert into "ores_scheduler_relation_sizes_tbl"
        ("sampled_at", "tenant_id", "relation_name", "size_bytes")
    select now(),
           ores_utility_system_tenant_id_fn(),
           c.relname,
           pg_total_relation_size(c.oid)
    from pg_class c
    join pg_namespace n on n.oid = c.relnamespace
    where n.nspname = 'public'
      and c.relkind in ('r', 'p');

    get diagnostics v_count = row_count;
    return v_count;
end;
$$;

comment on function ores_scheduler_sample_relation_sizes_fn() is
'Measures every relation the installation holds and writes one row each, so the
 screen can state what a table is doing and not only what it holds.';
