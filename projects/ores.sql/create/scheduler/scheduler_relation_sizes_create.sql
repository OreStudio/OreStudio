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
 * Every relation's size, as a series.
 *
 * A store's size answers what one catalogued table holds. It does not answer what
 * a table is doing: whether it doubled last week, whether a clean-up held, and
 * whether it is the one that needs a period. That question is asked of every
 * relation the installation holds, named by the catalogue or not, and it needs a
 * series and not a snapshot.
 *
 * The measurement is one row per relation per firing, keyed by the relation's
 * name because that is what the catalogue names a target by. A relation that
 * disappears stops gaining rows; the series that is already there stays readable.
 *
 * The series is a store itself, so it is in the catalogue with a period: a screen
 * that measures everything to bound it must not become the thing nothing bounds.
 */

create table if not exists "ores_scheduler_relation_sizes_tbl" (
    "id" uuid not null default gen_random_uuid(),
    "sampled_at" timestamp with time zone not null,
    "tenant_id" uuid not null,
    "relation_name" text not null,
    "size_bytes" bigint not null default 0,
    primary key (id, sampled_at),
    check ("relation_name" <> ''),
    check ("size_bytes" >= 0)
);

create index if not exists relation_sizes_name_sampled_at_idx
on "ores_scheduler_relation_sizes_tbl" ("relation_name", "sampled_at" desc);

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        perform public.create_hypertable(
            '"ores_scheduler_relation_sizes_tbl"',
            'sampled_at',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
    end if;
end $$;

alter table ores_scheduler_relation_sizes_tbl enable row level security;

drop policy if exists relation_sizes_tbl_tenant_isolation_policy
    on ores_scheduler_relation_sizes_tbl;

create policy relation_sizes_tbl_tenant_isolation_policy
on ores_scheduler_relation_sizes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists relation_sizes_tbl_shared_reference_read_policy
    on ores_scheduler_relation_sizes_tbl;

create policy relation_sizes_tbl_shared_reference_read_policy
on ores_scheduler_relation_sizes_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
);

comment on table ores_scheduler_relation_sizes_tbl is
'Every relation the installation holds, measured once a firing, so a screen can
 state what a table is doing and not only what it holds. The measurement is the
 installation''s: its rows belong to the system tenant.';
