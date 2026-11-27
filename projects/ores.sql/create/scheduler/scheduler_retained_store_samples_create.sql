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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_create.mustache
 * To modify, update the template and regenerate.
 *
 * Retained Store Sample Table
 *
 * A store's size is a series, not an adjective. The sampler measures each store it
 * can reach and writes one row here per store per interval, so a screen can say what
 * a store holds now and which way it has been going.
 *
 * The measurement is keyed by the store's code, not by its target, so the series
 * survives a table being replaced. The catalogue is what says which code a target
 * belongs to, and a store the sampler cannot reach — an object prefix or a folder on
 * a host — simply has no rows: an absent series is the honest answer, and better
 * than a zero.
 *
 * The model states :current_state: and carries no audit columns: a sample is a
 * fact about an instant, so the bi-temporal pair and the actor add nothing to it.
 */

create table if not exists "ores_scheduler_retained_store_samples_tbl" (
    "id" uuid not null,
    "sampled_at" timestamp with time zone not null,
    "tenant_id" uuid not null,
    "store_code" text not null,
    "size_bytes" bigint not null default 0,
    "measure_count" bigint not null default 0,
    primary key (id, sampled_at)
);



create or replace function ores_scheduler_retained_store_samples_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_scheduler_retained_store_samples_insert_trg
before insert on "ores_scheduler_retained_store_samples_tbl"
for each row execute function ores_scheduler_retained_store_samples_insert_fn();

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice 'TimescaleDB detected - creating hypertable (1 day chunks)';

        perform public.create_hypertable(
            '"ores_scheduler_retained_store_samples_tbl"',
            'sampled_at',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
        -- A hypertable grows without bound unless a retention policy drops the
        -- chunks that have aged out. The licence decides whether the policy can
        -- be set: the community edition has no retention job, so the model's
        -- interval is reported rather than silently ignored.
        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;
            if current_license = 'timescale' then
                perform public.add_retention_policy(
                    '"ores_scheduler_retained_store_samples_tbl"',
                    drop_after => interval '180 days',
                    if_not_exists => true
                );
            else
                raise notice 'TimescaleDB community edition - no retention policy is set for ores_scheduler_retained_store_samples_tbl';
            end if;
        end;
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;

-- =============================================================================
-- Row-level security: tenant isolation for Retained Store Sample
-- Every session reads its own rows and the system tenant's rows; writes
-- stay the current tenant's.
-- =============================================================================
alter table ores_scheduler_retained_store_samples_tbl enable row level security;

drop policy if exists retained_store_samples_tbl_tenant_isolation_policy
    on ores_scheduler_retained_store_samples_tbl;

create policy retained_store_samples_tbl_tenant_isolation_policy
on ores_scheduler_retained_store_samples_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- The read admits the system tenant's rows beside the caller's; the policy
-- above still binds every write to the caller's own rows. It must: a single
-- for-all policy that admitted them would let a tenant change or remove a
-- shared row. See doc/knowledge/security/data_read_across_tenants.org.
drop policy if exists retained_store_samples_tbl_shared_reference_read_policy
    on ores_scheduler_retained_store_samples_tbl;

create policy retained_store_samples_tbl_shared_reference_read_policy
on ores_scheduler_retained_store_samples_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
);
