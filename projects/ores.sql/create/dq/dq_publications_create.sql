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
 * Publication Table
 *
 * An append-only log of datasets published to production tables: which dataset
 * went where, under which mode, and how many records each outcome touched. The
 * publish path writes one row per dataset it dispatches.
 *
 * The table is called ores_dq_dataset_publications_tbl, so the model states its
 * table name rather than deriving it: the entity is a publication, and the table
 * names the thing published.
 *
 * It is a run log, not an entity with a lifecycle. Nothing edits a row after it is
 * written, so the model declares :current_state:: no transaction-time pair, no
 * exclusion constraint, no version and no audit tail. Rows carry their own
 * published_at, which is the ordering the reads use. The surrogate id is the
 * primary key because a dataset may be published many times.
 */

create table if not exists "ores_dq_dataset_publications_tbl" (
    "id" uuid not null default gen_random_uuid(),
    "tenant_id" uuid not null,
    "dataset_id" uuid not null,
    "dataset_code" text not null,
    "mode" text not null,
    "target_table" text not null,
    "records_inserted" bigint not null default 0,
    "records_updated" bigint not null default 0,
    "records_skipped" bigint not null default 0,
    "records_deleted" bigint not null default 0,
    "published_by" text not null,
    "published_at" timestamp with time zone not null default current_timestamp,
    primary key (id),
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("dataset_id" <> ores_utility_nil_uuid_fn()),
    check ("dataset_code" <> ''),
    check ("mode" in ('upsert', 'insert_only', 'replace_all')),
    check ("target_table" <> ''),
    check ("records_inserted" >= 0),
    check ("records_updated" >= 0),
    check ("records_skipped" >= 0),
    check ("records_deleted" >= 0),
    check ("published_by" <> '')
);



create index if not exists publications_dataset_id_idx
on "ores_dq_dataset_publications_tbl" (dataset_id);

create index if not exists publications_published_at_idx
on "ores_dq_dataset_publications_tbl" (published_at);

create index if not exists publications_published_by_idx
on "ores_dq_dataset_publications_tbl" (published_by);

create index if not exists publications_tenant_idx
on "ores_dq_dataset_publications_tbl" (tenant_id);

create or replace function ores_dq_dataset_publications_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_dq_dataset_publications_insert_trg
before insert on "ores_dq_dataset_publications_tbl"
for each row execute function ores_dq_dataset_publications_insert_fn();

