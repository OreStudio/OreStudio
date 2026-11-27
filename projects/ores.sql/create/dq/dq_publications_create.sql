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
    "outcome" text not null default 'published',
    "messages" jsonb not null,
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
    check ("published_by" <> ''),
    check ("outcome" in ('published', 'published_with_warnings', 'failed'))
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


/**
 * Records one publication run.
 *
 * The run log is written by every target service, and none of them holds a grant
 * on the DQ tables it would need to read the dataset's code, its target table or
 * the person who asked. This function reads them once and appends the row, so the
 * caller passes what only it knows: what the run wrote and what it said.
 *
 * A run is recorded whatever its outcome, because a log of the successes alone
 * answers "when was this last published?" with an older run that did write
 * something.
 */
create or replace function ores_dq_record_publication_fn(
    p_dataset_id uuid,
    p_instance_id uuid,
    p_mode text,
    p_inserted bigint,
    p_updated bigint,
    p_skipped bigint,
    p_deleted bigint,
    p_outcome text,
    p_messages jsonb
) returns void as $$
declare
    v_code text;
    v_target_table text;
    v_published_by text;
begin
    select d.code, coalesce(at.target_table, '')
      into v_code, v_target_table
      from ores_dq_datasets_tbl d
      left join ores_dq_artefact_types_tbl at on at.code = d.artefact_type
     where d.id = p_dataset_id
       and d.valid_to = ores_utility_infinity_timestamp_fn();

    -- A dataset with no target table cannot publish, so a run that reached the
    -- log has one. An empty one here means the store changed under the run.
    if v_code is null or v_target_table = '' then
        return;
    end if;

    select coalesce(created_by, '')
      into v_published_by
      from ores_workflow_workflow_instances_tbl
     where id = p_instance_id;
    if v_published_by is null or v_published_by = '' then
        v_published_by := 'unknown';
    end if;

    insert into ores_dq_dataset_publications_tbl (
        tenant_id, dataset_id, dataset_code, mode, target_table,
        records_inserted, records_updated, records_skipped, records_deleted,
        published_by, outcome, messages)
    values (
        ores_iam_current_tenant_id_fn(), p_dataset_id, v_code, p_mode, v_target_table,
        p_inserted, p_updated, p_skipped, p_deleted,
        v_published_by, p_outcome, p_messages);
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

revoke execute on function ores_dq_record_publication_fn(
    uuid, uuid, text, bigint, bigint, bigint, bigint, text, jsonb) from public;

-- The step that publishes a dataset runs in the target's service, so every
-- service role records through this function. It takes the tenant from the
-- session and states only the run, so the grant opens no read across a tenant.
grant execute on function ores_dq_record_publication_fn(
    uuid, uuid, text, bigint, bigint, bigint, bigint, text, jsonb)
    to :iam_service_user,
       :refdata_service_user,
       :dq_service_user,
       :variability_service_user,
       :assets_service_user,
       :scheduler_service_user,
       :reporting_service_user,
       :telemetry_service_user,
       :trading_service_user,
       :compute_service_user,
       :synthetic_service_user,
       :workflow_service_user,
       :ore_service_user,
       :marketdata_service_user,
       :analytics_service_user,
       :storage_service_user,
       :inbox_service_user,
       :test_dml_user,
       :test_ddl_user;
