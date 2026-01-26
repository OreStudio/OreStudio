/* -*- mode: sql; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
set schema 'metadata';

-- =============================================================================
-- Table: dq_publications_tbl
-- =============================================================================
-- Audit table for tracking dataset publication history.
-- Records each time a dataset is published to production tables.
-- =============================================================================

create table if not exists "metadata"."dq_publications_tbl" (
    "id" uuid not null default gen_random_uuid(),
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
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid),
    check ("dataset_id" <> '00000000-0000-0000-0000-000000000000'::uuid),
    check ("dataset_code" <> ''),
    check ("mode" in ('upsert', 'insert_only', 'replace_all')),
    check ("target_table" <> ''),
    check ("records_inserted" >= 0),
    check ("records_updated" >= 0),
    check ("records_skipped" >= 0),
    check ("records_deleted" >= 0),
    check ("published_by" <> '')
);

comment on table metadata.dq_publications_tbl is
    'Audit table for tracking dataset publication history.';

comment on column metadata.dq_publications_tbl.id is
    'Unique identifier for this publication record.';

comment on column metadata.dq_publications_tbl.dataset_id is
    'ID of the dataset that was published.';

comment on column metadata.dq_publications_tbl.dataset_code is
    'Code of the dataset that was published (e.g., iso.currencies).';

comment on column metadata.dq_publications_tbl.mode is
    'Publication mode used: upsert, insert_only, or replace_all.';

comment on column metadata.dq_publications_tbl.target_table is
    'Name of the production table that received the data.';

comment on column metadata.dq_publications_tbl.records_inserted is
    'Number of records inserted during publication.';

comment on column metadata.dq_publications_tbl.records_updated is
    'Number of records updated during publication (upsert mode).';

comment on column metadata.dq_publications_tbl.records_skipped is
    'Number of records skipped during publication.';

comment on column metadata.dq_publications_tbl.records_deleted is
    'Number of records deleted during publication (replace_all mode).';

comment on column metadata.dq_publications_tbl.published_by is
    'Username of the person who initiated the publication.';

comment on column metadata.dq_publications_tbl.published_at is
    'Timestamp when the publication occurred.';

-- Index for querying publication history by dataset
create index if not exists dq_publications_dataset_id_idx
    on metadata.dq_publications_tbl(dataset_id);

-- Index for querying publication history by time
create index if not exists dq_publications_published_at_idx
    on metadata.dq_publications_tbl(published_at);

-- Index for querying publication history by user
create index if not exists dq_publications_published_by_idx
    on metadata.dq_publications_tbl(published_by);
