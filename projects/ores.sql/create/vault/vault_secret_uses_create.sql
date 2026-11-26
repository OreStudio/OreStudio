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
 * Secret Use Table
 *
 * One row per act on a secret, appended and never changed. The row says who asked
 * and what happened; it never says what the value was, and there is no column for a
 * value, a plaintext or a ciphertext.
 *
 * The rows are append-only, so the model states :current_state:: an act is a fact
 * about an instant, and the bi-temporal pair has no meaning on it. The audit columns
 * go too, because the row has no author and no version of its own: the subject
 * column is the actor.
 *
 * Three acts append a row: a resolve, a rotation and a delete. A refusal appends one
 * as well, with outcome naming what was refused, because a caller that asks for a
 * secret it does not own is the row an operator most wants to read.
 *
 * The name is the secret's name at the time of the act and not a reference to the
 * secret row: the record has to outlive the row it describes, and a delete leaves a
 * use behind. The key_version is the version the row was opened under, and it is
 * zero for an act that opened nothing, such as a refusal or a delete.
 */

create table if not exists "ores_vault_secret_uses_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "name" text not null,
    "subject" text not null,
    "outcome" text not null,
    "key_version" integer not null default 0,
    "used_at" timestamp with time zone not null,
    primary key (id)
);



create index if not exists secret_uses_tenant_time_idx
on "ores_vault_secret_uses_tbl" (tenant_id, used_at desc);

create index if not exists secret_uses_name_time_idx
on "ores_vault_secret_uses_tbl" (name, used_at desc);

create index if not exists secret_uses_subject_time_idx
on "ores_vault_secret_uses_tbl" (subject, used_at desc);

create or replace function ores_vault_secret_uses_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_vault_secret_uses_insert_trg
before insert on "ores_vault_secret_uses_tbl"
for each row execute function ores_vault_secret_uses_insert_fn();


-- =============================================================================
-- Row-level security: tenant isolation for Secret Use
-- =============================================================================
alter table ores_vault_secret_uses_tbl enable row level security;

drop policy if exists secret_uses_tbl_tenant_isolation_policy
    on ores_vault_secret_uses_tbl;

create policy secret_uses_tbl_tenant_isolation_policy
on ores_vault_secret_uses_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
