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
 * Workflow Batch Link Table
 *
 * When a submit_compute workflow step runs, the service creates a batch and
 * records one row here that names the step and the step's instance. The
 * bridge polls this table, and when the batch reaches a terminal state it
 * publishes step_completed_event and deletes the row.
 *
 * The table has no validity window and carries no audit tail: the row is a
 * note that a step is outstanding, and it is deleted rather than closed. The
 * model therefore states :current_state: and :no_audit_columns:.
 *
 * The one decision the model records is the read scope. The bridge is a
 * service-level poller, not a caller acting for one tenant, so it reads
 * every tenant's links and deletes each under the tenant that owns it. That
 * is why the model states :tenant_read_scope: shared, which leaves the
 * tenant filter to row-level security rather than adding one on top, and
 * :rls_system_tenant_visible:, which is what lets the service's
 * system-tenant session see rows the tenant owns. Mutations stay
 * tenant-scoped, so the delete names the link's own tenant.
 */

create table if not exists "ores_compute_workflow_batch_links_tbl" (
    "batch_id" uuid not null,
    "tenant_id" uuid not null,
    "workflow_step_id" text not null,
    "workflow_instance_id" text not null,
    "created_at" timestamp with time zone not null,
    primary key (batch_id)
);



create index if not exists workflow_batch_links_workflow_batch_links_tenant_idx
on "ores_compute_workflow_batch_links_tbl" (tenant_id);

create or replace function ores_compute_workflow_batch_links_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_compute_workflow_batch_links_insert_trg
before insert on "ores_compute_workflow_batch_links_tbl"
for each row execute function ores_compute_workflow_batch_links_insert_fn();


-- =============================================================================
-- Row-level security: tenant isolation for Workflow Batch Link
-- System-tenant sessions may also read every tenant's rows.
-- =============================================================================
alter table ores_compute_workflow_batch_links_tbl enable row level security;

create policy workflow_batch_links_tbl_tenant_isolation_policy
on ores_compute_workflow_batch_links_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
);
