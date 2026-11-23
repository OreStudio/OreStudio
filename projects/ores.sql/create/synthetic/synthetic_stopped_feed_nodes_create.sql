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
 * Stopped Feed Node Table
 *
 * A node of the synthetic feed graph that an operator stopped, named by the
 * naming scheme. Two kinds of node are stopped here:
 *
 * - a /source/, a simulated source of the market data catalogue such as
 *   bloomberg-a, served by an adapter that runs the feeds the store demands of
 *   it and holds the vol surfaces its provider quotes; its stop stops the
 *   adapter;
 * - a /view/, one vendor's quotes of a latent vol surface, such as
 *   synthetic.ice.fx.fx_option.audusd; its stop withholds that vendor's points
 *   of that surface, and the surface and the other vendors run on.
 *
 * A row keeps the node stopped, a restart of the service included, until someone
 * starts it again and the row is removed. The row's history is the record of who
 * stopped and started the node, when, and why. One table holds every kind, so a
 * node is stopped and started by one pair of verbs, by its name; see
 * [[id:2376A086-4C3C-4D61-92E5-29B122DB6A41][Synthetic feed graph]].
 *
 * An operator stops and starts a node through the feed node verbs, which take
 * the feeds run permission, write this row and stop or start the node together.
 * The generated write and delete of this entity take their own permissions, in
 * the system tenant only, and change the row alone: the node follows the row at
 * the next start of the service. They are for the record, not for operating a
 * node.
 */

create table if not exists "ores_synthetic_stopped_feed_nodes_tbl" (
    "node_name" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "kind" text not null,
    "provider_code" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "acting_agent" text not null default '',
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, node_name, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        node_name WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("node_name" <> ''),
    check ("kind" in ('source', 'view'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists stopped_feed_nodes_version_uniq_idx
on "ores_synthetic_stopped_feed_nodes_tbl" (tenant_id, node_name, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists stopped_feed_nodes_node_name_uniq_idx
on "ores_synthetic_stopped_feed_nodes_tbl" (tenant_id, node_name)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists stopped_feed_nodes_tenant_idx
on "ores_synthetic_stopped_feed_nodes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_synthetic_stopped_feed_nodes_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate provider_code
    NEW.provider_code := ores_marketdata_validate_provider_fn(NEW.tenant_id, NEW.provider_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_synthetic_stopped_feed_nodes_tbl"
    where tenant_id = NEW.tenant_id
      and node_name = NEW.node_name
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        -- The write states what it believes about the row, and the store is
        -- what decides. Version zero means one thing: no current row exists.
        -- So a create that collides with a live row is refused here, for every
        -- client, rather than by a check each client has to remember.
        if NEW.version = 0 then
            if not ores_utility_version_replace_allowed_fn() then
                perform ores_outcome_raise_fn(
                    'already_exists',
                    'stopped_feed_node',
                    'node_name');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'stopped_feed_node',
                'node_name',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_synthetic_stopped_feed_nodes_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and node_name = NEW.node_name
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);
    NEW.acting_agent = coalesce(ores_iam_current_agent_fn(), '');

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_synthetic_stopped_feed_nodes_insert_trg
before insert on "ores_synthetic_stopped_feed_nodes_tbl"
for each row execute function ores_synthetic_stopped_feed_nodes_insert_fn();

create or replace rule ores_synthetic_stopped_feed_nodes_delete_rule as
on delete to "ores_synthetic_stopped_feed_nodes_tbl" do instead (
    update "ores_synthetic_stopped_feed_nodes_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and node_name = OLD.node_name
      -- A tenant reads the shared rows of the system tenant, so OLD can be one.
      -- The rule runs with the owner's rights, which row-level security does
      -- not bind, so the rule states the own-tenant limit that the delete
      -- policy cannot enforce here.
      and tenant_id = ores_iam_current_tenant_id_fn()
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Stopped Feed Node
-- Every session reads its own rows and the system tenant's rows; writes
-- stay the current tenant's.
-- =============================================================================
alter table ores_synthetic_stopped_feed_nodes_tbl enable row level security;

drop policy if exists stopped_feed_nodes_tbl_tenant_isolation_policy
    on ores_synthetic_stopped_feed_nodes_tbl;

create policy stopped_feed_nodes_tbl_tenant_isolation_policy
on ores_synthetic_stopped_feed_nodes_tbl
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
drop policy if exists stopped_feed_nodes_tbl_shared_reference_read_policy
    on ores_synthetic_stopped_feed_nodes_tbl;

create policy stopped_feed_nodes_tbl_shared_reference_read_policy
on ores_synthetic_stopped_feed_nodes_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
);
