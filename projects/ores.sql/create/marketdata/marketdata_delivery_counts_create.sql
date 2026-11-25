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
 * Delivery Count Table
 *
 * A delivery count is a fact about usage. The fan-out counts the ticks it delivers to each party for each item
 * and writes the counts in batches. A count holds no price. The only cost at tick time is one increment in
 * memory, and one row is written for each party, item and hour at each write.
 *
 * A row holds the deliveries counted so far in an hour, so a flush adds to the total and the rating step reads one row for the hour.
 */

create table if not exists "ores_marketdata_delivery_counts_tbl" (
    "party_id" uuid not null,
    "provider_code" text not null,
    "feed_id" uuid not null,
    "oresmd_uri" text not null,
    "period_start" timestamp with time zone not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "flushed_at" timestamp with time zone not null,
    "deliveries" bigint not null,
    primary key (party_id, provider_code, feed_id, oresmd_uri, period_start),
    check ("party_id" <> ores_utility_nil_uuid_fn()),
    check ("provider_code" <> ''),
    check ("feed_id" <> ores_utility_nil_uuid_fn()),
    check ("oresmd_uri" <> ''),
    check ("provider_code" <> ''),
    check ("oresmd_uri" <> ''),
    check ("deliveries" >= 0)
);



create or replace function ores_marketdata_delivery_counts_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_delivery_counts_insert_trg
before insert on "ores_marketdata_delivery_counts_tbl"
for each row execute function ores_marketdata_delivery_counts_insert_fn();


-- =============================================================================
-- Row-level security: tenant isolation for Delivery Count
-- System-tenant sessions may also read every tenant's rows.
-- =============================================================================
alter table ores_marketdata_delivery_counts_tbl enable row level security;

drop policy if exists delivery_counts_tbl_tenant_isolation_policy
    on ores_marketdata_delivery_counts_tbl;

create policy delivery_counts_tbl_tenant_isolation_policy
on ores_marketdata_delivery_counts_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
);

-- Party isolation (RESTRICTIVE): ANDed with the permissive tenant
-- policy above, a session sees only rows whose party_id its visible
-- party set admits. The visible_party_ids-is-null passthrough applies
-- for sessions with no party restriction (tenant admins, service
-- contexts).
drop policy if exists delivery_counts_tbl_party_isolation_policy
    on ores_marketdata_delivery_counts_tbl;

create policy delivery_counts_tbl_party_isolation_policy
on ores_marketdata_delivery_counts_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
