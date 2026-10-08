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
 * Series Axis Value Table
 *
 * One value an axis of a composite market series holds. The rows of one axis are
 * its ordered values, so a grid can be read as coordinates rather than as the set
 * of points that survived a build.
 *
 * The sequence column holds the position of each value among its axis's values.
 * A value that the axis declares and no point uses is still a declared value, and
 * a reader can tell it apart from a value the build never saw.
 *
 * The table is a current state, not a history: a rebuilt series writes its shape
 * again. A value is text because the codec keeps every value as the key spelled
 * it, and a projection that reinterpreted it would be a second spelling of the
 * same coordinate.
 */

create table if not exists "ores_marketdata_series_axis_values_tbl" (
    "series_id" uuid not null,
    "axis_field" text not null,
    "value" text not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "sequence" integer not null default 0,
    primary key (series_id, axis_field, value),
    check ("series_id" <> ores_utility_nil_uuid_fn()),
    check ("axis_field" <> ''),
    check ("value" <> '')
);



create index if not exists series_axis_values_series_idx
on "ores_marketdata_series_axis_values_tbl" (tenant_id, series_id);

create or replace function ores_marketdata_series_axis_values_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_series_axis_values_insert_trg
before insert on "ores_marketdata_series_axis_values_tbl"
for each row execute function ores_marketdata_series_axis_values_insert_fn();


-- =============================================================================
-- Row-level security: tenant isolation for Series Axis Value
-- =============================================================================
alter table ores_marketdata_series_axis_values_tbl enable row level security;

drop policy if exists series_axis_values_tbl_tenant_isolation_policy
    on ores_marketdata_series_axis_values_tbl;

create policy series_axis_values_tbl_tenant_isolation_policy
on ores_marketdata_series_axis_values_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation (RESTRICTIVE): ANDed with the permissive tenant
-- policy above, a session sees only rows whose party_id its visible
-- party set admits. The visible_party_ids-is-null passthrough applies
-- for sessions with no party restriction (tenant admins, service
-- contexts).
drop policy if exists series_axis_values_tbl_party_isolation_policy
    on ores_marketdata_series_axis_values_tbl;

create policy series_axis_values_tbl_party_isolation_policy
on ores_marketdata_series_axis_values_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
