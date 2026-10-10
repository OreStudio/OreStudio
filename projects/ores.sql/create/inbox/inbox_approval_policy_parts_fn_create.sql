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
 * The parts a change needs, from the policy.
 *
 * A policy row names an entity type and an operation, and, when one column
 * decides the part, that column. The function returns each part the policy
 * names for the act, once. A row with no column applies to every act of its
 * entity type and operation. A row with a column applies when that column is
 * among the columns the act changes. A change that creates a row changes every
 * column, so the caller passes them all.
 *
 * The system tenant's rows and the caller's own rows both count. A tenant may
 * add a part to an act and never remove one, so the union is the policy.
 *
 * The function returns no row for an act the policy does not gate. The caller
 * decides what that means.
 */
create or replace function ores_inbox_policy_parts_fn(
    p_entity_type text,
    p_operation text,
    p_columns text[]
) returns setof text as $$
    select distinct p.part_code
    from ores_inbox_approval_policies_tbl p
    where p.valid_to = ores_utility_infinity_timestamp_fn()
      and p.tenant_id in (ores_utility_system_tenant_id_fn(), ores_iam_current_tenant_id_fn())
      and p.entity_type = p_entity_type
      and p.operation = p_operation
      and (p.field_name is null or p.field_name = any(p_columns))
    order by p.part_code;
$$ language sql stable;
