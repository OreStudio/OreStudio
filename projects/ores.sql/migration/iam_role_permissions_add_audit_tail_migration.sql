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
 * One-shot migration: the role-permission junction's audit tail
 *
 * ores_iam_role_permissions_tbl carried valid_from and valid_to only, so a
 * change to a role's permission bundle left no record of who made it, when or
 * why. The bundle can now be written (iam.v1.roles.permissions.put), and a
 * write that cannot be attributed is worse than no write at all. The table
 * gains the tail ores_iam_account_roles_tbl already carries, and its insert
 * trigger stamps it the same way.
 *
 * Rows written before this column existed are backfilled with the actor the
 * insert trigger would have used had the columns been there: the reason is
 * 'system.initial_load' and the commentary says so, rather than borrowing a
 * reason that would claim a person made a change no person made.
 *
 * On a freshly recreated database the create script already emits the columns
 * and this migration is unnecessary. It exists for databases created before
 * the change.
 */

alter table ores_iam_role_permissions_tbl
    add column if not exists "assigned_by" text,
    add column if not exists "assigned_at" timestamp with time zone,
    add column if not exists "change_reason_code" text,
    add column if not exists "change_commentary" text;

update ores_iam_role_permissions_tbl
set "assigned_by" = coalesce(nullif("assigned_by", ''), current_user),
    "assigned_at" = coalesce("assigned_at", "valid_from"),
    "change_reason_code" = coalesce(nullif("change_reason_code", ''), 'system.initial_load'),
    "change_commentary" = coalesce(nullif("change_commentary", ''), 'Row written before the junction recorded its audit tail')
where "assigned_by" is null
   or "assigned_at" is null
   or "change_reason_code" is null
   or "change_commentary" is null;

alter table ores_iam_role_permissions_tbl
    alter column "assigned_by" set not null,
    alter column "assigned_at" set not null,
    alter column "change_reason_code" set not null,
    alter column "change_commentary" set not null;
