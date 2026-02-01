/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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

-- =============================================================================
-- Security tracking for login attempts.
-- Current-state table (no temporal versioning).
-- Tracks failed attempts and lock status.
-- =============================================================================

create table if not exists ores_iam_login_info_tbl (
    "tenant_id" uuid not null,
    "account_id" uuid not null,
    "last_ip" inet not null,
    "last_attempt_ip" inet not null,
    "failed_logins" integer not null,
    "locked" integer not null,
    "last_login" timestamp with time zone not null,
    "online" integer not null,
    "password_reset_required" integer not null default 0,
    primary key (account_id)
);

create index if not exists ores_iam_login_info_tenant_idx
on ores_iam_login_info_tbl (tenant_id);

create index if not exists ores_iam_login_info_account_id_idx
on ores_iam_login_info_tbl (account_id);

create index if not exists ores_iam_login_info_locked_idx
on ores_iam_login_info_tbl (locked)
where locked = 0;

-- -----------------------------------------------------------------------------
-- Trigger: Set tenant_id from session variable if not provided
-- -----------------------------------------------------------------------------
create or replace function ores_iam_login_info_before_insert_fn()
returns trigger as $$
begin
    -- Set tenant_id from session variable if not provided
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);
    return new;
end;
$$ language plpgsql;

create trigger ores_iam_login_info_before_insert_trigger
before insert on ores_iam_login_info_tbl
for each row execute function ores_iam_login_info_before_insert_fn();
