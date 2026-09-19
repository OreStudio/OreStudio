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
 * Login Info Table
 *
 * Login tracking and security state for one account: the last successful
 * login, the running failed-attempt count, the lock and online flags, the
 * forced password-reset flag, and the IP address of the last success and
 * the last attempt. One row per account, keyed by account_id.
 *
 * The table is current-state (see
 * projects/ores.sql/create/iam/iam_login_info_create.sql): it carries no
 * valid_from/valid_to, no GIST exclusion, no version column and no
 * audit tail, unlike the bi-temporal tables every other domain_entity in
 * this component generates. The :current_state: flag in the * SQL **
 * Flags drawer selects that shape. Three suppressions keep the generated
 * DDL at exactly the constraints the hand-written table has:
 * :skip_uuid_check: on account_id drops the nil-UUID check,
 * :skip_check: on the account foreign key drops the account existence
 * check, and the * SQL ** Indexes drawer restates the three hand-written
 * indexes so none is lost.
 */

create table if not exists "ores_iam_login_info_tbl" (
    "account_id" uuid not null,
    "tenant_id" uuid not null,
    "last_ip" inet not null,
    "last_attempt_ip" inet not null,
    "failed_logins" integer not null,
    "locked" integer not null,
    "last_login" timestamp with time zone not null,
    "online" integer not null,
    "password_reset_required" integer not null default 0,
    primary key (account_id)
);



create index if not exists login_info_tenant_idx
on "ores_iam_login_info_tbl" (tenant_id);

create index if not exists login_info_account_id_idx
on "ores_iam_login_info_tbl" (account_id);

create index if not exists login_info_locked_idx
on "ores_iam_login_info_tbl" (locked)
where locked = 0;

create or replace function ores_iam_login_info_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_iam_login_info_insert_trg
before insert on "ores_iam_login_info_tbl"
for each row execute function ores_iam_login_info_insert_fn();

