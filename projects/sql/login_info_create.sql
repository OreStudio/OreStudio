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
create schema if not exists oresdb;
create extension if not exists btree_gist;
set schema 'oresdb';

--
-- login_info table tracks login security information for accounts.
-- note: this is a current-state table without temporal versioning.
--
create table if not exists "oresdb"."login_info" (
    "account_id" uuid not null,
    "last_ip" inet not null,
    "last_attempt_ip" inet not null,
    "failed_logins" integer not null,
    "locked" boolean not null,
    "last_login" timestamp with time zone not null,
    "online" boolean not null,
    primary key (account_id)
);

create index if not exists login_info_account_id_idx
on "oresdb"."login_info" (account_id);

create index if not exists login_info_locked_idx
on "oresdb"."login_info" (locked)
where locked = false;
