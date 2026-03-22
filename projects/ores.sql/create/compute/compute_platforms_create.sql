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

-- Reference table: known compute platform identifiers
create table if not exists "ores_compute_platforms_tbl" (
    "code"        text primary key,
    "description" text not null default ''
);

-- Seed the known platforms
insert into ores_compute_platforms_tbl (code, description) values
    ('linux-x86_64',   'Linux x86-64'),
    ('linux-arm64',    'Linux ARM64'),
    ('windows-x86_64', 'Windows x86-64'),
    ('macos-arm64',    'macOS Apple Silicon')
on conflict (code) do nothing;

-- Junction table: supported platforms per app version
create table if not exists "ores_compute_app_version_platforms_tbl" (
    "app_version_id" uuid not null,
    "platform_code"  text not null references ores_compute_platforms_tbl(code),
    primary key (app_version_id, platform_code)
);
