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
 * One capacity measurement, written in one place.
 *
 * The database's size is the database's own statement and is read here, so a
 * caller cannot report a size the server disagrees with. The free space is the
 * caller's: only a process on the host can read a filesystem, and the scheduler
 * service is that process. It passes what it measured, and null when it could
 * not measure anything.
 *
 * The row is the installation's, held by the system tenant: the database is one
 * database for every tenant in it, and a tenant that wrote its own capacity row
 * would be reporting a number it does not own.
 */

create or replace function ores_scheduler_record_capacity_fn(
    p_database_name text,
    p_host_name text,
    p_measured_path text,
    p_disk_free_bytes bigint,
    p_disk_total_bytes bigint,
    p_note text default ''
)
returns bigint
language plpgsql
security definer
as $$
declare
    v_database_bytes bigint;
begin
    -- pg_database_size refuses a name the catalogue does not hold, and a job
    -- that named one would fail every firing. The row is written either way,
    -- with the size it could read.
    if exists (select 1 from pg_catalog.pg_database d where d.datname = p_database_name) then
        v_database_bytes := pg_catalog.pg_database_size(p_database_name);
    else
        v_database_bytes := 0;
        p_note := trim(both ' ' from coalesce(p_note, '') ||
            case when coalesce(p_note, '') = '' then '' else '; ' end ||
            'the database ''' || p_database_name || ''' is not on this server');
    end if;

    insert into "ores_scheduler_capacity_samples_tbl"
        ("sampled_at", "tenant_id", "database_name", "host_name", "measured_path",
         "database_bytes", "disk_free_bytes", "disk_total_bytes", "note")
    values
        (now(), ores_utility_system_tenant_id_fn(), p_database_name, p_host_name,
         p_measured_path, v_database_bytes, p_disk_free_bytes, p_disk_total_bytes,
         coalesce(p_note, ''));

    return v_database_bytes;
end;
$$;

comment on function ores_scheduler_record_capacity_fn(text, text, text, bigint, bigint, text) is
'Writes one capacity sample: the database''s own size, and the free and total
 space of the filesystem the caller measured. A free space the caller could not
 read is null, and the note says why: an unknown free space is not an empty
 disk.';
