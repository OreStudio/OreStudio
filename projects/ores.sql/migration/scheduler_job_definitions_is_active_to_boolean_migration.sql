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
 * One-shot migration: is_active becomes boolean
 *
 * The column was integer with a default of 1 while every consumer
 * already treated it as a flag: the domain struct, the protocol
 * struct, the Qt model and monitor, and the status handler all
 * declare bool. Only the storage side disagreed, and the mapper
 * converted between the two. Boolean is the convention elsewhere in
 * the schema, so the column now matches what the code always meant
 * and the conversion is gone.
 *
 * On a freshly recreated database the create script already emits
 * boolean and this migration is unnecessary. It exists for databases
 * created before the change.
 */

alter table ores_scheduler_job_definitions_tbl
    alter column is_active drop default;

alter table ores_scheduler_job_definitions_tbl
    alter column is_active type boolean
    using (is_active <> 0);

alter table ores_scheduler_job_definitions_tbl
    alter column is_active set default true;
