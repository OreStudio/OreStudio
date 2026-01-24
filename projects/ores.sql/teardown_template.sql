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

/**
 * Teardown Template Database
 *
 * Explicitly drops the ores_template database.
 * Must unmark as template first before it can be dropped.
 *
 * USAGE:
 *   psql -U postgres -f teardown_template.sql
 */

\set ON_ERROR_STOP on

\echo ''
\echo 'Dropping ores_template database...'

-- Unmark as template so it can be dropped
update pg_database set datistemplate = false where datname = 'ores_template';

-- Terminate any connections
select pg_terminate_backend(pid)
from pg_stat_activity
where datname = 'ores_template' and pid <> pg_backend_pid();

drop database if exists ores_template;

\echo 'ores_template dropped.'
\echo ''
