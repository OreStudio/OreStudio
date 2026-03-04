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
 * PostgreSQL Extensions Setup — postgres database only
 *
 * Previously installed pg_cron here for job scheduling. pg_cron has been
 * removed: ores.scheduler now uses an in-process timer loop (scheduler_loop)
 * instead of relying on the pg_cron extension.
 *
 * This file is retained for consistency with the setup script pipeline but
 * no longer installs any extensions.
 *
 * USAGE:
 *   psql -U postgres -f setup_extensions_postgres.sql
 */

\set ON_ERROR_STOP on

\echo ''
\echo 'No postgres-database extensions required.'
\echo '(pg_cron removed: ores.scheduler now uses in-process timer loop)'
\echo ''
