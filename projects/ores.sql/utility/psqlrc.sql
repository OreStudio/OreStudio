/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 * ORES psqlrc - Custom psql configuration for ORE Studio databases.
 *
 * USAGE:
 *   Load in existing session:  \i /path/to/psqlrc.sql
 *   Set as default:            export PSQLRC=/path/to/psqlrc.sql
 *   Or symlink:                ln -s /path/to/psqlrc.sql ~/.psqlrc
 */

-- Suppress output during setup
\set QUIET 1

--------------------------------------------------------------------------------
-- Display Settings
--------------------------------------------------------------------------------

-- Display null values clearly
\pset null '[null]'

-- Informative prompt: [user@host:port (time) database transaction_status]$
\set PROMPT1 '[%n@%M:%> (%`date +%H:%M:%S`) %/ %x]$ '
\set PROMPT2 '    -> '

-- Disable pager for easier scripting
\pset pager off

-- Show query execution time
\timing

--------------------------------------------------------------------------------
-- History Settings
--------------------------------------------------------------------------------

-- Separate history file per database and host
\set HISTFILE ~/.psql_history- :HOST - :DBNAME

-- Keep more history
\set HISTSIZE 5000

-- Ignore duplicate commands and commands starting with space
\set HISTCONTROL ignoreboth

--------------------------------------------------------------------------------
-- Search Path
--------------------------------------------------------------------------------

-- Set ORES schemas on the search path
SET search_path TO production, metadata, public;

--------------------------------------------------------------------------------
-- Useful Macros (invoke with :macroname)
--------------------------------------------------------------------------------

-- Table sizes (human readable)
\set tsize 'SELECT table_schema, table_name, pg_size_pretty(pg_relation_size(quote_ident(table_schema) || \'.\' || quote_ident(table_name))) AS size, pg_size_pretty(pg_total_relation_size(quote_ident(table_schema) || \'.\' || quote_ident(table_name))) AS total_size FROM information_schema.tables WHERE table_type = \'BASE TABLE\' AND table_schema IN (\'production\', \'metadata\') ORDER BY pg_relation_size(quote_ident(table_schema) || \'.\' || quote_ident(table_name)) DESC;'

-- List ORES databases
\set ores_dbs 'SELECT datname AS database, pg_size_pretty(pg_database_size(datname)) AS size FROM pg_database WHERE datname LIKE \'ores_%\' ORDER BY datname;'

-- List ORES roles and their memberships
\set ores_roles 'SELECT r.rolname AS role, r.rolcanlogin AS can_login, COALESCE(string_agg(m.rolname, \', \'), \'\') AS member_of FROM pg_roles r LEFT JOIN pg_auth_members am ON r.oid = am.member LEFT JOIN pg_roles m ON am.roleid = m.oid WHERE r.rolname LIKE \'ores_%\' GROUP BY r.rolname, r.rolcanlogin ORDER BY r.rolname;'

-- Show current connection info
\set conninfo 'SELECT current_user, session_user, current_database(), inet_server_addr() AS server, inet_server_port() AS port;'

-- Count rows in all ORES tables
\set row_counts 'SELECT schemaname, relname AS table_name, n_live_tup AS row_count FROM pg_stat_user_tables WHERE schemaname IN (\'production\', \'metadata\') ORDER BY n_live_tup DESC;'

-- Show active connections
\set connections 'SELECT pid, usename, datname, client_addr, state, query_start, left(query, 60) AS query FROM pg_stat_activity WHERE datname LIKE \'ores_%\' ORDER BY query_start;'

-- Show locks
\set locks 'SELECT pid, usename, pg_blocking_pids(pid) AS blocked_by, query FROM pg_stat_activity WHERE cardinality(pg_blocking_pids(pid)) > 0;'

-- Terminate all other connections to the current database (useful before DROP DATABASE)
\set kill_connections 'SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = current_database() AND pid <> pg_backend_pid();'

-- Reload this psqlrc (assumes current directory is ores.sql)
\set reload '\\i utility/psqlrc.sql'

--------------------------------------------------------------------------------
-- Finish Setup
--------------------------------------------------------------------------------

\unset QUIET

\echo ''
\echo 'ORES psqlrc loaded. Available macros:'
\echo '  :tsize            - Table sizes (human readable)'
\echo '  :ores_dbs         - List ORES databases'
\echo '  :ores_roles       - List ORES roles'
\echo '  :conninfo         - Current connection info'
\echo '  :row_counts       - Row counts per table'
\echo '  :connections      - Active connections'
\echo '  :locks            - Show blocked queries'
\echo '  :kill_connections - Terminate other connections to current DB'
\echo '  :reload           - Reload psqlrc (from ores.sql dir)'
\echo ''
