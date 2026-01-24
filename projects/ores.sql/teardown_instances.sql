/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Instance Database Teardown
 *
 * This file contains explicit teardown sequences for instance databases.
 * It is included by teardown_all.sql during Step 2.
 *
 * For each instance database, this script:
 *   1. Connects to the database
 *   2. Runs drop/drop.sql to drop all schema objects (validates drop logic)
 *   3. Connects back to postgres
 *   4. Drops the database
 *
 * USAGE:
 *   For production: Generate this file using the admin script, then review
 *   and commit before running teardown_all.sql.
 *
 *   psql -U postgres -f admin/admin_teardown_instances_generate.sql
 *   cat teardown_instances.sql   # Review
 *   git add teardown_instances.sql && git commit
 *   psql -U postgres -f teardown_all.sql
 *
 *   For development: Leave this file empty (or with only comments) if there
 *   are no instance databases to drop.
 *
 * FORMAT:
 *   Each database should have a complete teardown sequence:
 *
 *   -- Instance: ores_happy_penguin
 *   \echo 'Dropping schema objects in ores_happy_penguin...'
 *   \c ores_happy_penguin
 *   \ir drop/drop.sql
 *   \c postgres
 *   drop database if exists ores_happy_penguin;
 */

-- No instance databases to drop (placeholder).
-- Run admin/admin_teardown_instances_generate.sql to populate this file.
