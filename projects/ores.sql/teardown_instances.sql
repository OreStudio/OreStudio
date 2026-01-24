/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Instance Database Teardown
 *
 * This file contains explicit DROP DATABASE statements for instance databases.
 * It is included by teardown_all.sql during Step 2.
 *
 * USAGE:
 *   For production: Generate this file using the admin script, then review
 *   and commit before running teardown_all.sql.
 *
 *   psql -U postgres -f admin/generate_teardown_instances.sql
 *   cat teardown_instances.sql   # Review
 *   git add teardown_instances.sql && git commit
 *   psql -U postgres -f teardown_all.sql
 *
 *   For development: Leave this file empty (or with only comments) if there
 *   are no instance databases to drop.
 *
 * FORMAT:
 *   Each database should have an explicit drop statement:
 *
 *   -- Instance: ores_happy_penguin
 *   drop database if exists ores_happy_penguin;
 *
 *   -- Instance: ores_dancing_fox
 *   drop database if exists ores_dancing_fox;
 */

-- No instance databases to drop (placeholder).
-- Run admin/generate_teardown_instances.sql to populate this file.
