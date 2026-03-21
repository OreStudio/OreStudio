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

-- =============================================================================
-- Compute Grid Tables
-- =============================================================================
-- BOINC-inspired distributed compute grid: hosts, apps, workunits, results,
-- and batch orchestration.

\ir ./compute_apps_create.sql
\ir ./compute_apps_notify_trigger_create.sql
\ir ./compute_app_versions_create.sql
\ir ./compute_app_versions_notify_trigger_create.sql
\ir ./compute_hosts_create.sql
\ir ./compute_hosts_notify_trigger_create.sql
\ir ./compute_batches_create.sql
\ir ./compute_batches_notify_trigger_create.sql
\ir ./compute_batch_dependencies_create.sql
\ir ./compute_workunits_create.sql
\ir ./compute_workunits_notify_trigger_create.sql
\ir ./compute_results_create.sql
\ir ./compute_results_notify_trigger_create.sql
\ir ./compute_grid_samples_create.sql
\ir ./compute_node_samples_create.sql
