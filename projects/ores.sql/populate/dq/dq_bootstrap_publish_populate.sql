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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * Publishes the platform and world bundles into the system tenant.
 *
 * This runs last, once every artefact row is staged, so a database carries its
 * canonical reference data the moment it comes up: no test and no operator has
 * to provision a tenant or publish anything before reading a currency.
 *
 * The two bundles are published in one run so that the dependency edges between
 * them order the steps: a world dataset may depend on a platform dataset, such
 * as the coding schemes, and the closure sorts the pair together.
 */

\echo ''
\echo '--- Publishing the platform and world levels ---'
select dataset_code, action, record_count
from ores_dq_bootstrap_publish_fn(array['system_core', 'world'])
where record_count > 0;
