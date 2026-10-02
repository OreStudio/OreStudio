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
 * One-shot migration: link the pricing model configuration to its report
 * configuration
 *
 * The credit simulation, stress test and today's market configurations each
 * carry a configuration_id to the reporting configuration they are registered
 * as, which is how a report reaches them. The pricing model configuration now
 * carries it too. The column is nullable, because a document can be imported
 * before it is registered, so existing rows need no backfill: none of them is
 * registered yet. The insert trigger is refreshed so that a new row's link is
 * checked.
 *
 * On a freshly recreated database the create script already emits the column
 * and this migration is unnecessary. It exists for databases created before
 * the change.
 */

alter table ores_analytics_pricing_model_configs_tbl
    add column if not exists "configuration_id" uuid null;

-- The insert trigger checks the new column against the reporting
-- configurations. The create script is safe to run on an existing table: it
-- creates nothing that exists and replaces the trigger function, so including
-- it installs the check without copying the generated function here.
\ir ../create/analytics/analytics_pricing_model_configs_create.sql
