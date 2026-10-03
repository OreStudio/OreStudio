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
 * One-shot migration: the default curve section
 *
 * Adds the tables that hold the default curve entries of a curve configuration
 * and their configurations, and lets a quote and a bootstrap configuration
 * belong to a default curve configuration.
 *
 * Run after refdata_curve_inflation_migration.sql. Every statement adds a
 * column if it is missing, or creates or replaces, so a database that already
 * holds these objects is left as it is.
 */

begin;

alter table ores_refdata_curve_quotes_tbl
    add column if not exists "default_curve_configuration_id" uuid;
alter table ores_refdata_curve_bootstrap_configs_tbl
    add column if not exists "default_curve_configuration_id" uuid;

\ir ../create/refdata/refdata_default_curves_create.sql
\ir ../create/refdata/refdata_default_curves_notify_trigger_create.sql
\ir ../create/refdata/refdata_default_curve_configurations_create.sql
\ir ../create/refdata/refdata_default_curve_configurations_notify_trigger_create.sql
\ir ../create/refdata/refdata_curve_quotes_create.sql
\ir ../create/refdata/refdata_curve_bootstrap_configs_create.sql
\ir ../create/refdata/refdata_rls_policies_create.sql

\ir ../populate/iam/iam_permissions_populate.sql

commit;
