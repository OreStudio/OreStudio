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
-- Reporting Tables
-- =============================================================================
-- Report definitions (what to run and when) and report instances (each run).
-- Risk report configuration is stored in a dedicated table with analytics flags
-- derived from the ORE ore.xml structure.

-- Enum tables (must precede report_definitions which validates against them)
\ir ./reporting_report_types_create.sql
\ir ./reporting_report_types_notify_trigger_create.sql
\ir ./reporting_concurrency_policies_create.sql
\ir ./reporting_concurrency_policies_notify_trigger_create.sql

\ir ./reporting_report_definitions_create.sql
\ir ./reporting_report_definitions_notify_trigger_create.sql
\ir ./reporting_risk_report_configs_create.sql
\ir ./reporting_risk_report_configs_notify_trigger_create.sql
\ir ./reporting_risk_report_config_portfolios_create.sql
\ir ./reporting_risk_report_config_books_create.sql
\ir ./reporting_report_instances_create.sql
\ir ./reporting_report_instances_notify_trigger_create.sql
