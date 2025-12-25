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
 * Template Schema Creation Script
 *
 * Creates the complete ORES database schema including:
 * - Schema and extensions
 * - Utility functions
 * - All tables with temporal/bitemporal support
 * - Triggers and notification functions
 * - Reference data (currencies, flags)
 *
 * This script is used by both:
 * - setup_template.sql (to create the template database)
 * - create_database_direct.sql (to create a standalone database)
 *
 * NOTE: This does NOT include instance-specific initialization like
 * feature flags. Those are in instance/init_instance.sql
 */

-- Create schema and extensions
create schema if not exists ores;
create extension if not exists btree_gist;

-- Utility functions
\ir ../utility_functions_create.sql
\ir ../whimsical_names_create.sql
\ir ./database_functions.sql

-- Core tables
\ir ../currencies_create.sql
\ir ../currencies_notify_trigger.sql
\ir ../accounts_create.sql
\ir ../accounts_notify_trigger.sql
\ir ../feature_flags_create.sql
\ir ../login_info_create.sql
\ir ../sessions_create.sql
-- \ir ../session_stats_create.sql -- FIXME: disabled until we sort out timescaledb

-- RBAC tables
\ir ../permissions_create.sql
\ir ../roles_create.sql
\ir ../role_permissions_create.sql
\ir ../account_roles_create.sql
\ir ../rbac_functions_create.sql

-- Image/asset tables
\ir ../images_create.sql
\ir ../tags_create.sql
\ir ../image_tags_create.sql
\ir ../currency_images_create.sql

-- Reference data (immutable, belongs in template)
\ir ../load_flags.sql
\ir ../flags_populate.sql
\ir ../currencies_populate.sql
\ir ../currency_images_populate.sql
