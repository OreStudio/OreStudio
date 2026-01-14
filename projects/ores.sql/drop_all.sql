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

-- Geo
\ir ./drop/geo_ip2country_drop.sql

-- Data Quality
\ir ./drop/dq_countries_artefact_drop.sql
\ir ./drop/dq_dataset_drop.sql
\ir ./drop/dq_methodology_drop.sql
\ir ./drop/dq_treatment_dimension_drop.sql
\ir ./drop/dq_nature_dimension_drop.sql
\ir ./drop/dq_origin_dimension_drop.sql
\ir ./drop/dq_subject_area_drop.sql
\ir ./drop/dq_data_domain_drop.sql

-- Assets
\ir ./drop/assets_images_functions_drop.sql
\ir ./drop/assets_image_tags_drop.sql
\ir ./drop/assets_tags_drop.sql
\ir ./drop/assets_images_drop.sql

-- Telemetry
\ir ./drop/telemetry_stats_functions_drop.sql
\ir ./drop/telemetry_logs_drop.sql

-- IAM RBAC
\ir ./drop/iam_rbac_functions_drop.sql
\ir ./drop/iam_account_roles_drop.sql
\ir ./drop/iam_role_permissions_drop.sql
\ir ./drop/iam_roles_notify_trigger_drop.sql
\ir ./drop/iam_roles_drop.sql
\ir ./drop/iam_permissions_notify_trigger_drop.sql
\ir ./drop/iam_permissions_drop.sql

-- IAM sessions and login
\ir ./drop/iam_session_stats_drop.sql
\ir ./drop/iam_sessions_drop.sql
\ir ./drop/iam_login_info_drop.sql

-- IAM accounts
\ir ./drop/iam_accounts_notify_trigger_drop.sql
\ir ./drop/iam_accounts_drop.sql

-- Variability
\ir ./drop/variability_feature_flags_notify_trigger_drop.sql
\ir ./drop/variability_feature_flags_drop.sql

-- Refdata
\ir ./drop/refdata_currencies_notify_trigger_drop.sql
\ir ./drop/refdata_currencies_drop.sql
\ir ./drop/refdata_countries_notify_trigger_drop.sql
\ir ./drop/refdata_countries_drop.sql
\ir ./drop/dq_change_reasons_notify_trigger_drop.sql
\ir ./drop/dq_change_reason_functions_drop.sql
\ir ./drop/dq_change_reasons_drop.sql
\ir ./drop/dq_change_reason_categories_notify_trigger_drop.sql
\ir ./drop/dq_change_reason_categories_drop.sql

-- Utility
\ir ./drop/utility_functions_drop.sql
