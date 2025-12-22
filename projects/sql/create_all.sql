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
\ir ./create_database.sql
\ir ./currencies_create.sql
\ir ./currencies_notify_trigger.sql
\ir ./accounts_create.sql
\ir ./accounts_notify_trigger.sql
\ir ./feature_flags_create.sql
\ir ./login_info_create.sql
\ir ./permissions_create.sql
\ir ./roles_create.sql
\ir ./role_permissions_create.sql
\ir ./account_roles_create.sql
\ir ./rbac_functions_create.sql
\ir ./images_create.sql
\ir ./tags_create.sql
\ir ./image_tags_create.sql
\ir ./currency_images_create.sql

\ir ./bootstrap_mode_setup.sql
\ir ./disable_password_validation_setup.sql

-- Populate reference data
\ir ./load_flags.sql
\ir ./flags_populate.sql
\ir ./currencies_populate.sql
\ir ./currency_images_populate.sql
