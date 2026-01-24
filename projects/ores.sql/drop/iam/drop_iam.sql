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

-- RBAC
\ir ./iam_rbac_functions_drop.sql
\ir ./iam_account_roles_drop.sql
\ir ./iam_role_permissions_drop.sql
\ir ./iam_roles_notify_trigger_drop.sql
\ir ./iam_roles_drop.sql
\ir ./iam_permissions_notify_trigger_drop.sql
\ir ./iam_permissions_drop.sql

-- Sessions and login
\ir ./iam_session_stats_drop.sql
\ir ./iam_sessions_drop.sql
\ir ./iam_login_info_drop.sql

-- Accounts
\ir ./iam_accounts_notify_trigger_drop.sql
\ir ./iam_accounts_drop.sql

-- Population functions
\ir ./iam_population_functions_drop.sql
