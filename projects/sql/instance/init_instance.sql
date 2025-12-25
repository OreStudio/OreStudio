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
 * Instance Initialization Script
 *
 * This script initializes instance-specific data that should NOT be
 * part of the template. Each new database instance needs to run this
 * after creation.
 *
 * Includes:
 * - Bootstrap mode feature flag (instance-specific state)
 * - Password validation flag (development/production config)
 *
 * This script is idempotent and can be safely run multiple times.
 */

\ir ../bootstrap_mode_setup.sql
\ir ../disable_password_validation_setup.sql
