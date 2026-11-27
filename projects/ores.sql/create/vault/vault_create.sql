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
-- Vault Tables
-- =============================================================================
-- The secrets ORE must present to another party, the consumers it seals them
-- to, and the record of each act on one. All three are tenant scoped; the
-- consumers come first, because a secret names the consumer that may resolve
-- it, and the uses come last, because a use describes an act on a secret.

\ir ./vault_consumers_create.sql
\ir ./vault_consumers_notify_trigger_create.sql

\ir ./vault_secrets_create.sql
\ir ./vault_secrets_notify_trigger_create.sql

\ir ./vault_secret_uses_create.sql
\ir ./vault_secret_uses_notify_trigger_create.sql
