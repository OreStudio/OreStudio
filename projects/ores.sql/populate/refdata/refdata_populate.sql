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
 * Refdata Component Population Script
 *
 * Seeds DQ artefact/dataset data for curated (non-ISO) reference data, so
 * it can be published from the Librarian into a party. All scripts are
 * idempotent.
 */

\echo '=== Refdata Component Population ==='
\echo ''

\echo '--- Currency Pairs Seed Data ---'
\ir ./refdata_currency_pairs_seed_populate.sql

\echo ''
\echo '--- Currency Pair Conventions Seed Data ---'
\ir ./refdata_currency_pair_conventions_seed_populate.sql

\echo ''
\echo '=== Refdata Component Population Complete ==='
