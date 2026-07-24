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
 * Synthetic Component Population Script
 *
 * Seeds DQ artefact/dataset data for synthetic market data configs, so
 * they can be published from the Librarian into a party. All scripts are
 * idempotent.
 */

\echo '=== Synthetic Component Population ==='
\echo ''

\echo '--- FX Spot Config Seed Data ---'
\ir ./synthetic_fx_spot_configs_populate.sql

\echo '--- FX Spot Config Seed Data: Uniform Volatility Demo ---'
\ir ./synthetic_fx_spot_configs_uniform_demo_populate.sql

\echo '--- FX Spot Config Seed Data: 2016 ORE Samples ---'
\ir ./synthetic_fx_spot_configs_ore_samples_2016_populate.sql

\echo '--- FX Spot Config Seed Data: 2026 Realistic ---'
\ir ./synthetic_fx_spot_configs_realistic_2026_populate.sql

\echo '--- IR Curve Config Seed Data: Uniform Volatility Demo ---'
\ir ./synthetic_ir_curve_configs_uniform_demo_populate.sql

\echo '--- IR Curve Config Seed Data: 2016 ORE Samples (legacy IBOR-era curves) ---'
\ir ./synthetic_ir_curve_configs_ore_samples_2016_populate.sql

\echo '--- IR Curve Config Seed Data: 2026 Realistic (RFR curves) ---'
\ir ./synthetic_ir_curve_configs_realistic_2026_populate.sql

\echo ''
\echo '=== Synthetic Component Population Complete ==='
