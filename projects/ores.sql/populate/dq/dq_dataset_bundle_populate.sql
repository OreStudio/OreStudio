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
 * Dataset Bundle Population Script
 *
 * Seeds the database with dataset bundle definitions.
 * Bundles are named collections of datasets designed to work together.
 * This script is idempotent.
 */

DO $$
BEGIN
    -- =============================================================================
    -- Dataset Bundles
    -- =============================================================================

    -- --- Dataset Bundles ---

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'solvaris',
        'Solvaris',
        'Synthetic reference data for development and testing - an isolated fantasy world with its own countries, currencies, and reference data.'
    );

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'base',
        'Base System',
        'Industry-standard reference data (ISO + FpML) for production use. Includes country codes, currency codes, and financial market standards.'
    );

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'crypto',
        'Crypto',
        'Base System plus cryptocurrency reference data. Extends the production dataset with cryptocurrency symbols and icons.'
    );

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'risk_management',
        'Risk Management',
        'Sample organisational and risk-reporting data for development and testing. Includes business units, portfolios, trading books, and the standard set of risk report definitions (NPV, VaR, CVA, XVA, etc.) -- reports reference the book/portfolio tree, so both live in one bundle.'
    );

    -- Each of the three bundles below is a mutually-exclusive synthetic
    -- market-data theme: publishing more than one is fine (a party can make
    -- several available), but only one's feeds should ever run at a time --
    -- see MarketSimulatorWindow's Start-at-Root theme prompt, which refuses
    -- to cascade a start across themes for exactly this reason.

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic_ore_samples_2016',
        'Synthetic Data: 2016 ORE Samples',
        'Pre-benchmark-reform theme pinned to the 2016-02-05 Fed H.10 vintage: calibrated FX spot generation for all 8 major + 3 EM/exotic driver pairs, plus four legacy IBOR-era IR curves (USD-LIBOR-3M, EUR-EURIBOR-3M, GBP-LIBOR-6M, JPY-LIBOR-6M). Matches the conventions ORE''s own sample data comes from.'
    );

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic_realistic_2026',
        'Synthetic Data: 2026 Realistic',
        'Current-regime theme: one per-currency-calibrated overnight-RFR IR curve for each top-20-by-turnover currency, plus calibrated FX spot generation for all 8 major + 3 EM/exotic + 2 Nordic driver pairs, both grounded in the real 2026-05-05 Fed H.10 vintage.'
    );

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic_uniform_demo',
        'Synthetic Data: Uniform Volatility Demo',
        'Not a vintage theme -- a deliberately simple demo/exercise archetype: uniform, exaggerated volatility for both FX (single-component geometric process) and IR (one uniform Vasicek kappa/sigma across all 20 currencies), easy to eyeball and exercises every UI feature.'
    );
END $$;

