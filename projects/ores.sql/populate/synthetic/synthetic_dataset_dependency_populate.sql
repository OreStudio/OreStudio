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
 * Synthetic Dataset Dependencies
 *
 * Any synthetic config dataset seeded with price_source='vintage' depends on the marketdata
 * dataset that actually carries the real observations it resolves against at feed-start time --
 * without this, publication_service's dependency-graph resolution (see
 * publication_service.hpp's own doc comment; on by default) has nothing to walk, so a bundle
 * publish can seed the config rows while leaving the party with no matching
 * market_observation data, and every vintage-mode feed then fails to start with "No vintage
 * data found" -- exactly the failure mode this dependency exists to prevent.
 *
 * This script is idempotent.
 */

DO $$
BEGIN
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.fx_spot_configs.ore_samples_2016',
        'marketdata.fx_driver_rates',
        'vintage_source'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.fx_spot_configs.uniform_demo',
        'marketdata.fx_driver_rates',
        'vintage_source'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.fx_spot_configs.realistic_2026',
        'marketdata.fx_driver_rates_2026',
        'vintage_source'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.ir_curve_configs.ore_samples_2016',
        'marketdata.ir_deposit_rates',
        'vintage_source'
    );
END $$;
