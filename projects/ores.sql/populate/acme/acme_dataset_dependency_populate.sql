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
 * Acme Corporation Dataset Dependencies
 *
 * Portfolios resolve owner_unit_id by joining the *already-published*
 * business_units table for the same party (see
 * ores_refdata_publish_portfolios_from_dq_fn's bu_ref_map); books resolve
 * their (inherited) owner_unit_id the same way via their parent portfolio.
 * Without an explicit dependency here, publication_service's dependency-graph
 * resolution has nothing to walk for these datasets, so a bundle publish can
 * order portfolios before business_units (or books before portfolios) are
 * committed, silently leaving owner_unit_id null on every portfolio and book
 * -- exactly the failure mode this dependency exists to prevent.
 *
 * This script is idempotent.
 */

DO $$
BEGIN
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_group.portfolios', 'acme.acme_group.business_units', 'owner_unit_source');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_group.books', 'acme.acme_group.portfolios', 'parent_portfolio_source');

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_uk.portfolios', 'acme.acme_uk.business_units', 'owner_unit_source');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_uk.books', 'acme.acme_uk.portfolios', 'parent_portfolio_source');

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_us.portfolios', 'acme.acme_us.business_units', 'owner_unit_source');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_us.books', 'acme.acme_us.portfolios', 'parent_portfolio_source');

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_hk.portfolios', 'acme.acme_hk.business_units', 'owner_unit_source');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_hk.books', 'acme.acme_hk.portfolios', 'parent_portfolio_source');
END $$;
