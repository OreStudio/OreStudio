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

    -- Each entity's netting data publishes in dependency order: an agreement
    -- needs its counterparty (a bank from the GLEIF set, or another ACME
    -- entity), a netting set needs its agreement, a CSA needs its set.
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_group.netting_agreements', 'gleif.lei_counterparties.small', 'counterparty_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_group.netting_agreements', 'acme.lei_counterparties', 'counterparty_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_group.netting_sets', 'acme.acme_group.netting_agreements', 'agreement_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_group.csas', 'acme.acme_group.netting_sets', 'netting_set_reference');

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_uk.netting_agreements', 'gleif.lei_counterparties.small', 'counterparty_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_uk.netting_agreements', 'acme.lei_counterparties', 'counterparty_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_uk.netting_sets', 'acme.acme_uk.netting_agreements', 'agreement_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_uk.csas', 'acme.acme_uk.netting_sets', 'netting_set_reference');

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_us.netting_agreements', 'gleif.lei_counterparties.small', 'counterparty_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_us.netting_agreements', 'acme.lei_counterparties', 'counterparty_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_us.netting_sets', 'acme.acme_us.netting_agreements', 'agreement_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_us.csas', 'acme.acme_us.netting_sets', 'netting_set_reference');

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_hk.netting_agreements', 'gleif.lei_counterparties.small', 'counterparty_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_hk.netting_agreements', 'acme.lei_counterparties', 'counterparty_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_hk.netting_sets', 'acme.acme_hk.netting_agreements', 'agreement_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme.acme_hk.csas', 'acme.acme_hk.netting_sets', 'netting_set_reference');

    -- The service accounts' images ride assets.system_avatars, and the
    -- provisioning attaches them by code. Every ACME dataset declares the
    -- dependency, so publishing ACME data publishes the images first.
    PERFORM ores_dq_dataset_dependencies_upsert_fn(
        ores_utility_system_tenant_id_fn(), v.code, 'assets.system_avatars', 'visual_assets')
    FROM (VALUES
        ('acme.acme_group.account_contact_informations'),
        ('acme.acme_group.accounts'),
        ('acme.acme_group.books'),
        ('acme.acme_group.business_units'),
        ('acme.acme_group.portfolios'),
        ('acme.acme_hk.account_contact_informations'),
        ('acme.acme_hk.accounts'),
        ('acme.acme_hk.books'),
        ('acme.acme_hk.business_units'),
        ('acme.acme_hk.portfolios'),
        ('acme.acme_uk.account_contact_informations'),
        ('acme.acme_uk.accounts'),
        ('acme.acme_uk.books'),
        ('acme.acme_uk.business_units'),
        ('acme.acme_uk.portfolios'),
        ('acme.acme_us.account_contact_informations'),
        ('acme.acme_us.accounts'),
        ('acme.acme_us.books'),
        ('acme.acme_us.business_units'),
        ('acme.acme_us.portfolios'),
        ('acme.lei_entities'),
        ('acme.lei_parties'),
        ('acme.lei_relationships')
    ) AS v(code);
END $$;
