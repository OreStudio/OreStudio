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
 * CRM Topology Bundles Seed Population Script
 *
 * Registers the refdata.crm_topology_bundles dataset (a second member
 * of the marketdata.reference_vintage_2016_02_05 bundle, alongside
 * marketdata.fx_driver_rates -- so a party gets both in the same atomic
 * publish, per the CRM story's own architecture decision that a party's
 * CRM topology and the feed data it depends on must never be published
 * out of step) and seeds two named CRMs per party, matching real FX
 * desk tiering:
 *
 * - "majors": pivot USD, the 8 major driver pairs as spanning-tree
 *   edges, plus a curated set of enabled derived (non-edge) crosses
 *   commonly requested on demand (EUR/GBP, EUR/JPY, GBP/JPY).
 * - "exotics": pivot USD, the 3 EM driver pairs (ZAR/MXN/INR) as
 *   spanning-tree edges -- a pure USD-pivot star, no derived pairs
 *   needed since every configured exotic pair already is a driver edge.
 *
 * Explicit dataset dependencies (ores_dq_dataset_dependencies_upsert_fn)
 * record that this dataset presupposes marketdata.fx_driver_rates (the
 * currencies) and synthetic.fx_spot_configs.realistic (the live feeds
 * that give those currencies fresh, not just seeded, rates) -- currently
 * documentation/introspection only (the bundle publish workflow orders
 * by each bundle member's own display_order, not by walking this
 * dependency graph), but still real, queryable metadata, and this
 * dataset's own member position (order 2, after fx_driver_rates at
 * order 1) in marketdata.reference_vintage_2016_02_05 achieves the
 * actual ordering that matters for this specific bundle today.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Methodology Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_methodologies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'CRM Topology Curation',
        'Named CRM (Cross-Rates Matrix) topologies curated to mirror real FX desk liquidity tiering: a dense direct-quote "majors" mesh for liquid G10 pairs, and a strict USD-pivot-star "exotics" tier for thin-liquidity EM pairs where no one directly quotes cross rates. Driver pairs are chosen to exactly match currencies with real, live synthetic feed data (see marketdata.fx_driver_rates and synthetic.fx_spot_configs.realistic) -- never a CRM edge with no underlying feed.',
        'ores.analytics.quant CRM design (see doc/agile/versions/v0/sprint_23/crm_implementation/), cross-referenced against marketdata.fx_driver_rates and synthetic.fx_spot_configs.realistic for feed coverage.',
        'Every driver pair here has a corresponding row in both marketdata.fx_driver_rates and synthetic.fx_spot_configs.realistic; derived pairs are a curated subset of pairs triangulable from the majors driver set.'
    );
END $$;

-- =============================================================================
-- Catalog Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_catalogs_upsert_fn(ores_utility_system_tenant_id_fn(),
        'CRM Topology Bundles',
        'Curated Cross-Rates Matrix topology configs (named CRMs, driver pairs, curated derived pairs) for party provisioning.',
        'OreStudio Development Team'
    );
END $$;

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.crm_topology_bundles',
        'CRM Topology Bundles',
        'FX Spot',
        'Market Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'CRM Topology Curation',
        'CRM Topology Bundles: Majors + Exotics',
        'Two named CRM topologies per party: "majors" (8 G10 driver pairs + curated derived crosses) and "exotics" (3 EM driver pairs, USD-pivot star).',
        'ORESTUDIO',
        'Seed data for the CRM Topology Bundles Librarian bundle',
        current_date,
        'Internal Use Only',
        'crm_topology_bundles'
    );
END $$;

-- =============================================================================
-- Dataset Dependencies
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.crm_topology_bundles',
        'marketdata.fx_driver_rates',
        'driver_currencies'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.crm_topology_bundles',
        'synthetic.fx_spot_configs.realistic',
        'live_feed'
    );
END $$;

-- =============================================================================
-- Bundle Membership
--
-- Second member of marketdata.reference_vintage_2016_02_05, after
-- marketdata.fx_driver_rates (order 1) -- same atomic publish, so a
-- party's CRM topology and the driver-rate data it depends on are never
-- published out of step.
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(),
        'marketdata.reference_vintage_2016_02_05',
        'refdata.crm_topology_bundles',
        2,
        false
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

DO $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.crm_topology_bundles'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.crm_topology_bundles';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_crm_topology_bundles_artefact_tbl
    where dataset_id = v_dataset_id;

    raise debug 'Populating CRM topology bundles for dataset: refdata.crm_topology_bundles';

    insert into ores_dq_crm_topology_bundles_artefact_tbl (
        dataset_id, tenant_id, crm_name, pivot_currency_code,
        base_currency_code, quote_currency_code, row_kind
    )
    values
        -- majors: 8 driver pairs (spanning-tree edges), matching
        -- marketdata.fx_driver_rates / synthetic.fx_spot_configs.realistic
        -- exactly.
        (v_dataset_id, v_tenant_id, 'majors', 'USD', 'EUR', 'USD', 'driver'),
        (v_dataset_id, v_tenant_id, 'majors', 'USD', 'GBP', 'USD', 'driver'),
        (v_dataset_id, v_tenant_id, 'majors', 'USD', 'USD', 'CHF', 'driver'),
        (v_dataset_id, v_tenant_id, 'majors', 'USD', 'USD', 'JPY', 'driver'),
        (v_dataset_id, v_tenant_id, 'majors', 'USD', 'USD', 'SEK', 'driver'),
        (v_dataset_id, v_tenant_id, 'majors', 'USD', 'AUD', 'USD', 'driver'),
        (v_dataset_id, v_tenant_id, 'majors', 'USD', 'USD', 'CAD', 'driver'),
        (v_dataset_id, v_tenant_id, 'majors', 'USD', 'NZD', 'USD', 'driver'),
        -- majors: curated derived (non-edge) crosses commonly requested
        -- on demand -- all triangulable from the 8 driver edges above.
        (v_dataset_id, v_tenant_id, 'majors', 'USD', 'EUR', 'GBP', 'derived'),
        (v_dataset_id, v_tenant_id, 'majors', 'USD', 'EUR', 'JPY', 'derived'),
        (v_dataset_id, v_tenant_id, 'majors', 'USD', 'GBP', 'JPY', 'derived'),
        -- exotics: 3 EM driver pairs, a pure USD-pivot star -- every
        -- configured pair already is a driver edge, no derived pairs.
        (v_dataset_id, v_tenant_id, 'exotics', 'USD', 'USD', 'ZAR', 'driver'),
        (v_dataset_id, v_tenant_id, 'exotics', 'USD', 'USD', 'MXN', 'driver'),
        (v_dataset_id, v_tenant_id, 'exotics', 'USD', 'USD', 'INR', 'driver');

    get diagnostics v_count = row_count;
    raise debug 'Populated % CRM topology bundle row(s)', v_count;
end $$;
