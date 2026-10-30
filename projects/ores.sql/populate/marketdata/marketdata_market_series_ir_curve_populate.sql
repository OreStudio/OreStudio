
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
 * Demo IR Curve Market Series Population Script
 *
 * Seeds the two market_series catalog rows the demo's USD SOFR curve needs, and
 * gives both a fixed uuid so the seed that uses them can refer to them:
 *
 *  - The bootstrapped curve the republish service writes into:
 *    oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR-FOMC.
 *    The series starts OBSERVED and is claimed -- stamped IR_CURVE_BOOTSTRAP with
 *    the config's id and version -- by the republish service on its first run,
 *    which is why the seed writes the sentinel (nil config id, version 0) rather
 *    than the derived shape directly.
 *
 *  - The ladder the curve's quotes come from: the spot-starting USD SOFR swap
 *    quote the synthetic Bloomberg feed publishes, one datum per pillar, each
 *    datum carrying its pillar's term. Every pillar of the blueprint names this
 *    one series and finds its own rung by its end tenor, which is what
 *    [[id:7AE57098-08D4-4D4A-83D6-918FE520B68E][A pillar names the market
 *    series it reads]] decides. The row is seeded rather than left to the ingest
 *    loop so the blueprint's pillars have a series to name on a fresh database;
 *    the loop finds this row by the same (identity, provider, party) key and
 *    writes its ticks into it instead of creating a second one.
 *
 * The provider is bloomberg, the provider of the synthetic feed that publishes
 * the pillars, and the party is the system party: this script also gives that
 * party the subscription and the route to bloomberg-a, so the ingest stores the
 * feed's ticks into the ladder series.
 *
 * Both rows carry a junction row in
 * ores_marketdata_market_series_asset_classes_tbl, since the asset class is no
 * longer a column on the series row.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO NOTHING (a rerun must
 * not reset a row the republish service has already stamped). A database built
 * before the curve's spelling changed keeps the old rows and a null identity:
 * the schema is applied by recreation, so a rebuild is the migration.
 */

\echo '--- Demo IR Curve Market Series ---'

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, series_subclass, oresmd_uri,
    provider_code, derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (
        'f2d3e4a5-6c7d-4e8f-8a9b-1c2d3e4f5061',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR-FOMC',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped USD SOFR curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        'f7a1b2c3-8d4e-4f60-9a2b-3c4d5e6f7081',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'yield', 'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D',
        'bloomberg', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Spot-starting USD SOFR swap quotes: the ladder the demo blueprint pillars name'
    )
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- The curve points and the quotes they are built from are interest rates
-- series, which the junction carries rather than the series row.
insert into ores_marketdata_market_series_asset_classes_tbl (
    market_series_id, tenant_id, asset_class_code, version,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (
        'f2d3e4a5-6c7d-4e8f-8a9b-1c2d3e4f5061',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'The bootstrapped USD SOFR curve is an interest rates series'
    ),
    (
        'f7a1b2c3-8d4e-4f60-9a2b-3c4d5e6f7081',
        ores_utility_system_tenant_id_fn(),
        'interest_rates', 0,
        current_user, current_user, 'system.initial_load',
        'The spot-starting USD SOFR swap quotes are interest rates series'
    )
on conflict (tenant_id, market_series_id, asset_class_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary.
select 'marketdata_market_series (demo IR curve)' as entity, count(*) as count
from ores_marketdata_market_series_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and oresmd_uri in (
      'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR-FOMC',
      'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D')
  and valid_to = ores_utility_infinity_timestamp_fn();

\echo '--- The System Party Reads the Simulated Bloomberg Quotes ---'

-- A series ticks only for a party the ingest finds a route to its source. The ladder above
-- belongs to the system party, so the system party needs the subscription and the route that
-- make the ingest store the synthetic Bloomberg feed's ticks into it. Without these two rows
-- the ladder series holds no point, and the blueprint that reads it refuses every pillar with
-- "no observed rate".
--
-- The subscription is what the demand set is computed from and what entitles the party to read
-- the quotes; the route is what names the source the ingest delivers for. The provider is the
-- ladder's own, bloomberg, and the source is the adapter the simulator runs for it, bloomberg-a.
insert into ores_marketdata_subscriptions_tbl (
    id, tenant_id, version, party_id, provider_code, display, non_display, derived,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    '5f0e2b7a-9c41-5d63-8a2f-3b6c9d1e4f02',
    ores_utility_system_tenant_id_fn(),
    0,
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'bloomberg', true, true, false,
    current_user, current_user, 'system.initial_load',
    'The system party takes the simulated Bloomberg quotes its curve blueprints read'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_marketdata_routes_tbl (
    id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    '5f0e2b7a-9c41-5d63-8a2f-3b6c9d1e4f03',
    ores_utility_system_tenant_id_fn(),
    0,
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'bloomberg',
    'ir',
    '0a42775e-3da7-4c61-a732-6b62cb9f948f',
    current_user, current_user, 'system.initial_load',
    'The system party reads the simulated Bloomberg quotes through bloomberg-a'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary.
select 'marketdata_routes (system party)' as entity, count(*) as count
from ores_marketdata_routes_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and party_id = ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn())
  and publisher_code = 'bloomberg'
  and valid_to = ores_utility_infinity_timestamp_fn();
