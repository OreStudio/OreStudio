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
 * FOMC Segment Market Series Population Script
 *
 * Seeds the market_series catalog row for the bootstrapped USD SOFR curve's
 * FOMC-dated short end, with a fixed uuid the bootstrap config references as
 * its output_series_id (see refdata_ir_curve_bootstrap_configs_populate.sql):
 *
 *  - The bootstrapped curve the republish service writes into:
 *    DISCOUNT / RATE / 'USD/USD-SOFR-FOMC' -- ORE's own spelling for a
 *    discount curve's points (DISCOUNT/RATE/CCY/CURVE/TENOR, the corpus
 *    writes USD-DUMMY), which is what curve_republish_service publishes
 *    per point (point_id = pillar end tenor code, value = discount
 *    factor). The row's oresmd_uri is the key without its maturity, the
 *    series the pillars' points hang off. The series starts OBSERVED and
 *    is claimed -- stamped IR_CURVE_BOOTSTRAP with the config's id and
 *    version -- by the republish service on its first run, which is why
 *    the seed writes the sentinel (nil config id, version 0) rather than
 *    the derived shape directly.
 *
 * There is no raw grid row. The synthetic FOMC feed publishes one series per
 * pillar, each keyed as the meeting-dated OIS quote it simulates, so the
 * bootstrap reads a pillar from the series that pillar's own ORE key names
 * and no fixed grid id is needed to catch the feed's ticks.
 *
 * The row belongs to the system party, matching the party the synthetic
 * dataset publishes into and therefore the party_id on the feed's ticks.
 *
 * It also carries a junction row in
 * ores_marketdata_market_series_asset_classes_tbl, since the asset class is
 * no longer a column on the series row.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO NOTHING (a
 * rerun must not reset a row the republish service has already stamped).
 * A database built before the curve's spelling changed keeps the old row
 * and a null identity: the schema is applied by recreation, so a rebuild
 * is the migration.
 */

\echo '--- FOMC Segment Market Series ---'

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, series_type, metric, qualifier,
    series_subclass, oresmd_uri,
    derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (
        'f2d3e4a5-6c7d-4e8f-8a9b-1c2d3e4f5061',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'DISCOUNT', 'RATE', 'USD/USD-SOFR-FOMC',
        'yield', 'oresmd://ir/usd?curve_id=USD-SOFR-FOMC&type=quote&metric=rate&quote=discount',
        'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped USD SOFR curve (FOMC segment): republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    )
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- The curve belongs to the interest rates asset class, which the junction
-- carries rather than the series row.
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
    )
on conflict (tenant_id, market_series_id, asset_class_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary.
select 'marketdata_market_series (FOMC segment)' as entity, count(*) as count
from ores_marketdata_market_series_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and qualifier = 'USD/USD-SOFR-FOMC'
  and valid_to = ores_utility_infinity_timestamp_fn();
