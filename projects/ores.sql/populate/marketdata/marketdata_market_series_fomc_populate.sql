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
 * Seeds the two market_series catalog rows for the FOMC-dated short end of
 * the USD SOFR curve, each with a fixed uuid the bootstrap config
 * references (see refdata_ir_curve_bootstrap_configs_populate.sql):
 *
 *  - The raw grid the synthetic feed publishes into: the fixing URI of the
 *    FOMC-dated SOFR segment, 'oresmd://ir/usd?index=sofr&tenor=fomc&type=fixing' —
 *    the identity the ingest loop resolves the ir_curve_feed's ticks by.
 *    Seeding the row means the feed's ticks land in a fixed, known series
 *    id instead of one auto-created at first tick.
 *
 *  - The bootstrapped curve the republish service writes into: the
 *    discount curve URI 'oresmd://ir/usd?index=sofr&tenor=fomc&role=discount&type=curve' —
 *    a discount-factor curve, which is what curve_republish_service
 *    publishes per point (point_id = pillar end tenor code, value =
 *    discount factor). The series starts OBSERVED and is claimed -- stamped
 *    IR_CURVE_BOOTSTRAP with the config's id and version -- by the
 *    republish service on its first run, which is why the seed writes the
 *    sentinel (nil config id, version 0) rather than the derived shape
 *    directly.
 *
 * Both rows belong to the system party, matching the party the synthetic
 * dataset publishes into and therefore the party_id on the feed's ticks.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO NOTHING (a
 * rerun must not reset a row the republish service has already stamped).
 */

\echo '--- FOMC Segment Market Series ---'

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, oresmd_uri,
    derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (
        'e1c2d3f4-5b6c-4d7e-9f8a-0b1c2d3e4f50',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'oresmd://ir/usd?index=sofr&tenor=fomc&type=fixing',
        'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Raw FOMC-dated OIS grid: the synthetic feed''s tick target for the FOMC segment'
    ),
    (
        'f2d3e4a5-6c7d-4e8f-8a9b-1c2d3e4f5061',
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'oresmd://ir/usd?index=sofr&tenor=fomc&role=discount&type=curve',
        'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'Bootstrapped USD SOFR curve (FOMC segment): republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    )
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'marketdata_market_series (FOMC segment)' as entity, count(*) as count
from ores_marketdata_market_series_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and oresmd_uri like 'oresmd://ir/usd?index=sofr&tenor=fomc%'
  and valid_to = ores_utility_infinity_timestamp_fn();
