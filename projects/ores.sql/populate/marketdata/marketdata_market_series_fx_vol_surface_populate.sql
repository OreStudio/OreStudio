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
 * The Market Series of the EUR/USD Volatility Surface
 *
 * Seeds the two market_series catalog rows the seeded EUR/USD blueprint names, each
 * with a fixed uuid so the blueprint can refer to it:
 *
 *  - The fx_option series the derived source writes. It holds every quote of every
 *    expiry of the pair, so every pillar source names this one series. The row is
 *    seeded so that the blueprint has a series to name on a fresh database. The
 *    derived feed finds it by its URI and party, and writes into it instead of
 *    creating a second one, so it carries the feed's own derivation stamp.
 *
 *  - The fx_vol_surface series the calibration writes. It starts OBSERVED, as the
 *    bootstrapped curve's series does, and the calibration claims it on its first
 *    run.
 *
 * This script is idempotent. A rerun must not reset a row the calibration has claimed.
 */

\echo '--- EUR/USD Volatility Surface Market Series ---'

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, series_subclass, oresmd_uri,
    provider_code, derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary
)
select v.*
from (values
    (
        'a4e1c6d2-7b35-4f08-9c1e-5d2f8a7b3c01'::uuid,
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'volatility', 'oresmd://fx/EUR?type=series&instrument=fx_option&quote=rate_lnvol&ccy=USD',
        'derived', 'FX_VOL_DERIVATION', 'fefdf018-d882-5c4e-bd27-2aeedfcad9c9'::uuid, 1,
        current_user, current_user, 'system.initial_load',
        'The EUR/USD quotes the derived source writes, which the seeded blueprint reads'
    ),
    (
        'a4e1c6d2-7b35-4f08-9c1e-5d2f8a7b3c02'::uuid,
        ores_utility_system_tenant_id_fn(),
        0,
        ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
        'volatility',
        'oresmd://fx/EUR?type=series&instrument=fx_vol_surface&quote=rate_lnvol&ccy=USD&surface_id=EURUSD_SSVI',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        current_user, current_user, 'system.initial_load',
        'The calibrated EUR/USD surface: claimed by the calibration on its first run'
    )
) as v (
    id, tenant_id, version, party_id, series_subclass, oresmd_uri,
    provider_code, derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary
)
where not exists (
    select 1 from ores_marketdata_market_series_tbl s
    where s.tenant_id = v.tenant_id and s.id = v.id
      and s.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_market_series_asset_classes_tbl (
    market_series_id, tenant_id, asset_class_code, version,
    modified_by, performed_by, change_reason_code, change_commentary
)
select s.id::uuid, ores_utility_system_tenant_id_fn(), 'fx', 0,
       current_user, current_user, 'system.initial_load',
       'The EUR/USD volatility series are FX series'
from (values ('a4e1c6d2-7b35-4f08-9c1e-5d2f8a7b3c01'),
             ('a4e1c6d2-7b35-4f08-9c1e-5d2f8a7b3c02')) as s (id)
where not exists (
    select 1 from ores_marketdata_market_series_asset_classes_tbl a
    where a.tenant_id = ores_utility_system_tenant_id_fn()
      and a.market_series_id = s.id::uuid
      and a.asset_class_code = 'fx'
      and a.valid_to = ores_utility_infinity_timestamp_fn()
);
