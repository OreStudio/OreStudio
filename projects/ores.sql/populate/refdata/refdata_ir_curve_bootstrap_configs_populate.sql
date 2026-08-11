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
 * IR Curve Bootstrap Config and Pillar Population Script
 *
 * Seeds the single bootstrap configuration for the FOMC-dated short end of
 * the USD SOFR curve: the segment built off consecutive FOMC-meeting-dated
 * pillars (1F..8F) with a flat-forward step between meetings, handing over
 * to the continuous swap grid at the 1Y split tenor. The config is the
 * FUNDING anchor curve of its family: discount_curve_config_id holds the
 * nil sentinel, per the model's two-tier FUNDING/PROJECTION rule.
 *
 * Interpolation is the two-segment FLAT_FORWARD_THEN_LOG_LINEAR -- flat
 * forward between the meeting-dated pillars (each FOMC-to-FOMC interval
 * prices its own meeting rate), log-linear in the swap grid beyond 1Y.
 * Day count is ACT/360, the USD SOFR overnight-index convention; the
 * split_tenor_code 1Y is the PERIOD tenor whose calendar-axis resolution
 * (spot + one year) closes the segment under the RATES_SPOT_FOMC
 * convention (see refdata_tenor_convention_resolutions_populate.sql).
 *
 * The config's source_series_id and output_series_id reference the two
 * market_series rows seeded by
 * marketdata_market_series_fomc_populate.sql (a soft, cross-component
 * reference -- no FK): the raw RATES/YIELD grid the synthetic feed
 * publishes FOMC quotes into, and the YieldCurve/DISCOUNT series the
 * bootstrapped curve is written to.
 *
 * The nine pillars chain SPOT -> 1F -> 2F -> ... -> 8F -> 1Y in sequence
 * order: a DEPOSIT from spot to the first meeting, then one SWAP per
 * meeting interval whose fixed leg is the meeting-dated grid itself (the
 * FOMC-to-FOMC fixing schedule), closing with the 8F -> 1Y swap into the
 * split tenor. Each pillar's point id is its end tenor code, and the raw
 * grid must quote every one of them (1F..8F plus 1Y).
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- IR Curve Bootstrap Configs ---'

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, source_series_id,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f',
    ores_utility_system_tenant_id_fn(),
    0,
    'f2d3e4a5-6c7d-4e8f-8a9b-1c2d3e4f5061',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'e1c2d3f4-5b6c-4d7e-9f8a-0b1c2d3e4f50',
    'FUNDING', ores_utility_nil_uuid_fn(), 'FLAT_FORWARD_THEN_LOG_LINEAR',
    'A360', '1Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    party_id = excluded.party_id,
    source_series_id = excluded.source_series_id,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

\echo '--- IR Curve Bootstrap Pillars ---'

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('b1000000-0000-4000-8000-000000000001', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 0, 'SPOT', '1F', 'DEPOSIT',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000002', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 1, '1F', '2F', 'SWAP',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000003', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 2, '2F', '3F', 'SWAP',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000004', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 3, '3F', '4F', 'SWAP',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000005', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 4, '4F', '5F', 'SWAP',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000006', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 5, '5F', '6F', 'SWAP',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000007', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 6, '6F', '7F', 'SWAP',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000008', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 7, '7F', '8F', 'SWAP',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000009', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 8, '8F', '1Y', 'SWAP',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    party_id = excluded.party_id,
    bootstrap_config_id = excluded.bootstrap_config_id,
    sequence_index = excluded.sequence_index,
    start_tenor_code = excluded.start_tenor_code,
    end_tenor_code = excluded.end_tenor_code,
    curve_role_code = excluded.curve_role_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_ir_curve_bootstrap_configs' as entity, count(*) as count
from ores_refdata_ir_curve_bootstrap_configs_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'refdata_ir_curve_bootstrap_pillars', count(*)
from ores_refdata_ir_curve_bootstrap_pillars_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
