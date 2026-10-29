/**
 * IR Curve Bootstrap Config and Pillar Population Script
 *
 * Seeds the bootstrap configuration for the demo's USD SOFR curve: the
 * spot-starting OIS ladder the synthetic Bloomberg feed publishes, from the 3M
 * deposit out to the 30Y swap. The config is the FUNDING anchor curve of its
 * family: discount_curve_config_id is null, per the model's two-tier
 * FUNDING/PROJECTION rule -- a funding curve discounts off nothing, so nothing
 * stands in for a discount curve.
 *
 * The config carries its display name, which is what the blueprint screens draw
 * in the list, in the record header and as the first fact on Details. The name
 * is stored and never parsed out of the output series' oresmd URI.
 *
 * Interpolation is the single-segment LOG_LINEAR_DISCOUNT -- the factors are
 * log-linear between adjacent knots -- so split_tenor_code is the last pillar's
 * own end tenor, a genuine value and not a sentinel. Day count is ACT/360, the
 * USD SOFR overnight-index convention, and the tenor convention is
 * RATES_SPOT_FORWARD, which resolves every pillar tenor off spot.
 *
 * The config's output_series_id references the curve's market_series row
 * seeded by marketdata_market_series_ir_curve_populate.sql (a soft,
 * cross-component reference -- no FK): the DISCOUNT/RATE/USD/USD-SOFR-FOMC
 * series the bootstrapped curve is written to.
 *
 * Every pillar names the same input series -- the spot-starting USD SOFR swap
 * quote that seed also writes -- and finds its own rung of it by its end tenor.
 * That is what [[id:7AE57098-08D4-4D4A-83D6-918FE520B68E][A pillar names the
 * market series it reads]] decides: the input is a column a person reads, not a
 * spelling composed from the config's currency and the pillar's dates.
 *
 * The seven pillars ladder the tenors the seeded synthetic template publishes:
 * a 3M deposit from spot, a 6M forward rate agreement from spot, then swaps from
 * spot to 12M, 2Y, 5Y, 10Y and 30Y. Each swap's fixed leg is the end dates of
 * the pillars before it together with its own, so the ladder is one strip.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

/**
 * IR Curve Bootstrap Config and Pillar Population Script
 *
 * Seeds the single bootstrap configuration for the FOMC-dated short end of
 * the USD SOFR curve: the segment built off consecutive FOMC-meeting-dated
 * pillars (1F..8F) with a flat-forward step between meetings, handing over
 * to the continuous swap grid at the 1Y split tenor. The config is the
 * FUNDING anchor curve of its family: discount_curve_config_id is null, per
 * the model's two-tier FUNDING/PROJECTION rule -- a funding curve discounts
 * off nothing, so nothing stands in for a discount curve.
 *
 * The config carries its display name, USD-SOFR-FOMC, which is what the
 * blueprint screens draw in the list, in the record header and as the first
 * fact on Details. The name is stored and never parsed out of the output
 * series' oresmd URI.
 *
 * Interpolation is the two-segment FLAT_FORWARD_THEN_LOG_LINEAR -- flat
 * forward between the meeting-dated pillars (each FOMC-to-FOMC interval
 * prices its own meeting rate), log-linear in the swap grid beyond 1Y.
 * Day count is ACT/360, the USD SOFR overnight-index convention; the
 * split_tenor_code 1Y is the PERIOD tenor whose calendar-axis resolution
 * (spot + one year) closes the segment under the RATES_SPOT_FOMC
 * convention (see refdata_tenor_convention_resolutions_populate.sql).
 *
 * The config's output_series_id references the curve's market_series row
 * seeded by marketdata_market_series_fomc_populate.sql (a soft,
 * cross-component reference -- no FK): the DISCOUNT/RATE/USD/USD-SOFR-FOMC
 * series the bootstrapped curve is written to. The pillars are the config's
 * input, so no source series is named: each pillar is read from the series
 * the feed's tick for that instrument landed in.
 *
 * The nine pillars chain SPOT -> 1F -> 2F -> ... -> 8F -> 1Y in sequence
 * order: a DEPOSIT from spot to the first meeting, then one SWAP per
 * meeting interval whose fixed leg is the meeting-dated grid itself (the
 * FOMC-to-FOMC fixing schedule), closing with the 8F -> 1Y swap into the
 * split tenor. Each pillar's point id is its end tenor code, and each
 * pillar's own series must quote it.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- IR Curve Bootstrap Configs ---'

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
values (
    'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f',
    ores_utility_system_tenant_id_fn(),
    0,
    'f2d3e4a5-6c7d-4e8f-8a9b-1c2d3e4f5061',
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'USD-SOFR-FOMC', 'USD', '1D',
    'FUNDING', null, 'LOG_LINEAR_DISCOUNT',
    'A360', 'RATES_SPOT_FORWARD', '30Y',
    current_user, current_user, 'system.initial_load',
    'Initial population of IR curve bootstrap configs'
)
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    output_series_id = excluded.output_series_id,
    party_id = excluded.party_id,
    name = excluded.name,
    currency_code = excluded.currency_code,
    index_tenor = excluded.index_tenor,
    curve_family_role = excluded.curve_family_role,
    discount_curve_config_id = excluded.discount_curve_config_id,
    interpolation_method = excluded.interpolation_method,
    day_count_convention = excluded.day_count_convention,
    tenor_convention_code = excluded.tenor_convention_code,
    split_tenor_code = excluded.split_tenor_code,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

\echo '--- IR Curve Bootstrap Pillars ---'

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('b1000000-0000-4000-8000-000000000001', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 0, 'SPOT', '3M', 'DEPOSIT',
     'f7a1b2c3-8d4e-4f60-9a2b-3c4d5e6f7081',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000002', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 1, 'SPOT', '6M', 'FRA',
     'f7a1b2c3-8d4e-4f60-9a2b-3c4d5e6f7081',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000003', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 2, 'SPOT', '12M', 'SWAP',
     'f7a1b2c3-8d4e-4f60-9a2b-3c4d5e6f7081',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000004', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 3, 'SPOT', '2Y', 'SWAP',
     'f7a1b2c3-8d4e-4f60-9a2b-3c4d5e6f7081',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000005', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 4, 'SPOT', '5Y', 'SWAP',
     'f7a1b2c3-8d4e-4f60-9a2b-3c4d5e6f7081',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000006', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 5, 'SPOT', '10Y', 'SWAP',
     'f7a1b2c3-8d4e-4f60-9a2b-3c4d5e6f7081',
     current_user, current_user, 'system.initial_load', 'Initial population of IR curve bootstrap pillars'),
    ('b1000000-0000-4000-8000-000000000007', ores_utility_system_tenant_id_fn(), 0,
     ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
     'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f', 6, 'SPOT', '30Y', 'SWAP',
     'f7a1b2c3-8d4e-4f60-9a2b-3c4d5e6f7081',
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
    market_series_id = excluded.market_series_id,
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
