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
 * pgTAP tests for the series that answer a list of oresmd requirements.
 *
 * Tests cover:
 * - A requirement no series answers is a gap: one row, no series
 * - A series with a fixing at or before the date answers with that date
 * - A series whose only fixing is after the date has no latest point
 * - Another party's series does not answer
 * - A quote series answers with its last observation at or before the date
 *
 * Run with: pg_prove -d <database> test/marketdata_requirement_coverage_test.sql
 */

begin;

select plan(6);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       'oresmd://ir/GBP?type=fixing&index=ibor&name=PGTAPCOVER'::text as answered,
       'oresmd://ir/GBP?type=fixing&index=ibor&name=PGTAPGAP'::text as missing;

select set_config('app.current_party_id', (select party_id from t_ctx)::text, true);
select set_config('app.visible_party_ids', '{' || (select party_id from t_ctx)::text || '}', true);

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, oresmd_uri, series_subclass, provider_code,
    derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-0000000e3001', ores_utility_system_tenant_id_fn(), 0,
    party_id, answered, 'index_fixing', 'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
    current_user, current_user, 'system.new_record', 'Requirement coverage pgTAP fixture'
from t_ctx;

insert into ores_marketdata_market_fixings_tbl (
    id, tenant_id, party_id, series_id, fixing_date, value, source)
select '00000000-0000-0000-0000-0000000e4001', ores_utility_system_tenant_id_fn(), party_id,
    '00000000-0000-0000-0000-0000000e3001', date '2026-01-05', '0.0425', 'test.pgtap'
from t_ctx;

select results_eq(
    $$select series_id is null, latest_point is null
      from ores_marketdata_requirement_coverage_fn(ores_utility_system_tenant_id_fn(),
          (select party_id from t_ctx), array[(select missing from t_ctx)])$$,
    $$values (true, true)$$,
    'a requirement no series answers comes back once, as a gap');

select is(
    (select series_id from ores_marketdata_requirement_coverage_fn(ores_utility_system_tenant_id_fn(),
         (select party_id from t_ctx), array[(select answered from t_ctx)])),
    '00000000-0000-0000-0000-0000000e3001'::uuid,
    'a requirement is answered by the party''s series with its address');

select is(
    (select latest_point::date from ores_marketdata_requirement_coverage_fn(
         ores_utility_system_tenant_id_fn(), (select party_id from t_ctx),
         array[(select answered from t_ctx)], timestamptz '2026-03-01 00:00:00+00')),
    date '2026-01-05',
    'the latest point is the last fixing at or before the date');

select is(
    (select latest_point from ores_marketdata_requirement_coverage_fn(
         ores_utility_system_tenant_id_fn(), (select party_id from t_ctx),
         array[(select answered from t_ctx)], timestamptz '2026-01-01 00:00:00+00')),
    null::timestamptz,
    'a series whose only fixing is after the date has no latest point');

select is(
    (select series_id from ores_marketdata_requirement_coverage_fn(ores_utility_system_tenant_id_fn(),
         gen_random_uuid(), array[(select answered from t_ctx)])),
    null::uuid,
    'another party''s series does not answer the requirement');

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, oresmd_uri, series_subclass, provider_code,
    derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-0000000e3002', ores_utility_system_tenant_id_fn(), 0,
    party_id, 'oresmd://fx/GBP?type=series&instrument=fx_spot&quote=rate&ccy=PGT',
    'spot', 'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
    current_user, current_user, 'system.new_record', 'Requirement coverage pgTAP fixture'
from t_ctx;

insert into ores_marketdata_market_observations_tbl (
    id, tenant_id, party_id, series_id, observation_datetime, oresmd_uri, value, source,
    feed_id)
select '00000000-0000-0000-0000-0000000e5001', ores_utility_system_tenant_id_fn(), party_id,
    '00000000-0000-0000-0000-0000000e3002', timestamptz '2026-02-10 16:00:00+00',
    'oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=PGT', '1.25', 'test.pgtap',
    ores_utility_nil_uuid_fn()
from t_ctx;

select is(
    (select latest_point from ores_marketdata_requirement_coverage_fn(
         ores_utility_system_tenant_id_fn(), (select party_id from t_ctx),
         array['oresmd://fx/GBP?type=series&instrument=fx_spot&quote=rate&ccy=PGT'],
         timestamptz '2026-03-01 00:00:00+00')),
    timestamptz '2026-02-10 16:00:00+00',
    'a quote series answers with its last observation at or before the date');

select * from finish();

rollback;
