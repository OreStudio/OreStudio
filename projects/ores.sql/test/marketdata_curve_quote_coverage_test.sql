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
 * A curve configuration's quotes are answered by the series the party holds.
 *
 * A curve bootstraps from quote keys, and the market holds series addresses. The
 * two spellings name one datum, and the term the key ends with is the point
 * within the series rather than part of its identity, so a quote is answered by
 * a series that carries the currency, the forward start and the index tenor and
 * not the term.
 *
 * A key the codec would refuse names no datum, so it is reported as unreadable
 * and not as a gap the market could close. A series held by another party
 * answers nothing, because the export writes a run from the party's own quote
 * series. And a session that cannot see the party is refused rather than told
 * there are no gaps.
 *
 * The fixture is staged and published through the same DQ document path a
 * provision uses, so the rows the check reads are shaped as a provision leaves
 * them.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/marketdata_curve_quote_coverage_test.sql
 */

begin;

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- A curve configuration names its currencies, and a currency column is checked
-- against the currency catalogue.
select * from ores_refdata_publish_currencies_from_dq_fn(
    (select id from ores_dq_datasets_tbl
      where code = 'iso.currencies'
        and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code <> 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn() limit 1) as other_party_id,
       '00000000-0000-0000-0000-0000000f0001'::uuid as curve_header,
       '00000000-0000-0000-0000-0000000f0002'::uuid as curve_definition,
       '00000000-0000-0000-0000-0000000f0003'::uuid as curve_yield,
       '00000000-0000-0000-0000-0000000f0004'::uuid as curve_quote,
       '00000000-0000-0000-0000-0000000f0005'::uuid as curve_reporting,
       '00000000-0000-0000-0000-0000000f0006'::uuid as foreign_series,
       '00000000-0000-0000-0000-0000000f0007'::uuid as party_series;

-- Ten assertions, and one more when the tenant holds a second party for the
-- cross-party case to use.
select plan(12 + (select count(*) from t_ctx where other_party_id is not null)::integer);

do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.curve_quote_coverage', 'ACME Corporation', 'Reporting', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'OreStudio Code Generation Methodology',
        'Test Curve Quote Coverage', 'A curve configuration staged for a test.',
        'ACMEBANK', 'pgTAP fixture', current_date, 'Internal Use Only', 'curve_configuration');
end $$;

create temp table t_curve on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.curve_quote_coverage' and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_dq_curve_configurations_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, name, description, configuration_id)
select k.id, ores_utility_system_tenant_id_fn(), c.curve_header, 0, c.party_id,
    'Test curve configuration', 'A document', c.curve_reporting
from t_curve k, t_ctx c;

insert into ores_dq_curve_definitions_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, curve_configuration_id, section_code,
    curve_id, description, position)
select k.id, ores_utility_system_tenant_id_fn(), c.curve_definition, 0, c.party_id,
    c.curve_header, 'YieldCurves', 'GBP1D', 'GBP overnight curve', 0
from t_curve k, t_ctx c;

insert into ores_dq_yield_curve_configs_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, curve_definition_id, currency, discount_curve,
    has_report)
select k.id, ores_utility_system_tenant_id_fn(), c.curve_yield, 0, c.party_id,
    c.curve_definition, 'GBP', '', false
from t_curve k, t_ctx c;

-- The fixture's quote names a currency no seeded series answers, so the gap
-- asserted below is the fixture's and not a statement about what the database
-- holds.
insert into ores_dq_curve_quotes_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, curve_definition_id, quote_text, position)
select k.id, ores_utility_system_tenant_id_fn(), c.curve_quote, 0, c.party_id,
    c.curve_definition, 'IR_SWAP/RATE/ZZZ/0D/1D/1M', 0
from t_curve k, t_ctx c;

select lives_ok(
    format($$select * from ores_refdata_publish_curve_configuration_from_dq_fn(%L::uuid,
        ores_utility_system_tenant_id_fn(), 'upsert',
        jsonb_build_object('party_id', %L::text))$$,
        (select id from t_curve), (select party_id from t_ctx)),
    'the curve configuration publishes with its quote');

-- The pair the codec and this check both state: the key names the series, and
-- the term it ends with is the point within that series.
select is(
    ores_marketdata_curve_pillar_series_uri_fn('IR_SWAP/RATE/GBP/0D/1D/1M'),
    'oresmd://ir/GBP?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D',
    'an interest rate swap quote names its series without the term');

select is(
    ores_marketdata_curve_pillar_series_uri_fn('IR_SWAP/RATE/USD/LIBOR3M/0D/3M/10Y'),
    'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&index_name=LIBOR3M&fwd_start=0D&tenor=3M',
    'a quote that names an index keeps the index in the series');

-- The reader is as strict as the codec: a key it would refuse names no datum,
-- so the check reports the key rather than a market gap.
select is(
    ores_marketdata_curve_pillar_series_uri_fn('FX/GBP/USD'),
    null::text,
    'a quote of a form the check does not read names no series');

select is(
    ores_marketdata_curve_pillar_series_uri_fn('ir_swap/rate/GBP/0D/1D/1M'),
    null::text,
    'a key whose type is not spelled as ORE spells it names no series');

select is(
    ores_marketdata_curve_pillar_series_uri_fn('IR_SWAP/RATE/GBP//1D/1M'),
    null::text,
    'a key with an empty token names no series');

select is(
    ores_marketdata_curve_pillar_series_uri_fn('IR_SWAP/RATE/GBP/20261121/1D/1M'),
    null::text,
    'a key that starts at a date and ends at a period names no series');

select is(
    ores_marketdata_curve_pillar_series_uri_fn('IR_SWAP/RATE/USD/20261120/3M/20271120'),
    'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=20261120&tenor=3M',
    'a meeting-dated quote keeps its start date and drops its end date');

-- A published configuration whose quote no series answers is one row, and the
-- row says which work it asks for.
select results_eq(
    $$select quote_text, kind, reason from ores_marketdata_curve_quote_coverage_fn(
        ores_utility_system_tenant_id_fn(), (select party_id from t_ctx))$$,
    $$values ('IR_SWAP/RATE/ZZZ/0D/1D/1M'::text, 'unanswered'::text,
        'The party holds no series for this quote.'::text)$$,
    'a quote no series answers is listed as an unanswered gap');

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, oresmd_uri, provider_code, series_subclass,
    derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to)
select c.foreign_series, ores_utility_system_tenant_id_fn(), 0, c.other_party_id,
    'oresmd://ir/ZZZ?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D',
    'lseg', 'yield', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
    current_user, current_user, 'system.initial_load', 'pgTAP fixture',
    now(), ores_utility_infinity_timestamp_fn()
from t_ctx c
where c.other_party_id is not null;

select case
    when (select other_party_id from t_ctx) is null then
        skip(1, 'the tenant holds one party, so no other party can hold a series')
    else
        is((select count(*) from ores_marketdata_curve_quote_coverage_fn(
                ores_utility_system_tenant_id_fn(), (select party_id from t_ctx))),
           1::bigint,
           'a series another party holds answers nothing, so the quote is still a gap')
end;

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, oresmd_uri, provider_code, series_subclass,
    derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to)
select c.party_series, ores_utility_system_tenant_id_fn(), 0, c.party_id,
    'oresmd://ir/ZZZ?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D',
    'lseg', 'yield', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
    current_user, current_user, 'system.initial_load', 'pgTAP fixture',
    now(), ores_utility_infinity_timestamp_fn()
from t_ctx c;

select is(
    (select count(*) from ores_marketdata_curve_quote_coverage_fn(
        ores_utility_system_tenant_id_fn(), (select party_id from t_ctx))),
    0::bigint,
    'the party''s own series answers the quote');

-- A session that cannot see the party reads no configuration, and an empty
-- result would be read as "no gaps", so the call is refused instead.
select set_config('app.visible_party_ids', '{}', true);

select throws_ok(
    format($$select * from ores_marketdata_curve_quote_coverage_fn(
        ores_utility_system_tenant_id_fn(), %L::uuid)$$, (select party_id from t_ctx)),
    '42501',
    null,
    'a party the session cannot see is refused rather than reported as having no gaps'
);

select * from finish();
rollback;
