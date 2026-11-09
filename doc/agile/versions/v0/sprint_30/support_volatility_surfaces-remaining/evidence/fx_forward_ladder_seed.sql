\set ON_ERROR_STOP on
-- The market data the FX forward ladder scenario reads: a EUR and a USD funding
-- ladder owned by ACME Corporation US Inc, and a EUR/USD spot, all observed at
-- :as_of. Safe to run twice. Run with:
--   compass sql -u postgres -- -v as_of=2026-11-09T12:00:00Z -f <this file>
-- The curves are unpublished after this. fx_forward_ladder_publish.ores
-- publishes them through maker-checker.
select id as tenant_id from ores_iam_tenants_tbl
where name = 'Acme Corporation' and valid_to = ores_utility_infinity_timestamp_fn() \gset

select id as party_id from ores_refdata_parties_tbl
where short_code = 'ACCOUS' and tenant_id = :'tenant_id'
  and valid_to = ores_utility_infinity_timestamp_fn() \gset

begin;

create temp table seed_series (id uuid, subclass text, asset_class text, uri text) on commit drop;
insert into seed_series values
    ('bbbbbbbb-0000-4000-8000-000000000001', 'yield', 'interest_rates',
     'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D'),
    ('bbbbbbbb-0000-4000-8000-000000000002', 'yield', 'interest_rates',
     'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR-LADDER'),
    ('bbbbbbbb-0000-4000-8000-000000000101', 'yield', 'interest_rates',
     'oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D'),
    ('bbbbbbbb-0000-4000-8000-000000000102', 'yield', 'interest_rates',
     'oresmd://ir/EUR?type=series&instrument=discount&quote=rate&curve_id=EUR-ESTR-LADDER'),
    ('bbbbbbbb-0000-4000-8000-000000000201', 'spot', 'fx',
     'oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD');

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, series_subclass, oresmd_uri,
    provider_code, derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary)
select v.id, :'tenant_id', 0, :'party_id', v.subclass, v.uri,
       'bloomberg', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
       current_user, current_user, 'system.initial_load',
       'The FX forward ladder scenario: a seeded series'
from seed_series v
where not exists (select 1 from ores_marketdata_market_series_tbl s
                  where s.id = v.id and s.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_marketdata_market_series_asset_classes_tbl (
    market_series_id, tenant_id, asset_class_code, version,
    modified_by, performed_by, change_reason_code, change_commentary)
select v.id, :'tenant_id', v.asset_class, 0,
       current_user, current_user, 'system.initial_load',
       'The FX forward ladder scenario: the asset class of a seeded series'
from seed_series v
where not exists (select 1 from ores_marketdata_market_series_asset_classes_tbl a
                  where a.market_series_id = v.id and a.asset_class_code = v.asset_class
                    and a.valid_to = ores_utility_infinity_timestamp_fn());

create temp table seed_rung (seq int, term text, role text, usd text, eur text) on commit drop;
insert into seed_rung values
    (0, '3M',  'DEPOSIT', '0.0450', '0.0215'),
    (1, '6M',  'FRA',     '0.0440', '0.0212'),
    (2, '12M', 'SWAP',    '0.0430', '0.0210'),
    (3, '2Y',  'SWAP',    '0.0410', '0.0222'),
    (4, '5Y',  'SWAP',    '0.0395', '0.0245'),
    (5, '10Y', 'SWAP',    '0.0390', '0.0270'),
    (6, '30Y', 'SWAP',    '0.0385', '0.0265');

create temp table seed_ccy (ccy text, block int, series uuid, output uuid, config uuid,
                            name text, day_count text) on commit drop;
insert into seed_ccy values
    ('USD', 0, 'bbbbbbbb-0000-4000-8000-000000000001', 'bbbbbbbb-0000-4000-8000-000000000002',
     'bbbbbbbb-0000-4000-8000-000000000003', 'USD-SOFR-LADDER', 'A360'),
    ('EUR', 1, 'bbbbbbbb-0000-4000-8000-000000000101', 'bbbbbbbb-0000-4000-8000-000000000102',
     'bbbbbbbb-0000-4000-8000-000000000103', 'EUR-ESTR-LADDER', 'A360');

insert into ores_marketdata_market_observations_tbl (
    id, tenant_id, party_id, series_id, observation_datetime,
    oresmd_uri, value, source, feed_id, valid_from, valid_to)
select ('bbbbbbbb-0000-4000-8000-0000000' || (10000 + c.block * 100 + r.seq))::uuid,
       :'tenant_id', :'party_id', c.series, :'as_of'::timestamptz,
       'oresmd://ir/' || c.ccy || '?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=' || r.term,
       case c.ccy when 'USD' then r.usd else r.eur end, 'fx_forward_ladder_scenario',
       '437b14be-6443-4492-b421-a51ce34f318a'::uuid,
       clock_timestamp(), ores_utility_infinity_timestamp_fn()
from seed_ccy c cross join seed_rung r
where not exists (select 1 from ores_marketdata_market_observations_tbl o
                  where o.id = ('bbbbbbbb-0000-4000-8000-0000000' || (10000 + c.block * 100 + r.seq))::uuid
                    and o.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_marketdata_market_observations_tbl (
    id, tenant_id, party_id, series_id, observation_datetime,
    oresmd_uri, value, source, feed_id, valid_from, valid_to)
select 'bbbbbbbb-0000-4000-8000-000000020001', :'tenant_id', :'party_id',
       'bbbbbbbb-0000-4000-8000-000000000201', :'as_of'::timestamptz,
       'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD', '1.1650',
       'fx_forward_ladder_scenario', '437b14be-6443-4492-b421-a51ce34f318a'::uuid,
       clock_timestamp(), ores_utility_infinity_timestamp_fn()
where not exists (select 1 from ores_marketdata_market_observations_tbl o
                  where o.id = 'bbbbbbbb-0000-4000-8000-000000020001'::uuid
                    and o.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary)
select c.config, :'tenant_id', 0, c.output, :'party_id', c.name, c.ccy, '1D',
       'FUNDING', null, 'LOG_LINEAR_DISCOUNT', c.day_count, 'RATES_SPOT_FORWARD', '30Y',
       current_user, current_user, 'system.initial_load',
       'The FX forward ladder scenario: a seven-rung funding curve'
from seed_ccy c
where not exists (select 1 from ores_refdata_ir_curve_bootstrap_configs_tbl x
                  where x.id = c.config and x.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary)
select ('bbbbbbbb-0000-4000-8000-0000000' || (20000 + c.block * 100 + r.seq))::uuid,
       :'tenant_id', 0, :'party_id', c.config, r.seq, 'SPOT', r.term, r.role, c.series,
       current_user, current_user, 'system.initial_load',
       'The FX forward ladder scenario: one rung of the ladder'
from seed_ccy c cross join seed_rung r
where not exists (select 1 from ores_refdata_ir_curve_bootstrap_pillars_tbl b
                  where b.id = ('bbbbbbbb-0000-4000-8000-0000000' || (20000 + c.block * 100 + r.seq))::uuid
                    and b.valid_to = ores_utility_infinity_timestamp_fn());

commit;

select 'quotes' as entity, count(*) from ores_marketdata_market_observations_tbl
where source = 'fx_forward_ladder_scenario' and valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'pillars', count(*) from ores_refdata_ir_curve_bootstrap_pillars_tbl
where bootstrap_config_id in ('bbbbbbbb-0000-4000-8000-000000000003'::uuid, 'bbbbbbbb-0000-4000-8000-000000000103'::uuid)
  and valid_to = ores_utility_infinity_timestamp_fn();
