-- The live pass's own curve: an Acme-owned blueprint whose pillars name the ladder series the
-- synthetic feed ticks into, so the raise, the decision and the publish run between two people
-- of one party. Applied by hand to a provisioned tenant; idempotent.
\set ON_ERROR_STOP on

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, series_subclass, oresmd_uri,
    provider_code, derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary
)
select
    'c2b2b2b2-0000-4000-8000-000000000060', t.id, 0, p.id, 'yield',
    'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR-SYNTH2',
    'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
    'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user', 'system.initial_load',
    'The live pass: the curve this blueprint bootstraps, written by the republish'
from ores_iam_tenants_tbl t
cross join ores_refdata_parties_tbl p
where t.code = 'acme_corporation'
  and t.valid_to = ores_utility_infinity_timestamp_fn()
  and p.full_name = 'Acme Corporation Plc'
  and p.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_marketdata_market_series_tbl s
      where s.id = 'c2b2b2b2-0000-4000-8000-000000000060'::uuid
        and s.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_marketdata_market_series_asset_classes_tbl (
    market_series_id, tenant_id, asset_class_code, version,
    modified_by, performed_by, change_reason_code, change_commentary
)
select s.id, s.tenant_id, 'interest_rates', 0, 'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user',
       'system.initial_load', 'The live pass curve is an interest rates series'
from ores_marketdata_market_series_tbl s
where s.id = 'c2b2b2b2-0000-4000-8000-000000000060'::uuid
  and s.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_marketdata_market_series_asset_classes_tbl a
      where a.market_series_id = s.id and a.asset_class_code = 'interest_rates'
        and a.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
select 'b9b9b9b9-0000-4000-8000-000000000002', t.id, 0,
       'c2b2b2b2-0000-4000-8000-000000000060', p.id, 'USD-SOFR-SYNTH2', 'USD', '1D',
       'FUNDING', null, 'LOG_LINEAR_DISCOUNT', 'A360', 'RATES_SPOT_FORWARD', '30Y',
       'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user', 'system.initial_load',
       'The live pass: the spot-starting USD SOFR ladder the synthetic feed ticks'
from ores_iam_tenants_tbl t
cross join ores_refdata_parties_tbl p
where t.code = 'acme_corporation'
  and t.valid_to = ores_utility_infinity_timestamp_fn()
  and p.full_name = 'Acme Corporation Plc'
  and p.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_refdata_ir_curve_bootstrap_configs_tbl c
      where c.id = 'b9b9b9b9-0000-4000-8000-000000000002'::uuid
        and c.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
select v.id, c.tenant_id, 0, c.party_id, c.id, v.seq, 'SPOT', v.end_tenor, v.role,
       ladder.id, 'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user', 'system.initial_load',
       'The live pass: one rung of the synthetic ladder'
from (values
    ('b9000000-0000-4000-8000-000000000011'::uuid, 0, '3M', 'DEPOSIT'),
    ('b9000000-0000-4000-8000-000000000012'::uuid, 1, '6M', 'FRA'),
    ('b9000000-0000-4000-8000-000000000013'::uuid, 2, '12M', 'SWAP'),
    ('b9000000-0000-4000-8000-000000000014'::uuid, 3, '2Y', 'SWAP'),
    ('b9000000-0000-4000-8000-000000000015'::uuid, 4, '5Y', 'SWAP'),
    ('b9000000-0000-4000-8000-000000000016'::uuid, 5, '10Y', 'SWAP'),
    ('b9000000-0000-4000-8000-000000000017'::uuid, 6, '30Y', 'SWAP')
) as v(id, seq, end_tenor, role)
join ores_refdata_ir_curve_bootstrap_configs_tbl c
  on c.id = 'b9b9b9b9-0000-4000-8000-000000000002'::uuid
 and c.valid_to = ores_utility_infinity_timestamp_fn()
join ores_marketdata_market_series_tbl ladder
  on ladder.oresmd_uri = 'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D'
 and ladder.provider_code = 'bloomberg'
 and ladder.party_id = c.party_id
 and ladder.tenant_id = c.tenant_id
 and ladder.valid_to = ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_refdata_ir_curve_bootstrap_pillars_tbl b
    where b.id = v.id and b.valid_to = ores_utility_infinity_timestamp_fn());

select 'live pass pillars' as entity, count(*) as count
from ores_refdata_ir_curve_bootstrap_pillars_tbl
where bootstrap_config_id = 'b9b9b9b9-0000-4000-8000-000000000002'::uuid
  and valid_to = ores_utility_infinity_timestamp_fn();
