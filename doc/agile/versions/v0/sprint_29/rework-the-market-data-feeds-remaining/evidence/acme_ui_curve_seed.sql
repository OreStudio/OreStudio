\set ON_ERROR_STOP on
-- A hand-applied demo curve for the UI walkthrough: one spot-starting USD SOFR
-- ladder owned by ACME Corporation US Inc, with its quotes stored, so the Check
-- tab computes and the flow can be published by the two ACCOUS personas.
insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, series_subclass, oresmd_uri,
    provider_code, derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary)
select v.id, 'e472d07f-9c66-4c3b-a846-9c7ad978ac73', 0,
       'd47f42b3-6f6f-48e7-abdb-94f9ff291eae', v.subclass, v.uri,
       'bloomberg', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
       'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user',
       'system.initial_load', 'The UI walkthrough curve and its ladder series'
from (values
    ('aaaaaaaa-0000-4000-8000-000000000001'::uuid, 'yield',
     'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D'),
    ('aaaaaaaa-0000-4000-8000-000000000002'::uuid, 'yield',
     'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR-UI')
) as v(id, subclass, uri)
where not exists (select 1 from ores_marketdata_market_series_tbl s
                  where s.id = v.id and s.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_marketdata_market_series_asset_classes_tbl (
    market_series_id, tenant_id, asset_class_code, version,
    modified_by, performed_by, change_reason_code, change_commentary)
select s.id, s.tenant_id, 'interest_rates', 0,
       'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user',
       'system.initial_load', 'The UI walkthrough series is an interest rates series'
from ores_marketdata_market_series_tbl s
where s.id in ('aaaaaaaa-0000-4000-8000-000000000001'::uuid,
               'aaaaaaaa-0000-4000-8000-000000000002'::uuid)
  and s.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (select 1 from ores_marketdata_market_series_asset_classes_tbl a
                  where a.market_series_id = s.id and a.asset_class_code = 'interest_rates'
                    and a.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_marketdata_market_observations_tbl (
    id, tenant_id, party_id, series_id, observation_datetime,
    oresmd_uri, value, source, source_id, valid_from, valid_to)
select v.id, 'e472d07f-9c66-4c3b-a846-9c7ad978ac73',
       'd47f42b3-6f6f-48e7-abdb-94f9ff291eae', 'aaaaaaaa-0000-4000-8000-000000000001',
       clock_timestamp(),
       'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=' || v.term,
       v.value, 'acme_ui_walkthrough',
       '437b14be-6443-4492-b421-a51ce34f318a'::uuid,
       clock_timestamp(), ores_utility_infinity_timestamp_fn()
from (values
    ('aaaaaaaa-0000-4000-8000-000000000020'::uuid, '3M',  '0.0450'),
    ('aaaaaaaa-0000-4000-8000-000000000021'::uuid, '6M',  '0.0440'),
    ('aaaaaaaa-0000-4000-8000-000000000022'::uuid, '12M', '0.0430'),
    ('aaaaaaaa-0000-4000-8000-000000000023'::uuid, '2Y',  '0.0410'),
    ('aaaaaaaa-0000-4000-8000-000000000024'::uuid, '5Y',  '0.0395'),
    ('aaaaaaaa-0000-4000-8000-000000000025'::uuid, '10Y', '0.0390'),
    ('aaaaaaaa-0000-4000-8000-000000000026'::uuid, '30Y', '0.0385')
) as v(id, term, value)
where not exists (select 1 from ores_marketdata_market_observations_tbl o
                  where o.id = v.id and o.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, name, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary)
select 'aaaaaaaa-0000-4000-8000-000000000003', 'e472d07f-9c66-4c3b-a846-9c7ad978ac73', 0,
       'aaaaaaaa-0000-4000-8000-000000000002',
       'd47f42b3-6f6f-48e7-abdb-94f9ff291eae', 'USD-SOFR-UI', 'USD', '1D',
       'FUNDING', null, 'LOG_LINEAR_DISCOUNT', 'A360', 'RATES_SPOT_FORWARD', '30Y',
       'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user', 'system.initial_load',
       'The UI walkthrough curve: a seven-rung spot-starting USD SOFR ladder'
where not exists (select 1 from ores_refdata_ir_curve_bootstrap_configs_tbl c
                  where c.id = 'aaaaaaaa-0000-4000-8000-000000000003'::uuid
                    and c.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code, market_series_id,
    modified_by, performed_by, change_reason_code, change_commentary)
select v.id, 'e472d07f-9c66-4c3b-a846-9c7ad978ac73', 0,
       'd47f42b3-6f6f-48e7-abdb-94f9ff291eae',
       'aaaaaaaa-0000-4000-8000-000000000003', v.seq, 'SPOT', v.end_tenor, v.role,
       'aaaaaaaa-0000-4000-8000-000000000001',
       'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user', 'system.initial_load',
       'The UI walkthrough: one rung of the ladder'
from (values
    ('aaaaaaaa-0000-4000-8000-000000000010'::uuid, 0, '3M', 'DEPOSIT'),
    ('aaaaaaaa-0000-4000-8000-000000000011'::uuid, 1, '6M', 'FRA'),
    ('aaaaaaaa-0000-4000-8000-000000000012'::uuid, 2, '12M', 'SWAP'),
    ('aaaaaaaa-0000-4000-8000-000000000013'::uuid, 3, '2Y', 'SWAP'),
    ('aaaaaaaa-0000-4000-8000-000000000014'::uuid, 4, '5Y', 'SWAP'),
    ('aaaaaaaa-0000-4000-8000-000000000015'::uuid, 5, '10Y', 'SWAP'),
    ('aaaaaaaa-0000-4000-8000-000000000016'::uuid, 6, '30Y', 'SWAP')
) as v(id, seq, end_tenor, role)
where not exists (select 1 from ores_refdata_ir_curve_bootstrap_pillars_tbl b
                  where b.id = v.id and b.valid_to = ores_utility_infinity_timestamp_fn());

select 'series' as entity, count(*) from ores_marketdata_market_series_tbl
where id in ('aaaaaaaa-0000-4000-8000-000000000001'::uuid, 'aaaaaaaa-0000-4000-8000-000000000002'::uuid)
  and valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'quotes', count(*) from ores_marketdata_market_observations_tbl
where series_id = 'aaaaaaaa-0000-4000-8000-000000000001'::uuid
  and valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'pillars', count(*) from ores_refdata_ir_curve_bootstrap_pillars_tbl
where bootstrap_config_id = 'aaaaaaaa-0000-4000-8000-000000000003'::uuid
  and valid_to = ores_utility_infinity_timestamp_fn();
