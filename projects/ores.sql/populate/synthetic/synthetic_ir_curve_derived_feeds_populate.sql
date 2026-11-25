\echo '--- Derived provider and collection ---'

-- A derived index is a collection of its own, so it has a provider and a
-- container like every other generated feed. The provider is the one the
-- parties already subscribe to; the container hangs under the same dataset and
-- party as the demo collection it refines, so one publish carries both.
insert into ores_synthetic_market_data_providers_tbl (
    code, tenant_id, version, name, kind, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select 'derived', ores_utility_system_tenant_id_fn(), 0, 'Derived', 'reference',
       'Market data the platform computes from data it already holds: the term indices no publisher quotes in the run, stated from their currency''s overnight curve.',
       40, current_user, current_user, 'system.initial_load',
       'Initial population of the derived provider', now(),
       ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_synthetic_market_data_providers_tbl p
    where p.tenant_id = ores_utility_system_tenant_id_fn()
      and p.code = 'derived'
      and p.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_synthetic_market_data_generation_configs_tbl (
    id, tenant_id, version, party_id, name, description, enabled, dataset_id,
    provider_code, modified_by, performed_by, change_reason_code,
    change_commentary, valid_from, valid_to
)
select gen_random_uuid(), d.tenant_id, 0,
       ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
       'Derived',
       'The term indices the run states from its overnight curves, for the trades that float on them.',
       true, d.id, 'derived', current_user, current_user,
       'system.initial_load', 'Initial population of the derived collection',
       now(), ores_utility_infinity_timestamp_fn()
from ores_dq_datasets_tbl d
where d.tenant_id = ores_utility_system_tenant_id_fn()
  and d.code = 'synthetic.feeds.derived'
  and d.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_synthetic_market_data_generation_configs_tbl c
      where c.tenant_id = ores_utility_system_tenant_id_fn()
        and c.provider_code = 'derived'
        and c.valid_to = ores_utility_infinity_timestamp_fn()
  )
limit 1;

-- A collection staged under another dataset is moved onto this one, so a database
-- that grew the collection before the dataset existed converges without a recreate.
update ores_synthetic_market_data_generation_configs_tbl c
set dataset_id = d.id
from ores_dq_datasets_tbl d
where c.tenant_id = ores_utility_system_tenant_id_fn()
  and c.provider_code = 'derived'
  and c.valid_to = ores_utility_infinity_timestamp_fn()
  and d.tenant_id = ores_utility_system_tenant_id_fn()
  and d.code = 'synthetic.feeds.derived'
  and d.valid_to = ores_utility_infinity_timestamp_fn()
  and c.dataset_id is distinct from d.id;

\echo '--- Derived Term Index Configs ---'

insert into ores_synthetic_ir_curve_generation_configs_tbl (
    tenant_id, id, version, party_id, config_id, folder_id,
    currency_code, index_family, tenor, role, process_type,
    ticks_per_hour, enabled, auto_start, price_source,
    rate_source, derived_spread_basis_points, derived_spread_source,
    description, fixed_leg_payment_frequency_code, source_name,
    asset_class_code, arrival_mode, branching_ratio, decay, sensitivity,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select
    ores_utility_system_tenant_id_fn(),
    gen_random_uuid(),
    0,
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    derived_container.id,
    null,
    d.currency_code,
    d.index_family,
    d.tenor,
    'projection',
    'VASICEK',
    60,
    true,
    true,
    'fixed',
    d.rate_source,
    d.spread_basis_points,
    d.spread_source,
    d.description,
    'Quarterly',
    'synthetic.derived.' || d.currency_code || '.' || d.index_family || '.' || d.tenor,
    'interest_rates',
    'periodic',
    0.0,
    1.0,
    1.0,
    current_user, current_user,
    'system.initial_load',
    'Initial population of derived term index configs',
    now(),
    ores_utility_infinity_timestamp_fn()
from (values
    ('USD', 'sofr',     '1M', 'compounded_overnight',   0.0,
     null::text,
     'Term SOFR at 1M: the compounded overnight SOFR of its curve over the term, as the bond leg of the ACCOUS Ascot fixes.'),
    ('COP', 'ibr',      '3M', 'compounded_overnight',   0.0,
     null::text,
     'Term IBR at 3M: the compounded overnight IBR of its curve over the term, as the ACCOHK cross-currency legs fix.'),
    ('JPY', 'eytibor',  '3M', 'overnight_plus_spread',  10.0,
     'Market convention: EYTIBOR 3M quoted over TONA, at the spread the yen cross-currency legs of ACCOHK are booked at.',
     'EYTIBOR at 3M: TONA plus its stated spread, as the ACCOHK cross-currency legs fix.'),
    ('KRW', 'generic',  '3M', 'overnight_plus_spread',  15.0,
     'Market convention: the generic 3M interbank rate of the won quoted over KOFR.',
     'The generic KRW term index at 3M: KOFR plus its stated spread, as the ACCOHK cross-currency legs fix.'),
    ('PLN', 'wibor',    '6M', 'overnight_plus_spread',  20.0,
     'Market convention: WIBOR 6M quoted over POLONIA.',
     'WIBOR at 6M: POLONIA plus its stated spread, as the ACCOHK cross-currency legs fix.')
) as d(currency_code, index_family, tenor, rate_source, spread_basis_points, spread_source,
       description)
join lateral (
    select c.id
    from ores_synthetic_market_data_generation_configs_tbl c
    where c.tenant_id = ores_utility_system_tenant_id_fn()
      and c.provider_code = 'derived'
      and c.enabled
      and c.valid_to = ores_utility_infinity_timestamp_fn()
    limit 1
) as derived_container on true
where not exists (
    select 1
    from ores_synthetic_ir_curve_generation_configs_tbl existing
    where existing.tenant_id = ores_utility_system_tenant_id_fn()
      and existing.party_id = ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn())
      and existing.config_id = derived_container.id
      and existing.currency_code = d.currency_code
      and existing.index_family = d.index_family
      and existing.tenor = d.tenor
      and existing.role = 'projection'
      and existing.valid_to = ores_utility_infinity_timestamp_fn()
);

-- The fixing publisher needs an instrument for each index it states, and the market
-- data service demands a generator only for the instruments the run holds. The five
-- derived indices are not in the vendor seed, which builds its list from the datasets'
-- staged configs, so they are stated here beside the configs that publish them.
insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted,
    deviation_note, modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select md5('derived|oresmd|' || d.item || '|value')::uuid,
       ores_utility_system_tenant_id_fn(), 0, 'derived', 'oresmd', d.item, 'value',
       false, '', current_user, current_user, 'system.initial_load',
       'The derived term indices the platform states',
       now(), ores_utility_infinity_timestamp_fn()
from (values
    ('oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&tenor=1M'),
    ('oresmd://ir/COP?type=fixing&index=ibor&name=IBR&tenor=3M'),
    ('oresmd://ir/JPY?type=fixing&index=ibor&name=EYTIBOR&tenor=3M'),
    ('oresmd://ir/KRW?type=fixing&index=ibor&name=GENERIC&tenor=3M'),
    ('oresmd://ir/PLN?type=fixing&index=ibor&name=WIBOR&tenor=6M')
) as d(item)
where not exists (
    select 1 from ores_marketdata_market_data_vendor_instruments_tbl v
    where v.tenant_id = ores_utility_system_tenant_id_fn()
      and v.provider_code = 'derived'
      and v.external_id = d.item
      and v.field = 'value'
      and v.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_market_data_mappings_tbl (
    id, tenant_id, version, vendor_instrument_id, oresmd_uri, effective_from,
    modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v.id, v.external_id,
       timestamptz '2026-01-01 00:00:00+00', current_user, current_user, 'system.initial_load',
       'The derived term indices the platform states',
       now(), ores_utility_infinity_timestamp_fn()
from ores_marketdata_market_data_vendor_instruments_tbl v
where v.tenant_id = ores_utility_system_tenant_id_fn()
  and v.provider_code = 'derived'
  and v.field = 'value'
  and v.valid_to = ores_utility_infinity_timestamp_fn()
  and v.external_id in (
      'oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&tenor=1M',
      'oresmd://ir/COP?type=fixing&index=ibor&name=IBR&tenor=3M',
      'oresmd://ir/JPY?type=fixing&index=ibor&name=EYTIBOR&tenor=3M',
      'oresmd://ir/KRW?type=fixing&index=ibor&name=GENERIC&tenor=3M',
      'oresmd://ir/PLN?type=fixing&index=ibor&name=WIBOR&tenor=6M')
  and not exists (
      select 1 from ores_marketdata_market_data_mappings_tbl m
      where m.vendor_instrument_id = v.id
        and m.valid_to = ores_utility_infinity_timestamp_fn()
  );

-- Summary
select 'synthetic_ir_curve_derived_configs' as entity, count(*) as count
from ores_synthetic_ir_curve_generation_configs_tbl
where rate_source <> 'process'
  and valid_to = ores_utility_infinity_timestamp_fn();
