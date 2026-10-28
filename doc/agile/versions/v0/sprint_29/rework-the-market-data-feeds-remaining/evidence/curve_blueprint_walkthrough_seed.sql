/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Seeds the Acme tenant with one curve blueprint, the market series it
 * publishes into and its nine pillars, so the walk-through has a record of
 * the party it signs in as.
 *
 * The Acme provisioning seeds no curve blueprint and no interest-rate series,
 * and a session reads only its own tenant's rows, so the system tenant's
 * seeded FOMC recipe is copied into the Acme tenant rather than read there.
 * This is environment data for the walk-through, not a product seed.
 */

-- The tenant and the party it acts in are read, not written here: a
-- provisioned environment mints its own ids.
select t.id as tenant_id, p.id as party_id
from ores_iam_tenants_tbl t
join ores_refdata_parties_tbl p
  on p.tenant_id = t.id
 and p.valid_to = ores_utility_infinity_timestamp_fn()
where t.code = 'acme_corporation'
  and t.valid_to = ores_utility_infinity_timestamp_fn()
  and p.short_code = 'acme_corporation_system'
\gset

\set series '''a1a1a1a1-0000-4000-8000-000000000001'''
\set config '''b1b1b1b1-0000-4000-8000-000000000001'''

insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, oresmd_uri, provider_code, series_subclass,
    derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to)
select :series, :'tenant_id', 0, :'party_id', s.oresmd_uri, s.provider_code, s.series_subclass,
       s.derivation_kind, ores_utility_nil_uuid_fn(), 0,
       'tenant_admin', 'tenant_admin', 'system.initial_load',
       'Walk-through seed for the curve blueprint screens',
       current_timestamp, ores_utility_infinity_timestamp_fn()
from ores_marketdata_market_series_tbl s
where s.id = 'f2d3e4a5-6c7d-4e8f-8a9b-1c2d3e4f5061'
  and s.tenant_id = ores_utility_system_tenant_id_fn()
  and s.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_marketdata_market_series_tbl o
      where o.tenant_id = :'tenant_id' and o.id = :series
        and o.valid_to = ores_utility_infinity_timestamp_fn());

-- A second curve series the desk holds and no blueprint claims yet, which is
-- what the Add dialog's picker offers and the write takes.
insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, oresmd_uri, provider_code, series_subclass,
    derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to)
select 'a2a2a2a2-0000-4000-8000-000000000002', :'tenant_id', 0, :'party_id',
       'oresmd://ir/EUR?type=series&instrument=discount&quote=rate&curve_id=EUR-EURIBOR-6M',
       'ore', 'yield', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
       'tenant_admin', 'tenant_admin', 'system.new_record',
       'A curve series the desk holds for the EUR curve',
       current_timestamp, ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_marketdata_market_series_tbl o
    where o.tenant_id = :'tenant_id'
      and o.id = 'a2a2a2a2-0000-4000-8000-000000000002'
      and o.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, output_series_id, party_id, currency_code, index_tenor,
    curve_family_role, discount_curve_config_id, interpolation_method,
    day_count_convention, tenor_convention_code, split_tenor_code,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to)
select :config, :'tenant_id', 0, :series, :'party_id', c.currency_code, c.index_tenor,
       c.curve_family_role, ores_utility_nil_uuid_fn(), c.interpolation_method,
       c.day_count_convention, c.tenor_convention_code, c.split_tenor_code,
       'tenant_admin', 'tenant_admin', 'system.initial_load',
       'Walk-through seed for the curve blueprint screens',
       current_timestamp, ores_utility_infinity_timestamp_fn()
from ores_refdata_ir_curve_bootstrap_configs_tbl c
where c.id = 'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f'
  and c.tenant_id = ores_utility_system_tenant_id_fn()
  and c.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_refdata_ir_curve_bootstrap_configs_tbl o
      where o.tenant_id = :'tenant_id' and o.id = :config
        and o.valid_to = ores_utility_infinity_timestamp_fn());

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to)
select ('c1c1c1c1-0000-4000-8000-00000000000' || p.sequence_index)::uuid,
       :'tenant_id', 0, :'party_id', :config, p.sequence_index,
       p.start_tenor_code, p.end_tenor_code, p.curve_role_code,
       'tenant_admin', 'tenant_admin', 'system.initial_load',
       'Walk-through seed for the curve blueprint screens',
       current_timestamp, ores_utility_infinity_timestamp_fn()
from ores_refdata_ir_curve_bootstrap_pillars_tbl p
where p.bootstrap_config_id = 'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f'
  and p.tenant_id = ores_utility_system_tenant_id_fn()
  and p.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_refdata_ir_curve_bootstrap_pillars_tbl o
      where o.tenant_id = :'tenant_id'
        and o.id = ('c1c1c1c1-0000-4000-8000-00000000000' || p.sequence_index)::uuid
        and o.valid_to = ores_utility_infinity_timestamp_fn())
order by p.sequence_index;
