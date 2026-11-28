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
 * The Series the Computed Term Indices Are Stated In
 *
 * Each term index the run computes has one series, and the series says so: its
 * derivation_kind is =IR_TERM_FIXING= and it names the generation config and
 * version that produced it. The kind is not =IR_CURVE_BOOTSTRAP=: the rate is
 * compounded from the overnight curve, not bootstrapped from an instrument grid,
 * and an =IR_CURVE_BOOTSTRAP= config id resolves to a refdata blueprint, so one
 * kind would name two tables. Without this row the ingest would create the
 * series from the first fixing it stores and leave it =OBSERVED=, because a
 * fixing tick states a value and not how the value was made.
 *
 * The series is seeded here rather than written by the feed because the config
 * declares the index and the run computes it: the same shape as the curve
 * series the bootstrap republish stamps, and the reason the ingest finds the
 * series already there.
 *
 * The index is the administrator's, so the series is the administrator's, and
 * the derivation is the only mark of how the value was made. See
 * doc/knowledge/architecture/synthetic_generation_of_fixings.org.
 *
 * This script is idempotent.
 */

\echo '--- Series of the Computed Term Indices ---'

create temporary table t_computed_fixing_series (
    provider_code text not null,
    oresmd_uri text primary key,
    derivation_config_id uuid not null,
    derivation_config_version integer not null
);

insert into t_computed_fixing_series (
    provider_code, oresmd_uri, derivation_config_id, derivation_config_version
)
select g.provider_code, i.item, c.id, c.version
from (values
    ('cme',    'oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&tenor=1M'),
    ('banrep', 'oresmd://ir/COP?type=fixing&index=ibor&name=IBR&tenor=3M'),
    ('gpw',    'oresmd://ir/PLN?type=fixing&index=ibor&name=WIBOR&tenor=6M')
) as i(provider_code, item)
join ores_synthetic_market_data_generation_configs_tbl g
  on g.provider_code = i.provider_code
 and g.valid_to = ores_utility_infinity_timestamp_fn()
join ores_synthetic_ir_curve_generation_configs_tbl c
  on c.config_id = g.id
 and c.valid_to = ores_utility_infinity_timestamp_fn()
join ores_dq_ir_curve_generation_configs_artefact_tbl a
  on a.dataset_id = g.dataset_id
 and a.provider_code = i.provider_code
 and c.currency_code = a.currency_code
 and c.index_family = a.index_family
 and c.tenor = a.tenor;

-- compass:except(single_writer/second_writer) expires=2027-05-18 approver=marco reason=the populate script seeds the system tenant's series for the computed index, and the DQ publish writes a tenant's own series when it publishes a collection: the two write different tenants' rows
insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, series_subclass, oresmd_uri,
    provider_code, derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary
)
select md5('computed-fixing|' || s.provider_code || '|' || s.oresmd_uri)::uuid,
       ores_utility_system_tenant_id_fn(), 0,
       ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
       'index_fixing', s.oresmd_uri, s.provider_code,
       'IR_TERM_FIXING', s.derivation_config_id, s.derivation_config_version,
       current_user, current_user, 'system.initial_load',
       'The series the platform states the term index of ' || s.provider_code ||
           ' in, computed from the run''s own overnight curve'
from t_computed_fixing_series s
where not exists (
    select 1 from ores_marketdata_market_series_tbl x
    where x.tenant_id = ores_utility_system_tenant_id_fn()
      and x.oresmd_uri = s.oresmd_uri
      and x.provider_code = s.provider_code
      and x.valid_to = ores_utility_infinity_timestamp_fn()
);

-- compass:except(single_writer/second_writer) expires=2027-05-18 approver=marco reason=the asset class row belongs to the series this script seeds, and the DQ publish writes the classes of the series it creates: the two write different tenants' rows
insert into ores_marketdata_market_series_asset_classes_tbl (
    tenant_id, version, market_series_id, asset_class_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
select ores_utility_system_tenant_id_fn(), 0, x.id, 'interest_rates',
       current_user, current_user, 'system.initial_load',
       'The computed term indices are an interest rate series'
from t_computed_fixing_series s
join ores_marketdata_market_series_tbl x
  on x.oresmd_uri = s.oresmd_uri
 and x.provider_code = s.provider_code
 and x.tenant_id = ores_utility_system_tenant_id_fn()
 and x.valid_to = ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_marketdata_market_series_asset_classes_tbl c
    where c.tenant_id = ores_utility_system_tenant_id_fn()
      and c.market_series_id = x.id
      and c.asset_class_code = 'interest_rates'
      and c.valid_to = ores_utility_infinity_timestamp_fn()
);

drop table t_computed_fixing_series;

-- Summary
select 'computed_fixing_series' as entity, count(*) as count
from ores_marketdata_market_series_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
  and derivation_kind = 'IR_TERM_FIXING'
  and oresmd_uri like 'oresmd://ir/%type=fixing%';
