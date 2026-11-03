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
 * A feed's arrival and variance regimes publish as its artefact row states them.
 *
 * synthetic.feeds.uniform_demo runs every pair on the GJR-GARCH daily variance.
 * Inside this transaction GBP/USD is given a Hawkes arrival and authored GARCH
 * parameters that differ from every column default, so a publish that drops a
 * column leaves a default behind and fails here. The authored shape lands on the
 * approved, authored calibration of the pair, which the feed names.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/synthetic_fx_spot_regimes_publish_test.sql
 */

begin;

select plan(5);

create temporary table t_dataset on commit drop as
select id, code from ores_dq_datasets_tbl
where code in ('synthetic.feeds.uniform_demo', 'synthetic.market_data_providers')
  and valid_to = ores_utility_infinity_timestamp_fn();

create temporary table t_party on commit drop as
select id from ores_refdata_read_system_party_fn(ores_utility_system_tenant_id_fn());

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids', '{' || (select id::text from t_party) || '}', true);

create temporary table t_providers_published on commit drop as
select * from ores_synthetic_publish_market_data_providers_from_dq_fn(
    (select id from t_dataset where code = 'synthetic.market_data_providers'),
    ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

update ores_dq_fx_spot_generation_configs_artefact_tbl
set arrival_mode = 'hawkes', branching_ratio = 0.5, decay = 10.0, sensitivity = 2.0,
    garch_alpha = 0.04, garch_gamma = 0.06, garch_beta = 0.88, garch_window_days = 730
where dataset_id = (select id from t_dataset where code = 'synthetic.feeds.uniform_demo')
  and base_currency_code = 'GBP' and quote_currency_code = 'USD';

select lives_ok(
    format($$select * from ores_synthetic_publish_feeds_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)$$,
        (select id from t_dataset where code = 'synthetic.feeds.uniform_demo')),
    'the uniform demo feeds dataset publishes into the system tenant'
);

create temporary table t_feeds on commit drop as
select x.base_currency_code || x.quote_currency_code as pair,
       x.arrival_mode, x.branching_ratio, x.decay, x.sensitivity,
       x.variance_model, x.variance_calibration_id,
       v.method, v.state, v.input_id, v.alpha as garch_alpha, v.gamma as garch_gamma,
       v.beta as garch_beta
from ores_synthetic_fx_spot_generation_configs_tbl x
join ores_synthetic_market_data_generation_configs_tbl c
  on c.id = x.config_id and c.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_synthetic_variance_calibrations_tbl v
  on v.id = x.variance_calibration_id and v.valid_to = ores_utility_infinity_timestamp_fn()
where c.dataset_id = (select id from t_dataset where code = 'synthetic.feeds.uniform_demo')
  and x.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select count(*)::int from t_feeds where variance_model = 'gjr_garch'),
    11,
    'every uniform demo pair scales its volatility by the GJR-GARCH daily variance'
);

select results_eq(
    $$select arrival_mode, branching_ratio, decay, sensitivity from t_feeds where pair = 'GBPUSD'$$,
    $$values ('hawkes'::text, 0.5::double precision, 10.0::double precision, 2.0::double precision)$$,
    'the arrival regime of the artefact row reaches the live feed'
);

select results_eq(
    $$select garch_alpha, garch_gamma, garch_beta from t_feeds where pair = 'GBPUSD'$$,
    $$values (0.04::double precision, 0.06::double precision, 0.88::double precision)$$,
    'the GJR-GARCH shape of the artefact row reaches the calibration the feed runs'
);

select is(
    (select count(*)::int from t_feeds
     where method = 'authored' and state = 'approved' and input_id is null),
    11,
    'every pair runs an approved authored calibration, named by its feed'
);

select * from finish();

rollback;
