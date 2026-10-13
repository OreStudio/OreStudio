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
 * A synthetic feeds dataset publishes as one collection of feeds.
 *
 * synthetic.feeds.bloomberg publishes through the one publish path for
 * synthetic feeds, into the party it names. Every feed row names the
 * bloomberg provider, so the collection stands for Bloomberg and every feed's
 * source name starts synthetic.bloomberg. The pairs the Federal Reserve H.10
 * release quotes start from its vintage; the others start at a fixed level.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/synthetic_feeds_publish_test.sql
 */

begin;

select plan(10);

create temporary table t_dataset on commit drop as
select id, code from ores_dq_datasets_tbl
where code in ('synthetic.feeds.bloomberg', 'synthetic.market_data_providers')
  and valid_to = ores_utility_infinity_timestamp_fn();

create temporary table t_party on commit drop as
select id from ores_refdata_read_system_party_fn(ores_utility_system_tenant_id_fn());

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids', '{' || (select id::text from t_party) || '}', true);

-- A database the system provisioning step has not run on holds no
-- providers yet, and the collection's provider is validated against them.
create temporary table t_providers_published on commit drop as
select * from ores_synthetic_publish_market_data_providers_from_dq_fn(
    (select id from t_dataset where code = 'synthetic.market_data_providers'),
    ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

select lives_ok(
    format($$select * from ores_synthetic_publish_feeds_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'upsert', jsonb_build_object('party_id', %L))$$,
        (select id from t_dataset where code = 'synthetic.feeds.bloomberg'),
        (select id from t_party)),
    'the Bloomberg feeds dataset publishes into the party it names'
);

create temporary table t_collection on commit drop as
select c.id, c.scope, c.binding_mode, c.provider_code, c.name
from ores_synthetic_market_data_generation_configs_tbl c
where c.tenant_id = ores_utility_system_tenant_id_fn()
  and c.party_id = (select id from t_party)
  and c.dataset_id = (select id from t_dataset where code = 'synthetic.feeds.bloomberg')
  and c.valid_to = ores_utility_infinity_timestamp_fn();

select results_eq(
    $$select scope, binding_mode, provider_code, name from t_collection$$,
    $$values ('party'::text, 'bound'::text, 'bloomberg'::text, 'Bloomberg'::text)$$,
    'the dataset publishes one bound party collection that stands for Bloomberg'
);

select is(
    (select count(*)::int from ores_synthetic_fx_spot_generation_configs_tbl
     where config_id = (select id from t_collection)
       and source_name like 'synthetic.bloomberg.%'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    26,
    'the collection holds a USD spot for each of the 26 currencies, named for Bloomberg'
);

select is(
    (select count(*)::int from ores_synthetic_fx_spot_generation_configs_tbl
     where config_id = (select id from t_collection)
       and price_source = 'vintage'
       and vintage_source = 'fed.h10.2026-05-05'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    13,
    'the 13 pairs the H.10 release quotes start from its 2026-05-05 vintage'
);

select is(
    (select count(*)::int from ores_synthetic_fx_spot_generation_configs_tbl
     where config_id = (select id from t_collection)
       and price_source = 'fixed'
       and gmm_initial_price > 0
       and valid_to = ores_utility_infinity_timestamp_fn()),
    13,
    'the other 13 pairs start at a fixed level'
);

select is(
    (select count(*)::int from ores_synthetic_ir_curve_generation_configs_tbl
     where config_id = (select id from t_collection)
       and source_name like 'synthetic.bloomberg.%'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    29,
    'the collection holds a curve for each of the 28 currencies and the USD FOMC short end'
);

select is(
    (select count(*)::int from ores_synthetic_fx_spot_generation_configs_tbl
     where config_id = (select id from t_collection)
       and asset_class_code = 'fx'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    26,
    'every FX feed belongs to the fx asset class'
);

select is(
    (select count(*)::int from ores_synthetic_ir_curve_generation_configs_tbl
     where config_id = (select id from t_collection)
       and asset_class_code = 'interest_rates'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    29,
    'every curve feed belongs to the interest_rates asset class'
);

-- A feed added by hand names no folder: it is placed in the folder its kind uses.
insert into ores_synthetic_fx_spot_generation_configs_tbl (
    id, tenant_id, version, party_id, config_id, base_currency_code, quote_currency_code,
    source_name, oresmd_uri, price_source, gmm_initial_price, ticks_per_hour, process_type,
    enabled, auto_start, vintage_source, vintage_date, asset_class_code,
    modified_by, performed_by, change_reason_code, change_commentary
) select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, (select id from t_party),
    (select id from t_collection), 'EUR', 'JPY', 'synthetic.bloomberg.placement.eurjpy',
    'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=JPY', 'fixed', 160, 60,
    'geometric', false, false, '', '', 'fx',
    current_user, current_user, 'system.test', 'placement test';

select is(
    (select f.name || ' > ' || a.name || ' > ' || c.name
     from ores_synthetic_fx_spot_generation_configs_tbl x
     join ores_synthetic_folders_tbl f on f.id = x.folder_id and f.valid_to = ores_utility_infinity_timestamp_fn()
     join ores_synthetic_folders_tbl a on a.id = f.parent_id and a.valid_to = ores_utility_infinity_timestamp_fn()
     join ores_synthetic_folders_tbl c on c.id = a.parent_id and c.valid_to = ores_utility_infinity_timestamp_fn()
     where x.source_name = 'synthetic.bloomberg.placement.eurjpy'
       and x.valid_to = ores_utility_infinity_timestamp_fn()),
    'FX Rates > FX > Bloomberg',
    'a feed added by hand lands in the FX Rates folder of its collection'
);

-- A collection made by hand has no folders; its first feed makes them.
insert into ores_synthetic_market_data_generation_configs_tbl (
    id, tenant_id, version, party_id, scope, binding_mode, name, description, enabled,
    dataset_id, provider_code, modified_by, performed_by, change_reason_code, change_commentary
) select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, (select id from t_party),
    'party', 'sandboxed', 'Placement Test', 'test', true, null, 'ore',
    current_user, current_user, 'system.test', 'placement test';

insert into ores_synthetic_ir_curve_generation_configs_tbl (
    id, tenant_id, version, party_id, config_id, currency_code, index_family, tenor, role,
    process_type, ticks_per_hour, enabled, auto_start, price_source, vintage_source, vintage_date,
    vintage_series_uri, description, fixed_leg_payment_frequency_code, source_name, asset_class_code,
    modified_by, performed_by, change_reason_code, change_commentary
) select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, (select id from t_party),
    (select id from ores_synthetic_market_data_generation_configs_tbl
     where name = 'Placement Test' and valid_to = ores_utility_infinity_timestamp_fn()),
    'USD', 'sofr', '', 'self_discounting', 'VASICEK', 60, true, false, 'fixed', '', '', '',
    'placement test', 'Annual', 'synthetic.placementtest.usdsofr', 'interest_rates',
    current_user, current_user, 'system.test', 'placement test';

select is(
    (select f.name || ' > ' || a.name || ' > ' || c.name
     from ores_synthetic_ir_curve_generation_configs_tbl x
     join ores_synthetic_folders_tbl f on f.id = x.folder_id and f.valid_to = ores_utility_infinity_timestamp_fn()
     join ores_synthetic_folders_tbl a on a.id = f.parent_id and a.valid_to = ores_utility_infinity_timestamp_fn()
     join ores_synthetic_folders_tbl c on c.id = a.parent_id and c.valid_to = ores_utility_infinity_timestamp_fn()
     where x.source_name = 'synthetic.placementtest.usdsofr'
       and x.valid_to = ores_utility_infinity_timestamp_fn()),
    'IR Curves > Rates > Placement Test',
    'the first feed of a collection made by hand makes its folders'
);

select * from finish();

rollback;
