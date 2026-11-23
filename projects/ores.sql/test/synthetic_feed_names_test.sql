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
 * The synthetic feed names follow the naming scheme, and no two feeds of the system tenant
 * derive one name, whatever their kind: a name is an address, so two feeds under it would
 * publish to one subject. See doc/knowledge/domain/synthetic_feed_naming_scheme.org.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/synthetic_feed_names_test.sql
 */

begin;

select plan(9);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_names on commit drop as
select 'fx_spot' as kind, source_name from ores_synthetic_fx_spot_generation_configs_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'yield_curve', source_name from ores_synthetic_ir_curve_generation_configs_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'fx_vol_surface', source_name from ores_synthetic_fx_vol_surface_feeds_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

select ok((select count(*) from t_names) > 0, 'the system tenant holds published feeds');

select is(
    (select count(*)::int from (select source_name from t_names group by source_name having count(*) > 1) d),
    0,
    'no two feeds derive one name'
);

select is(
    (select count(*)::int from t_names
     where source_name !~ ('^synthetic\.[a-z0-9_]+\.(fx|ir)\.' || kind || '\.')),
    0,
    'every name is synthetic.<source>.<asset_class>.<instrument>.<subject>'
);

select is(
    ores_synthetic_fx_spot_name_fn('bloomberg', 'AUD', 'USD'),
    'synthetic.bloomberg.fx.fx_spot.audusd',
    'a spot is named by its pair'
);

select is(
    ores_synthetic_yield_curve_name_fn('ice', 'LIBOR', 'CHF', '3 M'),
    'synthetic.ice.ir.yield_curve.libor.chf.3m',
    'a curve is named index first, its tenor last'
);

select is(
    ores_synthetic_fx_vol_surface_name_fn('latent', 'AUD', 'USD'),
    'synthetic.latent.fx.fx_vol_surface.audusd',
    'a latent surface carries no vendor in its name'
);

select is(
    ores_synthetic_fx_option_view_name_fn('bloomberg', 'AUD', 'USD'),
    'synthetic.bloomberg.fx.fx_option.audusd',
    'a vendor view is named in its vendor namespace, as the C++ twin names it'
);

select is(
    (select count(*)::int from ores_synthetic_fx_vol_surface_feeds_tbl s
     where s.valid_to = ores_utility_infinity_timestamp_fn()
       and not exists (select 1 from ores_synthetic_ir_curve_generation_configs_tbl c
                       where c.source_name = s.base_curve_name
                         and c.valid_to = ores_utility_infinity_timestamp_fn())),
    0,
    'every surface names a base curve that a feed publishes'
);

select is(
    (select count(*)::int from ores_synthetic_fx_vol_surface_feeds_tbl s
     where s.valid_to = ores_utility_infinity_timestamp_fn()
       and not exists (select 1 from ores_synthetic_ir_curve_generation_configs_tbl c
                       where c.source_name = s.quote_curve_name
                         and c.valid_to = ores_utility_infinity_timestamp_fn())),
    0,
    'every surface names a quote curve that a feed publishes'
);

select * from finish();
rollback;
