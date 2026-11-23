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
 * The source names of the synthetic feeds, by the naming scheme:
 * synthetic.<source>.<asset_class>.<instrument>.<subject>, spelled from oresmd.
 * One function for each kind of feed, so the publish and the datasets derive a
 * name one way. See doc/knowledge/domain/synthetic_feed_naming_scheme.org.
 */

-- A vendor's FX spot: synthetic.bloomberg.fx.fx_spot.audusd.
create or replace function ores_synthetic_fx_spot_name_fn(
    p_source text, p_base text, p_quote text
) returns text as $$
    select 'synthetic.' || lower(p_source) || '.fx.fx_spot.' || lower(p_base || p_quote);
$$ language sql immutable;

-- A vendor's yield curve, the index before the currency so the curves of one
-- index sort together, and the index tenor last when it has one:
-- synthetic.bloomberg.ir.yield_curve.sofr.usd, synthetic.ice.ir.yield_curve.libor.gbp.3m.
create or replace function ores_synthetic_yield_curve_name_fn(
    p_source text, p_index_family text, p_currency text, p_tenor text
) returns text as $$
    select 'synthetic.' || lower(p_source) || '.ir.yield_curve.' || lower(p_index_family) ||
        '.' || lower(p_currency) ||
        coalesce('.' || nullif(lower(replace(coalesce(p_tenor, ''), ' ', '')), ''), '');
$$ language sql immutable;

-- A vol surface of a pair; a latent surface's source is the latent provider:
-- synthetic.latent.fx.fx_vol_surface.audusd.
create or replace function ores_synthetic_fx_vol_surface_name_fn(
    p_source text, p_base text, p_quote text
) returns text as $$
    select 'synthetic.' || lower(p_source) || '.fx.fx_vol_surface.' || lower(p_base || p_quote);
$$ language sql immutable;

-- One vendor's quotes of a latent surface, in the vendor's namespace:
-- synthetic.bloomberg.fx.fx_option.audusd.
create or replace function ores_synthetic_fx_option_view_name_fn(
    p_provider text, p_base text, p_quote text
) returns text as $$
    select 'synthetic.' || lower(p_provider) || '.fx.fx_option.' || lower(p_base || p_quote);
$$ language sql immutable;
