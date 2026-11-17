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
 * Synthetic Feeds Dataset: Bloomberg, Volatility Surfaces
 *
 * Adds the latent FX volatility surfaces to the synthetic.feeds.bloomberg
 * dataset, so they publish with its spots and curves. Nothing here lists a
 * pair or an expiry: the surfaces are the pairs the published FX volatility
 * surface blueprints calibrate and that this dataset's own spot feeds quote,
 * at the expiries those blueprints' pillars name. A cross the dataset quotes
 * no spot for has no latent spot to strike a surface against, so it is left
 * for the task that quotes crosses consistently with their legs.
 *
 * Each surface starts at its pair's spot volatility: the primary component of
 * the pair's spot mixture has a per-tick stdev of the annual vol over the square
 * root of the ticks in a year, read from the spot feed's own ticks per hour, so
 * every expiry's starting ATM vol is that stdev times that root, and the surface
 * and the spot it is struck against start from one level.
 *
 * The blueprints are read across datasets, because they live in the refdata
 * dataset; there is one blueprint per pair, so a pair's expiries are its one
 * blueprint's pillars. The dynamics are the models' documented defaults.
 *
 * One contributor quotes the surfaces for now, as the provider the dataset
 * stands for; the vendors the curves are quoted by, and a clean contributor no
 * tenant subscribes to, come with the task that quotes through them.
 *
 * Ids are derived from names, so the publish writes new versions of the same
 * rows on a second run. This runs after the refdata layer, which stages the
 * blueprints. This script is idempotent.
 */

do $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_contributor_id uuid := md5('synthetic.feeds.bloomberg|fx_vol_contributor|bloomberg')::uuid;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'synthetic.feeds.bloomberg'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: synthetic.feeds.bloomberg';
    end if;

    if exists (
        select 1 from ores_dq_fx_vol_surface_feeds_artefact_tbl where dataset_id = v_dataset_id
    ) then
        raise debug 'Bloomberg volatility surfaces already populated for dataset %', v_dataset_id;
        return;
    end if;

    create temporary table t_vol_pairs (
        base text, quote text, feed_id uuid, atm_vol double precision
    ) on commit drop;

    insert into t_vol_pairs (base, quote, feed_id, atm_vol)
    select distinct b.base_currency_code, b.quote_currency_code,
           md5('synthetic.feeds.bloomberg|fx_vol_surface_feed|' || b.base_currency_code || b.quote_currency_code)::uuid,
           g.stdev * sqrt(f.ticks_per_hour * 24.0 * 365.0)
    from ores_dq_fx_vol_surface_blueprints_artefact_tbl b
    join ores_dq_fx_spot_generation_configs_artefact_tbl f
      on f.dataset_id = v_dataset_id
     and f.base_currency_code = b.base_currency_code
     and f.quote_currency_code = b.quote_currency_code
    join ores_dq_gmm_components_artefact_tbl g
      on g.dataset_id = v_dataset_id
     and g.base_currency_code = b.base_currency_code
     and g.quote_currency_code = b.quote_currency_code
     and g.component_index = 0;

    if not exists (select 1 from t_vol_pairs) then
        raise exception 'No blueprint pair has a Bloomberg spot feed: the blueprints must be staged first.';
    end if;

    insert into ores_dq_fx_vol_contributors_artefact_tbl (
        dataset_id, tenant_id, id, version, provider_code, noise, wing_widening,
        expiry_widening, quote_step, missing_probability, stale_probability,
        outlier_probability, outlier_size, bias
    ) values (
        v_dataset_id, v_tenant_id, v_contributor_id, 0, 'bloomberg',
        0.002, 1.0, 0.5, 0.0005, 0.02, 0.05, 0.001, 0.01, 0.0
    );

    insert into ores_dq_fx_vol_surface_feeds_artefact_tbl (
        dataset_id, tenant_id, id, version, base_currency_code, quote_currency_code,
        provider_code, ticks_per_hour, enabled, auto_start, arrival_mode, branching_ratio,
        decay, sensitivity, skew_reversion, skew_volatility, curvature_reversion,
        curvature_volatility, spot_variance_correlation, spot_skew_correlation,
        segment_correlation, start_skew, start_curvature
    )
    select v_dataset_id, v_tenant_id, p.feed_id, 0, p.base, p.quote,
           'bloomberg', 12, true, true, 'periodic', 0.0,
           1.0, 1.0, 3.0, 0.6, 3.0,
           0.4, 0.3, -0.4,
           0.9, -0.1, 0.8
    from t_vol_pairs p;

    insert into ores_dq_fx_vol_surface_feed_expiries_artefact_tbl (
        dataset_id, tenant_id, id, version, feed_id, tenor, atm_vol,
        variance_reversion, variance_volatility
    )
    select distinct v_dataset_id, v_tenant_id,
           md5('synthetic.feeds.bloomberg|fx_vol_surface_feed_expiry|' || p.base || p.quote
               || '|' || x.expiry_code)::uuid,
           0, p.feed_id, x.expiry_code, p.atm_vol, 2.0, 0.08
    from t_vol_pairs p
    join ores_dq_fx_vol_surface_blueprints_artefact_tbl b
      on b.base_currency_code = p.base and b.quote_currency_code = p.quote
    join ores_dq_fx_vol_surface_pillars_artefact_tbl x
      on x.dataset_id = b.dataset_id and x.blueprint_id = b.id;

    insert into ores_dq_fx_vol_surface_feed_contributors_artefact_tbl (
        dataset_id, tenant_id, id, version, feed_id, contributor_id
    )
    select v_dataset_id, v_tenant_id,
           md5('synthetic.feeds.bloomberg|fx_vol_surface_feed_contributor|' || p.base || p.quote
               || '|bloomberg')::uuid,
           0, p.feed_id, v_contributor_id
    from t_vol_pairs p;
end $$;
