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
 * Provider Feeds: Refinitiv, ICE and Tradeweb
 *
 * Registers three synthetic feeds datasets, one for each provider that has feeds of a kind
 * the platform can simulate today. CME, LSE and Eurex have none yet: the futures, equity and
 * exchange kinds they quote do not exist, and the IR curve config admits term tenors only for
 * the LIBOR and EURIBOR families, so a CME Term SOFR curve cannot be stored. Each is published like every synthetic feeds dataset,
 * by ores_synthetic_publish_feeds_from_dq_fn, but as a system collection: the dataset is a
 * member of the system_core bundle, a publish that names no party, so the feeds are held by
 * the system tenant's system party, sandboxed, and run once there. A tenant sees them and
 * does not own them.
 *
 * One series has one running publisher. A feed's series is its currency, its index tenor and
 * its pillar, or its currency pair, so the datasets below name series that Bloomberg and the
 * 2016 ORE Samples do not, and every provider runs beside the others:
 *   - Refinitiv: spot rates of fourteen cross pairs, which Bloomberg leaves to the
 *     cross-rates matrix. A WM/Refinitiv style composite, started at the level the
 *     Bloomberg USD spots imply.
 *   - ICE: LIBOR curves of the tenors the 2016 ORE Samples leave out: USD 1M and 6M, GBP,
 *     CHF and JPY 3M.
 *   - Tradeweb: dealer composite curves of the six and one month EURIBOR, the one month GBP
 *     and JPY LIBOR and the six month CHF LIBOR.
 *
 * FX spots are a two-component geometric Gaussian mixture, as Bloomberg's: a primary
 * component of weight 0.95 and a per-tick stdev of the pair's annualised vol / 3970.9, at
 * 1800 ticks an hour, and a tail of four times that stdev at weight 0.05. The levels are
 * illustrative, not sourced quotes. IR curves are Cox-Ingersoll-Ross short-rate
 * processes with the seven-pillar template Bloomberg's use.
 *
 * This script is idempotent.
 */

DO $$
declare
    v_provider record;
begin
    for v_provider in
        select * from (values
            ('synthetic.feeds.refinitiv', 'Synthetic Feeds: Refinitiv',
             'The cross rates a simulated Refinitiv quotes: fourteen spot pairs between the major non-USD currencies.'),
            ('synthetic.feeds.ice', 'Synthetic Feeds: ICE Data Services',
             'The LIBOR-family curves a simulated ICE Data Services publishes, for the tenors the 2016 ORE Samples do not cover.'),
            ('synthetic.feeds.tradeweb', 'Synthetic Feeds: Tradeweb',
             'The dealer composite curves a simulated Tradeweb publishes for the EURIBOR and LIBOR tenors the other providers do not.')
        ) as t(code, title, description)
    loop
        perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
            v_provider.code,
            'Synthetic Market Data',
            'Trading',
            'Reference Data',
            'NONE',
            'Primary',
            'Synthetic',
            'Raw',
            'OreStudio Code Generation Methodology',
            v_provider.title,
            v_provider.description,
            'ORESTUDIO',
            'Provider feeds the system collections hold',
            current_date,
            'Internal Use Only',
            'synthetic_feeds'
        );
    end loop;
end $$;

do $$
declare
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_dataset record;
begin
    create temporary table t_provider_fx (
        dataset_code text, provider_code text, provider_name text,
        base text, quote text, start_level double precision, vol_pct double precision
    ) on commit drop;

    -- Levels follow from the Bloomberg USD spots: EUR/USD 1.1707, GBP/USD 1.3576, AUD/USD
    -- 0.7196, NZD/USD 0.5903, USD/CAD 1.3609, USD/CHF 0.7822, USD/JPY 157.68, USD/SEK 9.246
    -- and USD/NOK 9.2402. The vols are approximate, not fitted to a source.
    insert into t_provider_fx values
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'EUR', 'GBP', 0.8624,   6.5),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'EUR', 'JPY', 184.60,   9.5),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'EUR', 'CHF', 0.9158,   5.5),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'EUR', 'AUD', 1.6269,   8.0),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'EUR', 'CAD', 1.5932,   7.0),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'EUR', 'SEK', 10.825,   6.5),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'EUR', 'NOK', 10.817,   7.5),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'GBP', 'JPY', 214.10,  10.5),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'GBP', 'CHF', 1.0620,   7.5),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'AUD', 'JPY', 113.50,  11.5),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'NZD', 'JPY', 93.08,   11.5),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'CHF', 'JPY', 201.60,  10.0),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'CAD', 'JPY', 115.86,  10.0),
        ('synthetic.feeds.refinitiv', 'refinitiv', 'Refinitiv', 'AUD', 'NZD', 1.2190,   6.0);

    create temporary table t_provider_ir (
        dataset_code text, provider_code text, provider_name text,
        currency_code text, index_family text, tenor text,
        annual_kappa double precision, annual_sigma double precision, theta double precision
    ) on commit drop;

    -- dataset, provider, name, currency, index family, index tenor (term families only), annual kappa,
    -- annual sigma, theta (the mean and the start level).
    insert into t_provider_ir values
        ('synthetic.feeds.ice', 'ice', 'ICE Data Services', 'USD', 'libor', '1M', 0.50, 0.007, 0.0430),
        ('synthetic.feeds.ice', 'ice', 'ICE Data Services', 'USD', 'libor', '6M', 0.45, 0.008, 0.0450),
        ('synthetic.feeds.ice', 'ice', 'ICE Data Services', 'GBP', 'libor', '3M', 0.25, 0.012, 0.0470),
        ('synthetic.feeds.ice', 'ice', 'ICE Data Services', 'CHF', 'libor', '3M', 0.30, 0.009, 0.0120),
        ('synthetic.feeds.ice', 'ice', 'ICE Data Services', 'JPY', 'libor', '3M', 0.20, 0.006, 0.0040),
        ('synthetic.feeds.tradeweb', 'tradeweb', 'Tradeweb', 'EUR', 'euribor', '1M', 0.35, 0.009, 0.0300),
        ('synthetic.feeds.tradeweb', 'tradeweb', 'Tradeweb', 'EUR', 'euribor', '6M', 0.35, 0.010, 0.0320),
        ('synthetic.feeds.tradeweb', 'tradeweb', 'Tradeweb', 'GBP', 'libor', '1M', 0.25, 0.012, 0.0460),
        ('synthetic.feeds.tradeweb', 'tradeweb', 'Tradeweb', 'CHF', 'libor', '6M', 0.30, 0.009, 0.0130),
        ('synthetic.feeds.tradeweb', 'tradeweb', 'Tradeweb', 'JPY', 'libor', '1M', 0.20, 0.006, 0.0035);

    for v_dataset in
        select d.id, d.code
        from ores_dq_datasets_tbl d
        where d.tenant_id = v_tenant_id
          and d.code in ('synthetic.feeds.refinitiv', 'synthetic.feeds.ice', 'synthetic.feeds.tradeweb')
          and d.valid_to = ores_utility_infinity_timestamp_fn()
    loop
        if exists (select 1 from ores_dq_fx_spot_generation_configs_artefact_tbl where dataset_id = v_dataset.id)
           or exists (select 1 from ores_dq_ir_curve_generation_configs_artefact_tbl where dataset_id = v_dataset.id) then
            raise debug 'Provider feeds artefact already populated for dataset %', v_dataset.code;
            continue;
        end if;

        insert into ores_dq_fx_spot_generation_configs_artefact_tbl (
            dataset_id, tenant_id, id, version,
            name, description, enabled, auto_start,
            base_currency_code, quote_currency_code,
            gmm_initial_price, ticks_per_hour, process_type,
            price_source, vintage_source, vintage_date, provider_code
        )
        select
            v_dataset.id, v_tenant_id, gen_random_uuid(), 1,
            p.provider_name || ' FX Spot: ' || p.base || '/' || p.quote,
            'Simulated ' || p.provider_name || ' cross rate: a two-component geometric Gaussian mixture at about '
            || p.vol_pct || '% annualised vol, starting at an illustrative 2026 level, not a sourced quote.',
            true, true, p.base, p.quote, p.start_level, 1800, 'geometric',
            'fixed', '', '', p.provider_code
        from t_provider_fx p
        where p.dataset_code = v_dataset.code;

        insert into ores_dq_gmm_components_artefact_tbl (
            dataset_id, tenant_id, base_currency_code, quote_currency_code,
            component_index, description, mean, stdev, weight
        )
        select v_dataset.id, v_tenant_id, p.base, p.quote, c.component_index,
            c.label || ' ~' || p.vol_pct || '% annualised vol.',
            0.0, round((p.vol_pct / 100.0 / 3970.9 * c.scale)::numeric, 7)::double precision, c.weight
        from t_provider_fx p
        cross join (values
            (0, 'Primary component: calibrated to', 1.0, 0.95),
            (1, 'Tail component: four times the primary stdev of', 4.0, 0.05)
        ) as c(component_index, label, scale, weight)
        where p.dataset_code = v_dataset.code;

        insert into ores_dq_ir_curve_generation_configs_artefact_tbl (
            dataset_id, tenant_id, id, version,
            name, description, enabled, auto_start,
            currency_code, index_family, tenor, process_type,
            kappa, theta, sigma, initial_rate,
            ticks_per_hour, fixed_leg_payment_frequency_code,
            price_source, vintage_source, vintage_date, vintage_series_uri, provider_code
        )
        select
            v_dataset.id, v_tenant_id, gen_random_uuid(), 1,
            c.provider_name || ' IR Curve: ' || c.currency_code || '-' || upper(c.index_family) || '-' || c.tenor,
            'Simulated ' || c.provider_name || ' curve on ' || c.currency_code || '-' || upper(c.index_family)
            || '-' || c.tenor || ': a Cox-Ingersoll-Ross short rate, from deposits to a 10Y swap.',
            true, true, c.currency_code, c.index_family, c.tenor, 'COX_INGERSOLL_ROSS',
            c.annual_kappa, c.theta, c.annual_sigma, c.theta,
            60, 'Quarterly',
            'fixed', '', '', '', c.provider_code
        from t_provider_ir c
        where c.dataset_code = v_dataset.code;

        insert into ores_dq_ir_curve_template_entries_artefact_tbl (
            dataset_id, tenant_id, currency_code, index_family, tenor,
            sequence_index, start_tenor_code, end_tenor_code, instrument_code
        )
        select
            v_dataset.id, v_tenant_id, c.currency_code, c.index_family, c.tenor,
            e.sequence_index, e.start_tenor_code, e.end_tenor_code, e.instrument_code
        from t_provider_ir c
        cross join (values
            (0, 'SPOT', '1M', 'DEPO'),
            (1, 'SPOT', '3M', 'DEPO'),
            (2, '3M', '6M', 'FRA'),
            (3, '6M', '12M', 'FRA'),
            (4, 'SPOT', '2Y', 'IRS'),
            (5, 'SPOT', '5Y', 'IRS'),
            (6, 'SPOT', '10Y', 'IRS')
        ) as e(sequence_index, start_tenor_code, end_tenor_code, instrument_code)
        where c.dataset_code = v_dataset.code;
    end loop;
end $$;
