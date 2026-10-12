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
 * Synthetic Feeds Dataset: Bloomberg
 *
 * Registers the synthetic.feeds.bloomberg dataset: the FX spots and the
 * interest rate curves a simulated Bloomberg quotes. It is published like
 * every synthetic feeds dataset, by ores_synthetic_publish_feeds_from_dq_fn,
 * into the party the tenant's provisioning names. Every feed row names the
 * bloomberg provider, so its source name starts synthetic.bloomberg and the
 * collection stands for Bloomberg.
 *
 * Coverage is what the ACME reports need (capture E2FA0F12, "The market data
 * feeds the ACME reports need"): every currency of an ACME book has a USD
 * spot and a curve. Crosses come from the cross-rates matrix, not from feeds
 * of their own. One vendor quotes every pair, as a bank's single market data
 * source does.
 *
 * FX spots: one geometric two-component Gaussian mixture per pair. The
 * primary component has weight 0.95 and a per-tick stdev of the pair's
 * annualised vol / 3970.9, at 1800 ticks an hour (sqrt(1800 * 24 * 365) ~=
 * 3970.9). The tail component has weight 0.05 and four times that stdev.
 *   - The 13 pairs the Federal Reserve H.10 release quotes start from the
 *     2026-05-05 H.10 vintage (marketdata_fx_driver_rates_2026_populate.sql),
 *     read at feed start.
 *   - The other 13 start at a fixed, illustrative level near the 2026
 *     market. They are not sourced quotes.
 * The vols are approximate 2026 realised vols, not fitted to a source. The
 * HKD and CNH vols are low because the HKMA pegs HKD to USD inside a band
 * and the PBoC manages CNY, which CNH tracks.
 *
 * IR curves: one Cox-Ingersoll-Ross short-rate process per currency on its
 * overnight benchmark, with a seven-pillar template (deposits to a 10Y swap),
 * and the USD FOMC-dated short end. The calibration is per currency:
 * emerging markets revert slower, with a higher level and vol.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.feeds.bloomberg',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic Feeds: Bloomberg',
        'The FX spots and interest rate curves a simulated Bloomberg quotes: a USD spot and an overnight-benchmark curve for every currency the ACME books trade.',
        'ORESTUDIO',
        'Bloomberg feeds for the Synthetic data collections bundle',
        current_date,
        'Internal Use Only',
        'synthetic_feeds'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

do $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
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
        select 1 from ores_dq_synthetic_fx_spot_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Synthetic feeds (Bloomberg) artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating synthetic feeds (Bloomberg) for dataset: synthetic.feeds.bloomberg';

    create temporary table t_bloomberg_fx (
        base text, quote text, start_level double precision, vol_pct double precision, sourced boolean
    ) on commit drop;

    -- Market quoting order: EUR, GBP, AUD and NZD quote against USD; USD
    -- quotes against every other currency.
    insert into t_bloomberg_fx values
        -- Federal Reserve H.10, 2026-05-05: the level is the vintage's, read
        -- at feed start; it is kept here only as the record of the value.
        ('EUR', 'USD', 1.1707,   8.5,  true),
        ('GBP', 'USD', 1.3576,   9.5,  true),
        ('AUD', 'USD', 0.7196,   11.0, true),
        ('NZD', 'USD', 0.5903,   11.5, true),
        ('USD', 'CAD', 1.3609,   8.0,  true),
        ('USD', 'CHF', 0.7822,   8.5,  true),
        ('USD', 'DKK', 6.3827,   8.5,  true),
        ('USD', 'INR', 95.3200,  6.5,  true),
        ('USD', 'JPY', 157.6800, 11.5, true),
        ('USD', 'MXN', 17.3741,  14.0, true),
        ('USD', 'NOK', 9.2402,   10.0, true),
        ('USD', 'SEK', 9.2460,   9.5,  true),
        ('USD', 'ZAR', 16.6371,  17.5, true),
        -- Illustrative levels near the 2026 market, not sourced quotes.
        ('USD', 'HKD', 7.8300,   1.0,  false),
        ('USD', 'SGD', 1.2900,   5.0,  false),
        ('USD', 'CNH', 7.1500,   4.5,  false),
        ('USD', 'KRW', 1440.00,  9.0,  false),
        ('USD', 'TWD', 30.500,   6.0,  false),
        ('USD', 'PLN', 3.6500,   10.0, false),
        ('USD', 'CZK', 21.200,   9.5,  false),
        ('USD', 'HUF', 330.00,   11.5, false),
        ('USD', 'TRY', 44.000,   12.0, false),
        ('USD', 'ILS', 3.6000,   9.0,  false),
        ('USD', 'BRL', 5.4500,   15.0, false),
        ('USD', 'CLP', 940.00,   13.0, false),
        ('USD', 'COP', 4100.0,   14.0, false);

    insert into ores_dq_synthetic_fx_spot_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled, auto_start,
        base_currency_code, quote_currency_code,
        gmm_initial_price, ticks_per_hour, process_type,
        price_source, vintage_source, vintage_date, provider_code
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Bloomberg FX Spot: ' || p.base || '/' || p.quote,
        'Simulated Bloomberg FX spot: a two-component geometric Gaussian mixture at about '
        || p.vol_pct || '% annualised vol, starting at '
        || case when p.sourced then 'the 2026-05-05 Federal Reserve H.10 vintage.'
                else 'an illustrative 2026 level, not a sourced quote.' end,
        true, true, p.base, p.quote,
        case when p.sourced then 0 else p.start_level end, 1800, 'geometric',
        case when p.sourced then 'vintage' else 'fixed' end,
        case when p.sourced then 'fed.h10.2026-05-05' else '' end,
        case when p.sourced then '2026-05-05' else '' end,
        'bloomberg'
    from t_bloomberg_fx p;

    insert into ores_dq_synthetic_gmm_components_artefact_tbl (
        dataset_id, tenant_id, base_currency_code, quote_currency_code,
        component_index, description, mean, stdev, weight
    )
    select v_dataset_id, v_tenant_id, p.base, p.quote, c.component_index,
        c.label || ' ~' || p.vol_pct || '% annualised vol.',
        0.0, round((p.vol_pct / 100.0 / 3970.9 * c.scale)::numeric, 7)::double precision, c.weight
    from t_bloomberg_fx p
    cross join (values
        (0, 'Primary component: calibrated to', 1.0, 0.95),
        (1, 'Tail component: four times the primary stdev of', 4.0, 0.05)
    ) as c(component_index, label, scale, weight);

    create temporary table t_bloomberg_ir (
        currency_code text, index_family text,
        annual_kappa double precision, annual_sigma double precision, theta double precision
    ) on commit drop;

    -- currency, overnight benchmark, annual kappa, annual sigma, theta (the
    -- mean and the start level).
    insert into t_bloomberg_ir values
        ('USD', 'sofr',    0.55, 0.008, 0.0400),
        ('EUR', 'estr',    0.35, 0.010, 0.0300),
        ('JPY', 'tona',    0.20, 0.006, 0.0025),
        ('GBP', 'sonia',   0.25, 0.013, 0.0450),
        ('CHF', 'saron',   0.30, 0.009, 0.0100),
        ('AUD', 'aonia',   0.40, 0.012, 0.0430),
        ('CAD', 'corra',   0.45, 0.011, 0.0350),
        ('CNY', 'shibor',  0.20, 0.006, 0.0180),
        ('HKD', 'honia',   0.50, 0.009, 0.0450),
        ('SGD', 'sora',    0.35, 0.010, 0.0300),
        ('SEK', 'swestr',  0.30, 0.012, 0.0250),
        ('NOK', 'nowa',    0.30, 0.013, 0.0400),
        ('NZD', 'nzonia',  0.35, 0.014, 0.0400),
        ('KRW', 'kofr',    0.30, 0.011, 0.0280),
        ('INR', 'mibor',   0.30, 0.018, 0.0650),
        ('MXN', 'tiie',    0.25, 0.022, 0.1000),
        ('ZAR', 'zaronia', 0.25, 0.020, 0.0750),
        ('DKK', 'destr',   0.35, 0.010, 0.0280),
        ('PLN', 'polonia', 0.28, 0.016, 0.0550),
        ('TWD', 'taibor',  0.30, 0.008, 0.0200),
        -- CNH HIBOR: the offshore renminbi fixing in Hong Kong, more volatile
        -- than the onshore SHIBOR.
        ('CNH', 'hibor',   0.35, 0.015, 0.0180),
        ('CZK', 'czeonia', 0.30, 0.012, 0.0350),
        ('HUF', 'buboron', 0.28, 0.018, 0.0650),
        ('ILS', 'telboron', 0.30, 0.012, 0.0425),
        ('TRY', 'tlref',   0.20, 0.050, 0.3800),
        ('BRL', 'cdi',     0.25, 0.025, 0.1400),
        ('CLP', 'camara',  0.30, 0.015, 0.0450),
        ('COP', 'ibr',     0.25, 0.018, 0.0900);

    insert into ores_dq_synthetic_ir_curve_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled, auto_start,
        currency_code, index_family, tenor, process_type,
        kappa, theta, sigma, initial_rate,
        ticks_per_hour, fixed_leg_payment_frequency_code,
        price_source, vintage_source, vintage_date, vintage_series_uri, provider_code
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Bloomberg IR Curve: ' || c.currency_code || '/' || c.currency_code || '-' || upper(c.index_family),
        'Simulated Bloomberg curve on ' || c.currency_code || '-' || upper(c.index_family)
        || ': a Cox-Ingersoll-Ross short rate, from deposits to a 10Y swap.',
        true, true, c.currency_code, c.index_family, '', 'COX_INGERSOLL_ROSS',
        c.annual_kappa, c.theta, c.annual_sigma, c.theta,
        60, 'Quarterly',
        'fixed', '', '', '', 'bloomberg'
    from t_bloomberg_ir c;

    insert into ores_dq_synthetic_ir_curve_template_entries_artefact_tbl (
        dataset_id, tenant_id, currency_code, index_family, tenor,
        sequence_index, start_tenor_code, end_tenor_code, instrument_code
    )
    select
        v_dataset_id, v_tenant_id, c.currency_code, c.index_family, '',
        e.sequence_index, e.start_tenor_code, e.end_tenor_code, e.instrument_code
    from t_bloomberg_ir c
    cross join (values
        (0, 'SPOT', '1M', 'DEPO'),
        (1, 'SPOT', '3M', 'DEPO'),
        (2, '3M', '6M', 'FRA'),
        (3, '6M', '12M', 'FRA'),
        (4, 'SPOT', '2Y', 'IRS'),
        (5, 'SPOT', '5Y', 'IRS'),
        (6, 'SPOT', '10Y', 'IRS')
    ) as e(sequence_index, start_tenor_code, end_tenor_code, instrument_code);

    -- The USD FOMC-dated short end: one series per meeting-dated OIS pillar,
    -- with the main USD curve's calibration so the two segments move together.
    insert into ores_dq_synthetic_ir_curve_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled, auto_start,
        currency_code, index_family, tenor, process_type,
        kappa, theta, sigma, initial_rate,
        ticks_per_hour, fixed_leg_payment_frequency_code,
        price_source, vintage_source, vintage_date, vintage_series_uri, provider_code
    )
    values (
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Bloomberg IR Curve: USD/USD-SOFR-FOMC',
        'Simulated Bloomberg FOMC-dated short end of USD-SOFR: the meeting-dated pillars 1F to 8F '
        || 'and the 1Y split tenor, one series per pillar, with the main USD curve''s calibration.',
        true, true, 'USD', 'sofr', 'FOMC', 'COX_INGERSOLL_ROSS',
        0.55, 0.04, 0.008, 0.04,
        60, 'Quarterly',
        'fixed', '', '', '', 'bloomberg'
    );

    insert into ores_dq_synthetic_ir_curve_template_entries_artefact_tbl (
        dataset_id, tenant_id, currency_code, index_family, tenor,
        sequence_index, start_tenor_code, end_tenor_code, instrument_code
    )
    select
        v_dataset_id, v_tenant_id, 'USD', 'sofr', 'FOMC',
        e.sequence_index, e.start_tenor_code, e.end_tenor_code, e.instrument_code
    from (values
        (0, 'SPOT', '1F', 'DEPO'),
        (1, '1F', '2F', 'FRA'),
        (2, '2F', '3F', 'FRA'),
        (3, '3F', '4F', 'FRA'),
        (4, '4F', '5F', 'FRA'),
        (5, '5F', '6F', 'FRA'),
        (6, '6F', '7F', 'FRA'),
        (7, '7F', '8F', 'FRA'),
        (8, '8F', '1Y', 'IRS')
    ) as e(sequence_index, start_tenor_code, end_tenor_code, instrument_code);
end $$;
