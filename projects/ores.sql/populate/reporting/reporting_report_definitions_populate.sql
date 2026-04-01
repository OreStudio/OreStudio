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
 * Report Definition Seed Population Script
 *
 * Registers the ore.report_definitions dataset and seeds the artefact table
 * with the 28 standard ORE analytics report definitions. These cover the full
 * trading desk analytic cycle from market data calibration through regulatory
 * capital, plus the intraday Headline Position report. All use
 * report_type='risk' and concurrency_policy='skip'.
 *
 * Execution order within reporting_populate.sql:
 *   report_types → concurrency_policies → report_definitions (this file)
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Catalog Registration
-- =============================================================================

\echo '--- ORE Analytics Catalog ---'

select ores_dq_catalogs_upsert_fn(ores_iam_system_tenant_id_fn(),
    'ORE Analytics',
    'ORE risk analytics seed data including report definition templates for provisioning new parties.',
    'OreStudio Development Team'
);

-- =============================================================================
-- Dataset Registration
-- =============================================================================

\echo '--- ORE Analytics: Report Definitions Dataset ---'

select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'ore.report_definitions',
    'ORE Analytics',
    'Trading',
    'Reference Data',
    'NONE',
    'Primary',
    'Synthetic',
    'Raw',
    'OreStudio Code Generation Methodology',
    'ORE Analytics Report Definitions',
    'Default set of 27 ORE risk analytics report definitions covering calibration, valuation, sensitivities, counterparty credit risk, market risk, scenario analysis, and regulatory capital.',
    'ORESTUDIO',
    'Seed data for party provisioning wizard report setup',
    current_date,
    'Internal Use Only',
    'report_definitions'
);

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

\echo '--- ORE Report Definition Artefacts ---'

do $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_iam_system_tenant_id_fn();
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'ore.report_definitions'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: ore.report_definitions';
    end if;

    -- Skip if already populated
    if exists (
        select 1 from ores_dq_report_definitions_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise notice 'Report definitions artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise notice 'Populating report definitions for dataset: ore.report_definitions';

    insert into ores_dq_report_definitions_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, report_type, schedule_expression, concurrency_policy, display_order
    )
    values
    -- Market data & calibration (5-6 am)
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Model Calibration',
     'Calibrates interest rate, FX, and volatility models (LGM, Hull-White, SABR, Black-Scholes) to live market data. Outputs calibrated parameters and fit quality metrics (RMSE). Must run before exposure simulation, XVA, and sensitivity analytics that depend on calibrated model parameters.',
     'risk', '0 5 * * 1-5', 'skip', 10),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Yield Curves',
     'Bootstraps discount and projection yield curves from market instruments (deposits, FRAs, swaps, OIS, bonds). Outputs the full term structure of interest rates used by all pricing engines. Essential prerequisite for NPV, sensitivity, and Monte Carlo exposure analytics.',
     'risk', '0 5 * * 1-5', 'skip', 20),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'FX Spot Rates',
     'Loads and validates FX spot rates for all active currency pairs from market data feeds. Provides consistent FX conversion for multi-currency portfolio valuation, sensitivities, and regulatory capital calculations that require base-currency aggregation.',
     'risk', '0 5 * * 1-5', 'skip', 30),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Volatility Surfaces',
     'Constructs implied volatility surfaces for interest rates, FX, and equity from market option quotes. Applies smile interpolation (SVI, SABR) and arbitrage-free calibration. Required for options pricing, vega sensitivities, stressed VaR, and FRTB vega capital computation.',
     'risk', '0 5 * * 1-5', 'skip', 40),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Credit Curves',
     'Bootstraps CDS-implied survival probability curves and hazard rate curves for each counterparty and entity. Calibrates credit models (Jarrow-Turnbull, Hull-White credit) to market spreads. Prerequisite for CVA, DVA, FVA, and regulatory SA-CVA capital computations.',
     'risk', '0 5 * * 1-5', 'skip', 50),
    -- Portfolio valuation (6-7 am)
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'NPV',
     'Full mark-to-market portfolio valuation producing present values for all active trades. Applies validated yield curves and FX rates. Provides the daily P&L baseline, feeds downstream sensitivities, and serves as the reference for risk-neutral pricing across all asset classes.',
     'risk', '0 6 * * 1-5', 'skip', 60),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Cashflows',
     'Projects all future contractual cashflows across the portfolio: fixed, floating, contingent, and collateral flows. Used for liquidity risk, funding cost estimation, hedge effectiveness testing, and IFRS 9 / IFRS 7 cashflow disclosure.',
     'risk', '0 6 * * 1-5', 'skip', 70),
    -- Sensitivities (7-8 am)
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Delta and Gamma',
     'Computes first-order (delta) and second-order (gamma) price sensitivities to interest rates, FX, and credit spreads using bump-and-revalue. Produces risk ladder reports by tenor bucket and currency. Feeds hedging, P&L attribution, and FRTB sensitivity-based method capital.',
     'risk', '0 7 * * 1-5', 'skip', 80),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Vega',
     'Computes first-order sensitivity of portfolio value to implied volatility across all relevant expiry and strike dimensions. Aggregated by asset class, risk factor, and tenor. Required for volatility hedging and FRTB SBM vega capital.',
     'risk', '0 7 * * 1-5', 'skip', 90),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Bucketed DV01',
     'Key-rate DV01 (dollar value of one basis point) decomposition across standardised tenor buckets (1M, 3M, 6M, 1Y, 2Y, 5Y, 10Y, 20Y, 30Y). Provides a granular interest rate risk profile per currency, netting set, and book. Core input to duration management and FRTB delta capital.',
     'risk', '0 7 * * 1-5', 'skip', 100),
    -- Counterparty credit risk (8-9 am)
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Exposure',
     'Monte Carlo simulation of future exposure profiles (EE, PFE, EPE, ENE) at the netting-set level using risk-factor simulation. Drives CVA/DVA valuation, regulatory capital under SA-CCR, and internal credit limits. Computationally intensive; scheduled before XVA to provide input exposure paths.',
     'risk', '0 8 * * 1-5', 'skip', 110),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'CVA',
     'Credit Valuation Adjustment — the market value of counterparty default risk embedded in OTC derivatives. Computed as the risk-neutral expectation of loss given default, integrating EPE profiles with counterparty survival probability and LGD. Required for IFRS 13 fair value disclosure and regulatory capital under SA-CVA and BA-CVA.',
     'risk', '0 8 * * 1-5', 'skip', 120),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'DVA',
     'Debt Valuation Adjustment — the own-credit component of OTC derivative fair value, reflecting the benefit to the portfolio holder from the institution''s own default risk. Symmetric counterpart to CVA. Required for IFRS 13 compliance and bilateral CVA (BCVA) reporting.',
     'risk', '0 8 * * 1-5', 'skip', 130),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'FVA',
     'Funding Valuation Adjustment — the cost or benefit of funding uncollateralised or partially collateralised derivative positions at the institution''s unsecured borrowing rate. Decomposes into FCA (funding cost) and FBA (funding benefit). Material for institutions with significant uncollateralised derivative books.',
     'risk', '0 8 * * 1-5', 'skip', 140),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'KVA',
     'Capital Valuation Adjustment — the cost of holding regulatory capital against a derivative position over its lifetime, discounted at the hurdle rate. Reflects the economic cost of capital consumed by the trade under SA-CCR or IMM, including CVA capital charges. Used in strategic pricing and deal profitability analysis.',
     'risk', '0 8 * * 1-5', 'skip', 150),
    -- Market risk (9-10 am)
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Historical VaR',
     'Historical simulation Value-at-Risk at 99% (regulatory) and 95% (internal) confidence levels over a 250-day look-back. Applies full revaluation for non-linear exposures. Produces VaR by risk factor, desk, and portfolio. Primary input to Basel III Internal Models Approach capital.',
     'risk', '0 9 * * 1-5', 'skip', 160),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Parametric VaR',
     'Delta-normal (parametric) VaR using a covariance matrix of risk-factor returns. Faster than historical simulation; used as an intraday risk estimate and for limit monitoring. Decomposes into marginal VaR and component VaR by position for attribution and hedging analysis.',
     'risk', '0 9 * * 1-5', 'skip', 170),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Stressed VaR',
     'VaR computed over a stressed historical window (typically the 2008 financial crisis or COVID-2020 period) as required by Basel 2.5. Uses full revaluation. Required as a capital add-on under the IMA. Scenario window is updated annually per regulatory review.',
     'risk', '0 9 * * 1-5', 'skip', 180),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Expected Shortfall',
     'Expected Shortfall (ES) at 97.5% confidence, the Basel IV FRTB replacement for VaR under IMA. Computed using a 12-month liquidity-adjusted horizon with scenario weighting. Produces partial ES by risk class (GIRR, CSR, FX, EQ, CMDTY) for the FRTB capital formula.',
     'risk', '0 9 * * 1-5', 'skip', 190),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'P&L Attribution',
     'Daily attribution of P&L into risk-factor components: delta, gamma, vega, theta, and unexplained residual. Required for FRTB IMA back-testing and P&L attribution test (PLAT) compliance. Compares hypothetical P&L (from sensitivities) against actual P&L to validate model quality.',
     'risk', '0 9 * * 1-5', 'skip', 200),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Back-Testing',
     'Daily comparison of 1-day 99% VaR against actual and hypothetical P&L over a 250-day rolling window. Counts exceptions and assigns capital multiplier (green/amber/red zone) per Basel internal models framework. Required for ongoing IMA approval and supervisory reporting.',
     'risk', '0 9 * * 1-5', 'skip', 210),
    -- Scenario analysis (10-11 am)
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Stress Testing',
     'Portfolio revaluation under a library of regulatory and internal stress scenarios including: 2008 credit crisis, 2010 European sovereign debt, 2020 COVID shock, and custom management scenarios. Produces P&L impact, VaR delta, and liquidity stress metrics by desk and book.',
     'risk', '0 10 * * 1-5', 'skip', 220),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Sensitivity Analysis',
     'Systematic grid-based sensitivity analysis varying key market factors (rates, spreads, FX, vol) across a user-defined range. Produces heat maps and waterfall charts for risk-factor impact assessment. Complements historical VaR with forward-looking scenario coverage.',
     'risk', '0 10 * * 1-5', 'skip', 230),
    -- Regulatory capital (10-11 am)
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'FRTB-SA',
     'Fundamental Review of the Trading Book (FRTB) Standardised Approach capital charge. Computes the sensitivity-based method (SBM) capital requirement using supervisory prescribed delta, vega, and curvature sensitivities across all risk classes. Floor model and fallback for desks not approved for IMA.',
     'risk', '0 10 * * 1-5', 'skip', 240),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'SA-CVA',
     'Standardised CVA (SA-CVA) regulatory capital charge per Basel IV / CRR3. Aggregates CVA delta and vega sensitivities across risk classes using supervisory prescribed delta factors and correlation matrices. Produces the CVA risk capital requirement for institutions that elect or are required to use the SA-CVA approach under FRTB.',
     'risk', '0 10 * * 1-5', 'skip', 250),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'BA-CVA',
     'Basic CVA (BA-CVA) regulatory capital charge, the simplified alternative to SA-CVA under Basel IV. Computes capital using supervisory EAD, maturity, and credit risk weights without full sensitivity computation. Applicable to institutions below the material CVA portfolio threshold for SA-CVA.',
     'risk', '0 10 * * 1-5', 'skip', 260),
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'SA-CCR',
     'Standardised Approach for Counterparty Credit Risk (SA-CCR) Exposure-at-Default (EAD) calculation per Basel III/IV. Applies supervisory delta, maturity factor, and supervisory factor to each netting set. Required for Risk-Weighted Asset (RWA) and leverage ratio calculations. Replaces the legacy Current Exposure Method (CEM).',
     'risk', '0 10 * * 1-5', 'skip', 270),
    -- Intraday reports (continuous / every 10 minutes)
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Headline Position',
     'Displays the most important Greeks across the entire deal set for a book or portfolio. The primary real-time risk overview report for traders and risk managers providing high-level sign-off visibility on the current state of risk. Configurable measures (PV, Daily P&L, MTD P&L, YTD P&L, Delta P&L) and aggregation dimensions (Book, Portfolio, Currency Pair, Greek, Unit Hedge). Supports filtering by book and by threshold. Scheduled versions run at EOD and at AM/PM Flash times; EOD run uses the signed-off market data cut.',
     'risk', '*/10 * * * *', 'skip', 280);

    raise notice 'Inserted 28 report definition artefacts for ore.report_definitions';
end;
$$ language plpgsql;

-- =============================================================================
-- Summary
-- =============================================================================

select 'ORE Report Definition Artefacts' as entity, count(*) as count
from ores_dq_report_definitions_artefact_tbl a
join ores_dq_datasets_tbl d on d.id = a.dataset_id
where d.code = 'ore.report_definitions'
  and d.valid_to = ores_utility_infinity_timestamp_fn();
