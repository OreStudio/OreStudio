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
 * Report Definition Seed Population Script — Tiered
 *
 * Registers the ore.report_definitions dataset and seeds the artefact table
 * with 30 report definitions across four audience tiers:
 *
 *   Common      — every entity (data quality, system health, audit)
 *   Regulatory  — all regulated entities (SA-CCR, leverage, FRTB, LCR, etc.)
 *   Strategic   — holding company only (group consolidation, board dashboard)
 *   Trading     — trading desks only (market risk, sensitivities, XVA, calibration)
 *
 * Per-entity provisioning filters by tier so a holding company gets strategic
 * oversight reports while a trading desk gets desk-level analytics.
 *
 * Execution order within reporting_populate.sql:
 *   report_types → concurrency_policies → report_definitions (this file)
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Catalog Registration
-- =============================================================================

-- --- ORE Analytics Catalog ---

DO $$
BEGIN
    PERFORM ores_dq_catalogs_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ORE Analytics',
        'ORE risk analytics seed data including report definition templates for provisioning new parties.',
        'OreStudio Development Team'
    );
END $$;

-- =============================================================================
-- Dataset Registration
-- =============================================================================

-- --- ORE Analytics: Report Definitions Dataset ---

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
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
        '30 tiered ORE report definitions covering common operational reports, regulatory capital, strategic consolidation, and trading-desk analytics. Tier-filtered during per-entity provisioning.',
        'ORESTUDIO',
        'Seed data for party provisioning wizard report setup',
        current_date,
        'Internal Use Only',
        'report_definitions'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data — 30 reports across 4 tiers
-- =============================================================================

do $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
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
        raise debug 'Report definitions artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating 30 tiered report definitions for dataset: ore.report_definitions';

    insert into ores_dq_report_definitions_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, report_type, schedule_expression, concurrency_policy,
        display_order, tier, applicable_jurisdiction
    )
    values
    -- =========================================================================
    -- Common — every entity (3 reports)
    -- =========================================================================

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Data Quality Completeness',
     'Validates all required market data, reference data, and trade data is present across the entity. Flags gaps in instrument codes, missing curve pillars, stale fixings, and orphaned trades. Runs early so downstream analytics have a clean data baseline.',
     'risk', '0 5 * * 1-5', 'skip', 5, 'common', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'System Health',
     'Monitors service availability, NATS message latency, database connection pool saturation, and per-service error rates. Produces a traffic-light dashboard: green (all healthy), amber (degraded), red (down). Alerts on sustained error-rate spikes above 5%.',
     'risk', '30 5 * * 1-5', 'skip', 10, 'common', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Audit Trail Summary',
     'Daily summary of all create, update, and delete operations across all domain entities for the entity. Broken down by service, actor, and change reason category. Supports SOX and SOC 2 compliance requirements for change logging.',
     'risk', '0 6 * * 1-5', 'skip', 15, 'common', null),

    -- =========================================================================
    -- Regulatory — all regulated entities (7 reports)
    -- =========================================================================

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'SA-CCR Exposure',
     'Standardised Approach for Counterparty Credit Risk: computes Exposure-at-Default (EAD), Replacement Cost (RC), and Potential Future Exposure (PFE) per netting set under Basel III/IV supervisory rules. Required for Risk-Weighted Asset (RWA) and leverage ratio calculations. Replaces the legacy Current Exposure Method (CEM).',
     'risk', '0 6 * * 1-5', 'skip', 20, 'regulatory', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Leverage Ratio',
     'Computes the Basel III leverage ratio: Tier 1 capital divided by total exposure measure (on- and off-balance sheet, derivatives, SFTs). Alerts if the ratio approaches the 3% minimum threshold. Required for all regulated entities under CRR/Basel III pillar 2.',
     'risk', '0 6 * * 1-5', 'skip', 25, 'regulatory', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'FRTB SA Capital',
     'Fundamental Review of the Trading Book — Standardised Approach: computes market risk capital using supervisory-prescribed sensitivity-based method (SBM) across delta, vega, and curvature risk for all risk classes (GIRR, CSR, FX, EQ, CMDTY). Floor model and fallback for desks not approved for IMA.',
     'risk', '0 7 * * 1-5', 'skip', 30, 'regulatory', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Liquidity Coverage',
     'Liquidity Coverage Ratio (LCR): High-Quality Liquid Assets divided by net cash outflows over a 30-day stress horizon. Flags when HQLA buffer drops below 100% minimum. Required under Basel III for all regulated deposit-taking and investment firms.',
     'risk', '0 7 * * 1-5', 'skip', 35, 'regulatory', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Large Exposures',
     'Monitors single-counterparty exposure concentration against the 25% of Tier 1 capital regulatory limit. Aggregates across all netting sets, SFTs, and indirect exposures for each legal entity counterparty. Produces breach alerts and a top-20 concentration report.',
     'risk', '0 7 * * 1-5', 'skip', 40, 'regulatory', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'MIFID II Transaction Reporting',
     'Trade transparency reporting under MIFID II / MIFIR Article 26. Produces transaction reports for submission to the FCA Approved Reporting Mechanism (ARM). Covers all financial instruments traded on a UK/EEA trading venue. UK entities only.',
     'risk', '0 8 * * 1-5', 'skip', 45, 'regulatory', 'GB'),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'CFTC Swap Data Reporting',
     'Swap transaction reporting under CFTC Part 43/45 rules. Produces real-time public and regulatory reports for all swap transactions to a registered Swap Data Repository (SDR). Covers all CFTC-jurisdiction swaps. US entities only.',
     'risk', '0 8 * * 1-5', 'skip', 50, 'regulatory', 'US'),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'HKMA Trade Repository',
     'OTC derivatives transaction reporting under HKMA Securities and Futures (OTC Derivative Transactions — Reporting and Record Keeping Obligations) Rules. Reports to the HKTR via the DTCC GTR. HK entities only.',
     'risk', '0 8 * * 1-5', 'skip', 55, 'regulatory', 'HK'),

    -- =========================================================================
    -- Strategic — holding company only (4 reports)
    -- =========================================================================

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Group Consolidated Exposure',
     'Aggregate exposure across all subsidiaries by asset class, currency, and counterparty. Eliminates intercompany positions per IFRS 10 consolidation rules. Provides the single group-wide risk view for the board and regulators. Holding company only.',
     'risk', '0 8 * * 1-5', 'skip', 60, 'strategic', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Board Risk Dashboard',
     'One-page executive summary for the board risk committee: group VaR (99% 1-day), total exposure by asset class, consolidated P&L, capital ratios (CET1, leverage, LCR), top 10 counterparty exposures, and month-on-month trend arrows. Holding company only.',
     'risk', '0 8 * * 1-5', 'skip', 65, 'strategic', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Cross-Entity Counterparty Concentration',
     'Aggregates single-name counterparty exposure across all legal entities in the group. Flags names exceeding 25% of consolidated Tier 1 capital. Identifies hidden concentration where individual subsidiary exposures appear within limits but group aggregate breaches the threshold. Holding company only.',
     'risk', '0 8 * * 1-5', 'skip', 70, 'strategic', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Intercompany Exposure Matrix',
     'Matrix of exposures between all group entities for consolidation elimination. Identifies circular funding, transfer pricing mismatches, and netting inefficiencies across the group. Monthly cadence (not daily — intercompany positions churn slowly). Holding company only.',
     'risk', '0 6 * * 1,15', 'skip', 75, 'strategic', null),

    -- =========================================================================
    -- Trading — trading desks only (16 reports)
    -- =========================================================================

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Model Calibration',
     'Calibrates interest rate, FX, and volatility models (LGM, Hull-White, SABR, Black-Scholes) to live market data. Outputs calibrated parameters and fit quality metrics (RMSE). Must run before exposure simulation, XVA, and sensitivity analytics that depend on calibrated model parameters.',
     'risk', '0 5 * * 1-5', 'skip', 80, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Yield Curves',
     'Bootstraps discount and projection yield curves from market instruments (deposits, FRAs, swaps, OIS, bonds). Outputs the full term structure of interest rates used by all pricing engines. Essential prerequisite for NPV, sensitivity, and Monte Carlo exposure analytics.',
     'risk', '0 5 * * 1-5', 'skip', 85, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'FX Spot Rates',
     'Loads and validates FX spot rates for all active currency pairs from market data feeds. Provides consistent FX conversion for multi-currency portfolio valuation, sensitivities, and regulatory capital calculations that require base-currency aggregation.',
     'risk', '0 5 * * 1-5', 'skip', 90, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Volatility Surfaces',
     'Constructs implied volatility surfaces for interest rates, FX, and equity from market option quotes. Applies smile interpolation (SVI, SABR) and arbitrage-free calibration. Required for options pricing, vega sensitivities, stressed VaR, and FRTB vega capital.',
     'risk', '0 5 * * 1-5', 'skip', 95, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Credit Curves',
     'Bootstraps CDS-implied survival probability curves and hazard rate curves for each counterparty and entity. Calibrates credit models (Jarrow-Turnbull, Hull-White credit) to market spreads. Prerequisite for CVA, DVA, FVA, and regulatory SA-CVA capital computations.',
     'risk', '0 5 * * 1-5', 'skip', 100, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'NPV',
     'Full mark-to-market portfolio valuation producing present values for all active trades. Applies validated yield curves and FX rates. Provides the daily P&L baseline, feeds downstream sensitivities, and serves as the reference for risk-neutral pricing across all asset classes.',
     'risk', '0 6 * * 1-5', 'skip', 105, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Cashflows',
     'Projects all future contractual cashflows across the portfolio: fixed, floating, contingent, and collateral flows. Used for liquidity risk, funding cost estimation, hedge effectiveness testing, and IFRS 9 / IFRS 7 cashflow disclosure.',
     'risk', '0 6 * * 1-5', 'skip', 110, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Delta and Gamma',
     'Computes first-order (delta) and second-order (gamma) price sensitivities to interest rates, FX, and credit spreads using bump-and-revalue. Produces risk ladder reports by tenor bucket and currency. Feeds hedging, P&L attribution, and FRTB sensitivity-based method capital.',
     'risk', '0 7 * * 1-5', 'skip', 115, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Vega',
     'Computes first-order sensitivity of portfolio value to implied volatility across all relevant expiry and strike dimensions. Aggregated by asset class, risk factor, and tenor. Required for volatility hedging and FRTB SBM vega capital.',
     'risk', '0 7 * * 1-5', 'skip', 120, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Bucketed DV01',
     'Key-rate DV01 (dollar value of one basis point) decomposition across standardised tenor buckets (1M, 3M, 6M, 1Y, 2Y, 5Y, 10Y, 20Y, 30Y). Provides a granular interest rate risk profile per currency, netting set, and book. Core input to duration management and FRTB delta capital.',
     'risk', '0 7 * * 1-5', 'skip', 125, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Exposure',
     'Monte Carlo simulation of future exposure profiles (EE, PFE, EPE, ENE) at the netting-set level using risk-factor simulation. Drives CVA/DVA valuation, regulatory capital under SA-CCR, and internal credit limits. Computationally intensive; uses queue concurrency to serialise with other long-running reports.',
     'risk', '0 8 * * 1-5', 'queue', 130, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'CVA/DVA/FVA',
     'Composite report computing all three valuation adjustments: Credit VA (counterparty default risk), Debit VA (own default risk), and Funding VA (uncollateralised funding cost). Uses exposure profiles from the Exposure report and credit curves from Credit Curves. Essential for IFRS 13 fair value.',
     'risk', '0 8 * * 1-5', 'queue', 135, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Stressed VaR',
     'VaR computed over a stressed historical window (2008 financial crisis or COVID-2020) at 99% confidence, 10-day holding period. Required as a capital add-on under Basel 2.5 IMA. Uses full revaluation for non-linear exposures.',
     'risk', '0 8 * * 1-5', 'skip', 140, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'P&L Attribution',
     'Daily decomposition of P&L into risk-factor components: delta, gamma, vega, theta, carry, and unexplained residual. Required for FRTB IMA back-testing and P&L attribution test (PLAT) compliance. Compares hypothetical P&L against actual P&L to validate model quality.',
     'risk', '0 16 * * 1-5', 'skip', 145, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Intraday Risk Monitor',
     'Real-time risk snapshot updated every 30 minutes during trading hours. Displays key risk metrics: PV, delta ladder, top 10 P&L movers, VaR estimate, and limit-utilisation gauges. Uses queue concurrency to serialise updates. Provides traders and desk heads with continuous risk visibility between end-of-day analytic runs.',
     'risk', '*/30 8-17 * * 1-5', 'queue', 150, 'trading', null),

    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Headline Position',
     'Displays the most important Greeks across the entire deal set for a book or portfolio. The primary real-time risk overview report for traders and risk managers. Configurable measures (PV, Daily P&L, MTD P&L, YTD P&L, Delta P&L) and aggregation dimensions (Book, Portfolio, Currency Pair, Greek, Unit Hedge).',
     'risk', '*/10 * * * *', 'skip', 155, 'trading', null);

    raise debug 'Inserted 30 tiered report definition artefacts for ore.report_definitions';
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

-- Tier breakdown for verification
select tier, count(*) as count
from ores_dq_report_definitions_artefact_tbl a
join ores_dq_datasets_tbl d on d.id = a.dataset_id
where d.code = 'ore.report_definitions'
  and d.valid_to = ores_utility_infinity_timestamp_fn()
group by tier
order by tier;
