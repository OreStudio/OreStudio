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
 * DQ Artefact FpML Asset Measure Population Script
 *
 * Populates the dq_asset_measures_artefact_tbl with reference data.
 * Dataset: fpml.asset_measure
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_asset_measures_publish_fn() to publish to production.
 */

set schema 'metadata';

-- =============================================================================
-- DQ Artefact FpML Asset Measure
-- =============================================================================

\echo '--- DQ Artefact FpML Asset Measure ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from metadata.dq_datasets_tbl
    where code = 'fpml.asset_measure'
    and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.asset_measure not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from metadata.dq_asset_measures_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AccruedCoupon',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The coupon accrued on the underlying bonds from that the most recent bond coupon payment date until the valuation date.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AccruedInterest',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The value of interest accrued from the previous payment to the valuation date.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AccruedInterestResetPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The value of interest accrued for price at last Reset.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AdditionalPriceNotation',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The secondary price field as required by CFTC''s 17 CFR Part 43.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AverageExposure',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The average exposure of this trade over its lifetime'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BucketedCreditSpreadSensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a point change shift in the credit spread.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BucketedDefaultProbabilitySensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a point change shift in the default probability.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BucketedInterestRateConvexity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in interest rate sensitivity caused by a single point change in the yield curve (IR Gamma).'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BucketedInterestRateSensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a single point change in the yield curve (IR Delta).'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BucketedInterestRateVolatilitySensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a point change shift in the volatility matrix (vega).'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BucketedRecoveryRateSensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a point change shift in the credit default recovery rate.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CalculatedStrike',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The effective strike price of the option as derived from the underlying asset swap. (Used for options on asset swaps).'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CAPMBeta',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Systematic risk = Ratio of expected return to expected return of the market'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Cash',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'A monetary amount paid or received. For example, a monetary amount payable on the valuation date, or a monetary amount payable on another specified date, such as a payment date.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CashEquivalent',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The CashEquivalentLocalCurrency converted to the reporting currency (e.g. USD) at the spot exchange rate.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CashEquivalentLocalCurrency',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The aggregated equivalent FX position in a specific currency. This includes the NPVs payable in that currency, plus equivalent positions generated by trades price sensitivity to FX rates.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CleanGrossCurrentMarketPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The price of an asset, expressed in par value, excluding accrued interest, excluding commissions, as observed on a market.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CleanGrossCurrentSettlementPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The price of an asset, expressed in par value, excluding accrued interest, excluding commissions, for settlement purposes.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CleanGrossResetPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The reset price of an asset, expressed in par value, excluding accrued interest, excluding commissions.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CleanNetCurrentMarketPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The price of an asset, expressed in par value, excluding accrued interest, including commissions, as observed on a market.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CleanNetCurrentSettlementPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The price of an asset, expressed in par value, excluding accrued interest, including commissions, for settlement purposes.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CleanNetResetPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The reset price of an asset, expressed in par value, excluding accrued interest, including commissions.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ConvexityAdjustment',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'An adjustment to the price of an instrument (such as a future) to compensate for its lack of convexity.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CreditSpread',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The spread between the return of a credit instrument and of a corresponding risk free instrument.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CurrentNotional',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The notional in effect on the valuation date.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DE@R',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'VAR for 1 day time horizon and 95% level of confidence'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DeltaAdjustedLongSwaptionPosition',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The Delta Adjusted Long Swaption Position.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DeltaAdjustedShortSwaptionPosition',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The Delta Adjusted Short Swaption Position.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DeltaFactor',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The Delta factor.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DirtyGrossCurrentMarketPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The price of an asset, expressed in par value, including accrued interest, excluding commissions, as observed on a market.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DirtyGrossCurrentSettlementPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The price of an asset, expressed in par value, including accrued interest, excluding commissions, for settlement purposes.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DirtyGrossResetPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The reset price of an asset, expressed in par value, including accrued interest, excluding commissions.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DirtyNetCurrentMarketPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The price of an asset, expressed in par value, including accrued interest, including commissions, as observed on a market.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DirtyNetCurrentSettlementPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The price of an asset, expressed in par value, including accrued interest, including commissions, for settlement purposes.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DirtyNetResetPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The reset price of an asset, expressed in par value, including accrued interest, including commissions.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DividendYield',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The dividend payout ratio, expressed as a decimal (e.g. 0.03 = 3%) per year.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EconomicCapital',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Capital which is kept aside to compensate for unexpected losses due to credit risk. (VAR for 1 year and 99.97%)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EquityAccrual',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Unrealized profit or loss on an equity price based stream or product. This is based on the difference between current market price and the reset/reference price.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EVA',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Economic Value Added = (Spread + Fees - Expected loss - Operating cost) -ROE*(Capital at risk)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FixedPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'A numerical price (usually a stock or bond price or a commodity price) that is used to price a derivative.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FixedRate',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'A numerical rate (usually an interest or FX rate) that is used to price a derivative.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FundingOnRealizedGains',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Funding-related interest charges associated with profit or loss on realized gains that have not yet been exchanged.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FXSpotSensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a change in FX spot rate'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GrossNotional',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The gross notional.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GrossNPV',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The gross NPV.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ImpliedVolatility',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The implied volatility of the underlying asset from the valuation date to the expiration of the option.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'InterestOnRealizedGains',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Accrued interest on realized gains, for portfolio swap agreements where unwind profit/loss not exchanged until reset.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JensensAlpha',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The average excess return on a portfolio relative to the excess return predicted by CAPM'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LastAvailableSpotPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The last available spot price at the time of the transaction of the underlying asset with no spread.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LoanEquivalent',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The loan equivalent exposure of this asset.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LongNotionalPosition',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The Long Notional Position.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LongSwapPosition',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The Long Swap Position.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MarginalRisk',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change of a portfolio VAR with addition of a specified asset.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MarketQuote',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The price of an instrument as quoted on an exchange or similar market.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ModifiedSharpeRatio',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Sharpe ratio where both return and risk are defined relative to a benchmark portfolio'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NonDeltaAdjustedLongSwaptionPosition',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The Non Delta Adjusted Long Swaption Position.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NonDeltaAdjustedShortSwaptionPosition',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The Non Delta Adjusted Short Swaption Position.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NPV',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Net Present Value = sum of present values of all cash flows; excludes cash flows paid or received on the valution date.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NPVLocalCurrency',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'NPV in the trade currency.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NumberOfUnderlyingSecurities',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Used for bond positions to report the product of the open units and the par value of the bond.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PackagePrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Traded price of the entire package in which the reported derivative transaction is a component.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PackageSpread',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Traded price of the entire package in which the reported derivative transaction is a component of a package transaction. Package transaction price when the price of the package is expressed as a spread, difference between two reference prices. See CFTC Amendments to Part 45 for full definition.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PAI',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Price adjustment interest ... the amount of interest owing on the NPV over the previous calculation period (use in clearing models).'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ParallelShiftCreditSpreadSensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a parallel shift in the credit spread.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ParallelShiftDefaultProbabilitySensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a parallel shift in the default probability.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ParallelShiftInterestRateSensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a parallel shift in the yield curve/risk free rate of interest (IR Delta, rho).'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ParallelShiftInterestRateVolatilitySensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a parallel shift in the volatility matrix (vega).'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ParallelShiftRecoveryRateSensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a parallel shift in the credit default recovery rate.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PayNPV',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'NPV of cash flows for which the base counterparty pays.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PeakExposure',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The peak/potential exposure of this trade over its lifetime'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Premium',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'A fee paid or received to purchase a contract (usually an option).'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PriceNotation',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The primary price field as required by CFTC''s 17 CFR Part 43.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PriorNPV',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Net Present Value for prior day/processing run = sum of present values of all cash flows; excludes cash flows paid or received on the valution date.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RAROC',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Risk adjusted return on capital = (Adjusted income)/(Capital at risk)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RealizedTradingGains',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Realized profit or loss that has not yet been exchanged. This is based on positions that have been closed out but not settled.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RealizedVariance',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Realized variance between effective date and valuation date.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ReceiveNPV',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'NPV of cash flows for which the base counterparty receives.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RecoveryRate',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The estimated amount that a creditor would receive in final satisfaction of the claims on a defaulted credit.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RegulatoryCapital',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'A provision for expected losses, required by the BIS.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ReturnOnEconomicCapital',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The return from an asset expressed as a percentage of the amount of economic capital involved in holding that asset.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ReturnOnRegulatoryCapital',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The return from an asset expressed as a percentage of the amount of regulatory capital involved in holding that asset.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RiskConcentration',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Measures the amount of risk concentrated in individual counterparties, similar assets, common geographical locations, or common industries.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ROA',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Return on assets = (Adjusted income)/Assets'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RORAC',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Return on risk-adjusted capital = (Adjusted income)/(BIS risk - based capital requirement)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SettlementFxRate',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The FX rate used to compute a settlement amount.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SettlementPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The settlement price.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SharpeRatio',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The ratio between portfolio return in excess of the risk-free return and portfolio risk (measured as volatility)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ShortNotionalPosition',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The Short Notional Position.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ShortSwapPosition',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The Short Swap Position.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SortinoRatio',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Similar to Sharpe Ratio but risk defined as downside risk rather than portfolio variance.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'StrikePrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The strike price.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TransactedGrossPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The price, exclusive of any commission, at which a transaction has been conducted.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TransactedNetPrice',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The actual price (inclusive of commissions, when applicable) at which a transaction has been conducted.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TreatedRate',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'A rate following rate treatment procedures.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TreynorRatio',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Similar to Sharpe Ratio but risk defined as CAPM systematic risk (beta) rather than portfolio variance.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ValuationAdjusted',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Adjusted valuation required for regulatory reporting (Ex: JFSA 39 Valuation Amount).'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ValuationDateChangeSensitivity',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Change in NPV/value caused by a change in valuation date (theta).'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ValuationUnadjusted',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Unadjusted valuation required for regulatory reporting (Ex: JFSA 39 Valuation Amount).'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'VAR',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Value at Risk is the amount of money that could be lost over a pre-defined period of time with a a given level of confidence.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'VariationMargin',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'Amount required to be posted to accommodate change in net value of trade or portfolio.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_asset_measures_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Volatility',
        1,
        'FPML_ASSET_MEASURE',
        'FpML',
        'The underlying price volatility used for calculating the value of this asset.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_asset_measures_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_asset_measures_artefact' as entity, count(*) as count
from metadata.dq_asset_measures_artefact_tbl;

select coding_scheme_code, count(*) as count
from metadata.dq_asset_measures_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
