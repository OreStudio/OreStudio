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
 * Reference Data asset_measures Population Script
 *
 * Populates the refdata_asset_measures_tbl with reference data.
 * Source: asset_measures_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data asset_measures
-- =============================================================================

\echo '--- Reference Data asset_measures ---'

insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AccruedCoupon',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The coupon accrued on the underlying bonds from that the most recent bond coupon payment date until the valuation date.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'AccruedCoupon'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AccruedInterest',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The value of interest accrued from the previous payment to the valuation date.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'AccruedInterest'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AccruedInterestResetPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The value of interest accrued for price at last Reset.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'AccruedInterestResetPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AdditionalPriceNotation',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The secondary price field as required by CFTC''s 17 CFR Part 43.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'AdditionalPriceNotation'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AverageExposure',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The average exposure of this trade over its lifetime',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'AverageExposure'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BucketedCreditSpreadSensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a point change shift in the credit spread.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'BucketedCreditSpreadSensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BucketedDefaultProbabilitySensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a point change shift in the default probability.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'BucketedDefaultProbabilitySensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BucketedInterestRateConvexity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in interest rate sensitivity caused by a single point change in the yield curve (IR Gamma).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'BucketedInterestRateConvexity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BucketedInterestRateSensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a single point change in the yield curve (IR Delta).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'BucketedInterestRateSensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BucketedInterestRateVolatilitySensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a point change shift in the volatility matrix (vega).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'BucketedInterestRateVolatilitySensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BucketedRecoveryRateSensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a point change shift in the credit default recovery rate.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'BucketedRecoveryRateSensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CalculatedStrike',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The effective strike price of the option as derived from the underlying asset swap. (Used for options on asset swaps).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CalculatedStrike'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CAPMBeta',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Systematic risk = Ratio of expected return to expected return of the market',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CAPMBeta'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Cash',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'A monetary amount paid or received. For example, a monetary amount payable on the valuation date, or a monetary amount payable on another specified date, such as a payment date.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'Cash'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CashEquivalent',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The CashEquivalentLocalCurrency converted to the reporting currency (e.g. USD) at the spot exchange rate.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CashEquivalent'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CashEquivalentLocalCurrency',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The aggregated equivalent FX position in a specific currency. This includes the NPVs payable in that currency, plus equivalent positions generated by trades price sensitivity to FX rates.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CashEquivalentLocalCurrency'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CleanGrossCurrentMarketPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The price of an asset, expressed in par value, excluding accrued interest, excluding commissions, as observed on a market.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CleanGrossCurrentMarketPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CleanGrossCurrentSettlementPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The price of an asset, expressed in par value, excluding accrued interest, excluding commissions, for settlement purposes.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CleanGrossCurrentSettlementPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CleanGrossResetPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The reset price of an asset, expressed in par value, excluding accrued interest, excluding commissions.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CleanGrossResetPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CleanNetCurrentMarketPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The price of an asset, expressed in par value, excluding accrued interest, including commissions, as observed on a market.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CleanNetCurrentMarketPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CleanNetCurrentSettlementPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The price of an asset, expressed in par value, excluding accrued interest, including commissions, for settlement purposes.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CleanNetCurrentSettlementPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CleanNetResetPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The reset price of an asset, expressed in par value, excluding accrued interest, including commissions.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CleanNetResetPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ConvexityAdjustment',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'An adjustment to the price of an instrument (such as a future) to compensate for its lack of convexity.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ConvexityAdjustment'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CreditSpread',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The spread between the return of a credit instrument and of a corresponding risk free instrument.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CreditSpread'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CurrentNotional',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The notional in effect on the valuation date.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'CurrentNotional'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DE@R',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'VAR for 1 day time horizon and 95% level of confidence',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'DE@R'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DeltaAdjustedLongSwaptionPosition',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The Delta Adjusted Long Swaption Position.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'DeltaAdjustedLongSwaptionPosition'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DeltaAdjustedShortSwaptionPosition',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The Delta Adjusted Short Swaption Position.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'DeltaAdjustedShortSwaptionPosition'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DeltaFactor',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The Delta factor.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'DeltaFactor'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DirtyGrossCurrentMarketPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The price of an asset, expressed in par value, including accrued interest, excluding commissions, as observed on a market.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'DirtyGrossCurrentMarketPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DirtyGrossCurrentSettlementPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The price of an asset, expressed in par value, including accrued interest, excluding commissions, for settlement purposes.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'DirtyGrossCurrentSettlementPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DirtyGrossResetPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The reset price of an asset, expressed in par value, including accrued interest, excluding commissions.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'DirtyGrossResetPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DirtyNetCurrentMarketPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The price of an asset, expressed in par value, including accrued interest, including commissions, as observed on a market.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'DirtyNetCurrentMarketPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DirtyNetCurrentSettlementPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The price of an asset, expressed in par value, including accrued interest, including commissions, for settlement purposes.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'DirtyNetCurrentSettlementPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DirtyNetResetPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The reset price of an asset, expressed in par value, including accrued interest, including commissions.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'DirtyNetResetPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DividendYield',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The dividend payout ratio, expressed as a decimal (e.g. 0.03 = 3%) per year.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'DividendYield'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EconomicCapital',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Capital which is kept aside to compensate for unexpected losses due to credit risk. (VAR for 1 year and 99.97%)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'EconomicCapital'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EquityAccrual',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Unrealized profit or loss on an equity price based stream or product. This is based on the difference between current market price and the reset/reference price.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'EquityAccrual'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EVA',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Economic Value Added = (Spread + Fees - Expected loss - Operating cost) -ROE*(Capital at risk)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'EVA'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'FixedPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'A numerical price (usually a stock or bond price or a commodity price) that is used to price a derivative.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'FixedPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'FixedRate',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'A numerical rate (usually an interest or FX rate) that is used to price a derivative.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'FixedRate'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'FundingOnRealizedGains',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Funding-related interest charges associated with profit or loss on realized gains that have not yet been exchanged.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'FundingOnRealizedGains'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'FXSpotSensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a change in FX spot rate',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'FXSpotSensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GrossNotional',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The gross notional.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'GrossNotional'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GrossNPV',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The gross NPV.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'GrossNPV'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ImpliedVolatility',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The implied volatility of the underlying asset from the valuation date to the expiration of the option.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ImpliedVolatility'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'InterestOnRealizedGains',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Accrued interest on realized gains, for portfolio swap agreements where unwind profit/loss not exchanged until reset.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'InterestOnRealizedGains'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'JensensAlpha',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The average excess return on a portfolio relative to the excess return predicted by CAPM',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'JensensAlpha'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'LastAvailableSpotPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The last available spot price at the time of the transaction of the underlying asset with no spread.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'LastAvailableSpotPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'LoanEquivalent',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The loan equivalent exposure of this asset.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'LoanEquivalent'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'LongNotionalPosition',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The Long Notional Position.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'LongNotionalPosition'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'LongSwapPosition',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The Long Swap Position.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'LongSwapPosition'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MarginalRisk',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change of a portfolio VAR with addition of a specified asset.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'MarginalRisk'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MarketQuote',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The price of an instrument as quoted on an exchange or similar market.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'MarketQuote'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ModifiedSharpeRatio',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Sharpe ratio where both return and risk are defined relative to a benchmark portfolio',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ModifiedSharpeRatio'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NonDeltaAdjustedLongSwaptionPosition',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The Non Delta Adjusted Long Swaption Position.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'NonDeltaAdjustedLongSwaptionPosition'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NonDeltaAdjustedShortSwaptionPosition',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The Non Delta Adjusted Short Swaption Position.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'NonDeltaAdjustedShortSwaptionPosition'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NPV',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Net Present Value = sum of present values of all cash flows; excludes cash flows paid or received on the valution date.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'NPV'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NPVLocalCurrency',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'NPV in the trade currency.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'NPVLocalCurrency'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NumberOfUnderlyingSecurities',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Used for bond positions to report the product of the open units and the par value of the bond.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'NumberOfUnderlyingSecurities'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PackagePrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Traded price of the entire package in which the reported derivative transaction is a component.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'PackagePrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PackageSpread',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Traded price of the entire package in which the reported derivative transaction is a component of a package transaction. Package transaction price when the price of the package is expressed as a spread, difference between two reference prices. See CFTC Amendments to Part 45 for full definition.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'PackageSpread'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PAI',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Price adjustment interest ... the amount of interest owing on the NPV over the previous calculation period (use in clearing models).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'PAI'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ParallelShiftCreditSpreadSensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a parallel shift in the credit spread.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ParallelShiftCreditSpreadSensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ParallelShiftDefaultProbabilitySensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a parallel shift in the default probability.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ParallelShiftDefaultProbabilitySensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ParallelShiftInterestRateSensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a parallel shift in the yield curve/risk free rate of interest (IR Delta, rho).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ParallelShiftInterestRateSensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ParallelShiftInterestRateVolatilitySensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a parallel shift in the volatility matrix (vega).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ParallelShiftInterestRateVolatilitySensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ParallelShiftRecoveryRateSensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a parallel shift in the credit default recovery rate.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ParallelShiftRecoveryRateSensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PayNPV',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'NPV of cash flows for which the base counterparty pays.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'PayNPV'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PeakExposure',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The peak/potential exposure of this trade over its lifetime',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'PeakExposure'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Premium',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'A fee paid or received to purchase a contract (usually an option).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'Premium'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PriceNotation',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The primary price field as required by CFTC''s 17 CFR Part 43.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'PriceNotation'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PriorNPV',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Net Present Value for prior day/processing run = sum of present values of all cash flows; excludes cash flows paid or received on the valution date.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'PriorNPV'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'RAROC',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Risk adjusted return on capital = (Adjusted income)/(Capital at risk)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'RAROC'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'RealizedTradingGains',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Realized profit or loss that has not yet been exchanged. This is based on positions that have been closed out but not settled.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'RealizedTradingGains'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'RealizedVariance',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Realized variance between effective date and valuation date.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'RealizedVariance'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ReceiveNPV',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'NPV of cash flows for which the base counterparty receives.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ReceiveNPV'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'RecoveryRate',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The estimated amount that a creditor would receive in final satisfaction of the claims on a defaulted credit.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'RecoveryRate'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'RegulatoryCapital',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'A provision for expected losses, required by the BIS.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'RegulatoryCapital'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ReturnOnEconomicCapital',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The return from an asset expressed as a percentage of the amount of economic capital involved in holding that asset.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ReturnOnEconomicCapital'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ReturnOnRegulatoryCapital',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The return from an asset expressed as a percentage of the amount of regulatory capital involved in holding that asset.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ReturnOnRegulatoryCapital'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'RiskConcentration',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Measures the amount of risk concentrated in individual counterparties, similar assets, common geographical locations, or common industries.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'RiskConcentration'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ROA',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Return on assets = (Adjusted income)/Assets',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ROA'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'RORAC',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Return on risk-adjusted capital = (Adjusted income)/(BIS risk - based capital requirement)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'RORAC'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SettlementFxRate',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The FX rate used to compute a settlement amount.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'SettlementFxRate'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SettlementPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The settlement price.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'SettlementPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SharpeRatio',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The ratio between portfolio return in excess of the risk-free return and portfolio risk (measured as volatility)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'SharpeRatio'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ShortNotionalPosition',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The Short Notional Position.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ShortNotionalPosition'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ShortSwapPosition',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The Short Swap Position.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ShortSwapPosition'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SortinoRatio',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Similar to Sharpe Ratio but risk defined as downside risk rather than portfolio variance.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'SortinoRatio'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'StrikePrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The strike price.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'StrikePrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TransactedGrossPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The price, exclusive of any commission, at which a transaction has been conducted.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'TransactedGrossPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TransactedNetPrice',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The actual price (inclusive of commissions, when applicable) at which a transaction has been conducted.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'TransactedNetPrice'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TreatedRate',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'A rate following rate treatment procedures.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'TreatedRate'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TreynorRatio',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Similar to Sharpe Ratio but risk defined as CAPM systematic risk (beta) rather than portfolio variance.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'TreynorRatio'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ValuationAdjusted',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Adjusted valuation required for regulatory reporting (Ex: JFSA 39 Valuation Amount).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ValuationAdjusted'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ValuationDateChangeSensitivity',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Change in NPV/value caused by a change in valuation date (theta).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ValuationDateChangeSensitivity'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ValuationUnadjusted',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Unadjusted valuation required for regulatory reporting (Ex: JFSA 39 Valuation Amount).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'ValuationUnadjusted'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'VAR',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Value at Risk is the amount of money that could be lost over a pre-defined period of time with a a given level of confidence.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'VAR'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'VariationMargin',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'Amount required to be posted to accommodate change in net value of trade or portfolio.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'VariationMargin'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_asset_measures_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Volatility',
    0,
    'FPML_ASSET_MEASURE',
    'FpML',
    'The underlying price volatility used for calculating the value of this asset.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_asset_measures_tbl
    where code = 'Volatility'
    and coding_scheme_code = 'FPML_ASSET_MEASURE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'asset_measures' as entity, count(*) as count
from ores.refdata_asset_measures_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_asset_measures_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
