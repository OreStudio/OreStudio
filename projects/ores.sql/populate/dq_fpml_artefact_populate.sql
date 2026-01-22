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
 * DQ Artefact FpML Account Type Population Script
 *
 * Populates the dq_account_types_artefact_tbl with reference data.
 * Dataset: fpml.account_type
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_account_types() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Account Type
-- =============================================================================

\echo '--- DQ Artefact FpML Account Type ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.account_type'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.account_type not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_account_types_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_account_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Client',
        1,
        'FPML_ACCOUNT_TYPE',
        'FpML',
        'The account contains trading activity or positions that belong to a client of the firm that opened the account.'
    );
    v_count := v_count + 1;
    insert into ores.dq_account_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'House',
        1,
        'FPML_ACCOUNT_TYPE',
        'FpML',
        'The account contains proprietary trading activity or positions, belonging to the firm that is the owner of the account.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_account_types_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_account_types_artefact' as entity, count(*) as count
from ores.dq_account_types_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_account_types_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Asset Class Population Script
 *
 * Populates the dq_asset_classes_artefact_tbl with reference data.
 * Dataset: fpml.asset_class
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_asset_classes() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Asset Class
-- =============================================================================

\echo '--- DQ Artefact FpML Asset Class ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.asset_class'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.asset_class not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_asset_classes_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_asset_classes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Commodity',
        1,
        'FPML_ASSET_CLASS',
        'FpML',
        'Commodity.'
    );
    v_count := v_count + 1;
    insert into ores.dq_asset_classes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Credit',
        1,
        'FPML_ASSET_CLASS',
        'FpML',
        'Credit.'
    );
    v_count := v_count + 1;
    insert into ores.dq_asset_classes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Equity',
        1,
        'FPML_ASSET_CLASS',
        'FpML',
        'Equity.'
    );
    v_count := v_count + 1;
    insert into ores.dq_asset_classes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ForeignExchange',
        1,
        'FPML_ASSET_CLASS',
        'FpML',
        'ForeignExchange.'
    );
    v_count := v_count + 1;
    insert into ores.dq_asset_classes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'InterestRate',
        1,
        'FPML_ASSET_CLASS',
        'FpML',
        'InterestRate.'
    );
    v_count := v_count + 1;
    insert into ores.dq_asset_classes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SecuritiesFinancing',
        1,
        'FPML_ASSET_CLASS',
        'FpML',
        'SecuritiesFinancing.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_asset_classes_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_asset_classes_artefact' as entity, count(*) as count
from ores.dq_asset_classes_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_asset_classes_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * Use dq_populate_asset_measures() to publish to production.
 */

set schema 'ores';

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
    from ores.dq_datasets_tbl
    where code = 'fpml.asset_measure'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.asset_measure not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_asset_measures_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
    insert into ores.dq_asset_measures_artefact_tbl (
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
from ores.dq_asset_measures_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_asset_measures_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Benchmark Rate Population Script
 *
 * Populates the dq_benchmark_rates_artefact_tbl with reference data.
 * Dataset: fpml.benchmark_rate
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_benchmark_rates() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Benchmark Rate
-- =============================================================================

\echo '--- DQ Artefact FpML Benchmark Rate ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.benchmark_rate'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.benchmark_rate not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_benchmark_rates_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AMERIBOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CORRA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CZEONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DESTR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DKK OIS',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EFFR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        '"EFFR" or "Fed Funds" per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EuroSTR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HUFONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KOFR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MIBOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MYOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NOWA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NZIONA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'POLONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'POLSTR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RUONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SARON',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SFXROD',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SHIR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SOFR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SORA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SORR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'STIBOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definition up to V3, Section 10.3 Overnight Rate Benchmarks. What is defined as "SEK OIS" in the 2006 ISDA Collateral Cash Price Matrix up to November 10, 2021 publication.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SWESTR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TELBOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'THOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TLREF',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TONA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks. What is defined as "TONAR" in the 2006 ISDA Collateral Cash Price Matrix.'
    );
    v_count := v_count + 1;
    insert into ores.dq_benchmark_rates_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'WIRON',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_benchmark_rates_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_benchmark_rates_artefact' as entity, count(*) as count
from ores.dq_benchmark_rates_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_benchmark_rates_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Business Center Population Script
 *
 * Populates the dq_business_centres_artefact_tbl with reference data.
 * Dataset: fpml.business_center
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_business_centres() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Business Center
-- =============================================================================

\echo '--- DQ Artefact FpML Business Center ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.business_center'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.business_center not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_business_centres_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AEAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abu Dhabi, Business Day (as defined in 2021 ISDA Definitions Section 2.1.10 (ii))'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AEAD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abu Dhabi, Settlement Day (as defined in 2021 ISDA Definitions Section 2.1.10 (i))'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AEDU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dubai, United Arab Emirates'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AMYE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Yerevan, Armenia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AOLU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Luanda, Angola'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ARBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Buenos Aires, Argentina'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ATVI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Vienna, Austria'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUAD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Adelaide, Australia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brisbane, Australia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUCA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Canberra, Australia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUDA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Darwin, Australia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUME',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Melbourne, Australia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUPE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Perth, Australia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUSY',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sydney, Australia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AZBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Baku, Azerbaijan'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BBBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bridgetown, Barbados'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BDDH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dhaka, Bangladesh'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BEBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brussels, Belgium'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BGSO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sofia, Bulgaria'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BHMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Manama, Bahrain'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BMHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hamilton, Bermuda'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BNBS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bandar Seri Begawan, Brunei'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BOLP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'La Paz, Bolivia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BRBD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brazil Business Day.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BRBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brasilia, Brazil.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BRRJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rio de Janeiro, Brazil.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BRSP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sao Paulo, Brazil.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BSNA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Nassau, Bahamas'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BWGA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Gaborone, Botswana'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BYMI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Minsk, Belarus'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CACL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Calgary, Canada'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CAFR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Fredericton, Canada.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CAMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Montreal, Canada'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CAOT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ottawa, Canada'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CATO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Toronto, Canada'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CAVA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Vancouver, Canada'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CAWI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Winnipeg, Canada'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CHBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Basel, Switzerland'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CHGE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Geneva, Switzerland'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CHZU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Zurich, Switzerland'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CIAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abidjan, Cote d''Ivoire'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CLSA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Santiago, Chile'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CMYA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Yaounde, Cameroon'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CNBE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Beijing, China'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CNSH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Shanghai, China'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'COBO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bogota, Colombia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CRSJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Jose, Costa Rica'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CWWI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Willemstad, Curacao'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CYNI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Nicosia, Cyprus'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CZPR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Prague, Czech Republic'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DECO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Cologne, Germany'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEDU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dusseldorf, Germany'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEFR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Frankfurt, Germany'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hannover, Germany'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEHH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hamburg, Germany'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DELE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Leipzig, Germany'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mainz, Germany'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEMU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Munich, Germany'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEST',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Stuttgart, Germany'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DKCO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Copenhagen, Denmark'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DOSD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Santo Domingo, Dominican Republic'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DZAL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Algiers, Algeria'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ECGU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Guayaquil, Ecuador'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EETA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tallinn, Estonia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EGCA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Cairo, Egypt'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ESAS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'ESAS Settlement Day (as defined in 2006 ISDA Definitions Section 7.1 and Supplement Number 15 to the 2000 ISDA Definitions)'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ESBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Barcelona, Spain'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ESMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Madrid, Spain'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ESSS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Sebastian, Spain'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ETAA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Addis Ababa, Ethiopia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EUR-ICESWAP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for ICE Swap rates based on EUR-EURIBOR rates'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EUTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'TARGET Settlement Day'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FIHE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Helsinki, Finland'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FRPA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Paris, France'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GBED',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Edinburgh, Scotland'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GBLO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'London, United Kingdom'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GBP-ICESWAP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for GBP ICE Swap rates'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GETB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tbilisi, Georgia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GGSP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Saint Peter Port, Guernsey'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GHAC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Accra, Ghana'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GIGI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Gibraltar, Gibraltar'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GMBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Banjul, Gambia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GNCO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Conakry, Guinea'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GRAT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Athens, Greece'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GTGC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Guatemala City, Guatemala'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GUGC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Guatemala City, Guatemala [DEPRECATED, to be removed in 2024. Replaced by GTGC.]'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HKHK',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hong Kong, Hong Kong'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HNTE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tegucigalpa, Honduras'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HRZA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Zagreb, Republic of Croatia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HUBU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Budapest, Hungary'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'IDJA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Jakarta, Indonesia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'IEDU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dublin, Ireland'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ILJE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Jerusalem, Israel'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ILS-SHIR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates of the ILS-SHIR index.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ILS-TELBOR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates of the ILS-TELBOR index.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ILTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tel Aviv, Israel'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INAH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ahmedabad, India'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bangalore, India'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INCH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Chennai, India'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INHY',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hyderabad, India'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INKO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kolkata, India'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INMU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mumbai, India'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INND',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New Delhi, India'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'IQBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Baghdad, Iraq'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'IRTE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Teheran, Iran'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ISRE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Reykjavik, Iceland'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ITMI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Milan, Italy'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ITRO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rome, Italy'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ITTU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Turin, Italy'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JESH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'St. Helier, Channel Islands, Jersey'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JMKI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kingston, Jamaica'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JOAM',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Amman, Jordan'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JPTO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tokyo, Japan'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KENA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Nairobi, Kenya'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KHPP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Phnom Penh, Cambodia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KRSE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Seoul, Republic of Korea'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KWKC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kuwait City, Kuwait'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KYGE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'George Town, Cayman Islands'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KZAL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Almaty, Kazakhstan'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LAVI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Vientiane, Laos'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LBBE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Beirut, Lebanon'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LKCO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Colombo, Sri Lanka'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LULU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Luxembourg, Luxembourg'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LVRI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Riga, Latvia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MACA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Casablanca, Morocco'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MARA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rabat, Morocco'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MCMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Monaco, Monaco'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MNUB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ulan Bator, Mongolia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MOMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Macau, Macao'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MTVA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Valletta, Malta'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MUPL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Port Louis, Mauritius'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MVMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Male, Maldives'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MWLI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lilongwe, Malawi'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MXMC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mexico City, Mexico'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MYKL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kuala Lumpur, Malaysia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MYLA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Labuan, Malaysia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MZMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Maputo, Mozambique'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NAWI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Windhoek, Namibia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NGAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abuja, Nigeria'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NGLA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lagos, Nigeria'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NLAM',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Amsterdam, Netherlands'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NLRO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rotterdam, Netherlands'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NOOS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Oslo, Norway'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NPKA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kathmandu, Nepal'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NYFD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New York Fed Business Day (as defined in 2006 ISDA Definitions Section 1.9, 2000 ISDA Definitions Section 1.9, and 2021 ISDA Definitions Section 2.1.7)'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NYSE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New York Stock Exchange Business Day (as defined in 2006 ISDA Definitions Section 1.10, 2000 ISDA Definitions Section 1.10, and 2021 ISDA Definitions Section 2.1.8)'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NZAU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Auckland, New Zealand'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NZBD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New Zealand Business Day (proposed effective date: 2025-10-06)'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NZWE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Wellington, New Zealand'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'OMMU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Muscat, Oman'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PAPC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Panama City, Panama'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PELI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lima, Peru'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PHMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Manila, Philippines'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PHMK',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Makati, Philippines'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PKKA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Karachi, Pakistan'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PLWA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Warsaw, Poland'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PRSJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Juan, Puerto Rico'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PTLI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lisbon, Portugal'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'QADO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Doha, Qatar'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ROBU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bucharest, Romania'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RSBE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Belgrade, Serbia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RUMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Moscow, Russian Federation'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SAAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abha, Saudi Arabia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SAJE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Jeddah, Saudi Arabia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SARI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Riyadh, Saudi Arabia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SEST',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Stockholm, Sweden'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SGSI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Singapore, Singapore'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SILJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ljubljana, Slovenia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SKBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bratislava, Slovakia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SLFR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Freetown, Sierra Leone'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SNDA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dakar, Senegal'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SVSS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Salvador, El Salvador'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'THBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bangkok, Thailand'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TNTU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tunis, Tunisia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TRAN',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ankara, Turkey'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TRIS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Istanbul, Turkey'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TTPS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Port of Spain, Trinidad and Tobago'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TWTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Taipei, Taiwan'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TZDA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dar es Salaam, Tanzania'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TZDO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dodoma, Tanzania'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UAKI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kiev, Ukraine'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UGKA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kampala, Uganda'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USBO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Boston, Massachusetts, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USCH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Chicago, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USCR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Charlotte, North Carolina, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USDC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Washington, District of Columbia, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USD-ICESWAP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for ICE Swap rates based on USD-LIBOR rates'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USD-MUNI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for the USD-Municipal Swap Index'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USDN',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Denver, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USDT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Detroit, Michigan, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USGS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'U.S. Government Securities Business Day (as defined in 2006 ISDA Definitions Section 1.11 and 2000 ISDA Definitions Section 1.11)'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USHL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Honolulu, Hawaii, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USHO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Houston, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USLA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Los Angeles, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USMB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mobile, Alabama, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USMN',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Minneapolis, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USNY',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New York, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USPO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Portland, Oregon, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USSA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sacramento, California, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USSE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Seattle, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USSF',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Francisco, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USWT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Wichita, United States'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UYMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Montevideo, Uruguay'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UZTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tashkent, Uzbekistan'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'VECA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Caracas, Venezuela'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'VGRT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Road Town, Virgin Islands (British)'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'VNHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hanoi, Vietnam'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'VNHC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ho Chi Minh (formerly Saigon), Vietnam'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'YEAD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Aden, Yemen'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ZAJO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Johannesburg, South Africa'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ZMLU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lusaka, Zambia'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ZWHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Harare, Zimbabwe'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_business_centres_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_business_centres_artefact' as entity, count(*) as count
from ores.dq_business_centres_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_business_centres_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Business Process Population Script
 *
 * Populates the dq_business_processes_artefact_tbl with reference data.
 * Dataset: fpml.business_process
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_business_processes() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Business Process
-- =============================================================================

\echo '--- DQ Artefact FpML Business Process ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.business_process'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.business_process not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_business_processes_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Allocation',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for splitting a trade across accounts.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Clearing',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for novating a trade to a central counterparty (with margining) for credit risk mitigation.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Confirmation',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for verifying the terms of a trade.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Execution',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for executing a trade.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Reconciliation',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for comparing representations of a trade or portfolio for the purpose of identifying and resolving discrepancies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_business_processes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Settlement',
        1,
        'FPML_BUSINESS_PROCESS',
        'FpML',
        'Process for calculating payment amounts and performing payments as required by the terms of a transaction.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_business_processes_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_business_processes_artefact' as entity, count(*) as count
from ores.dq_business_processes_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_business_processes_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Cashflow Type Population Script
 *
 * Populates the dq_cashflow_types_artefact_tbl with reference data.
 * Dataset: fpml.cashflow_type
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_cashflow_types() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Cashflow Type
-- =============================================================================

\echo '--- DQ Artefact FpML Cashflow Type ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.cashflow_type'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.cashflow_type not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_cashflow_types_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AmendmentFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow associated with an amendment lifecycle event.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AssignmentFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow resulting from the assignment of a contract to a new counterparty.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Coupon',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow corresponding to the periodic accrued interests.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CreditEvent',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cashflow resulting from a credit event.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DividendReturn',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow corresponding to the synthetic dividend of an equity underlyer asset traded through a derivative instrument.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ExerciseFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow associated with an exercise lifecycle event.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Fee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A generic term for describing a non-scheduled cashflow that can be associated either with the initial contract, with some later corrections to it (e.g. a correction to the day count fraction that has a cashflow impact) or with some lifecycle events. Fees that are specifically associated with termination and partial termination, increase, amendment, and exercise events are qualified accordingly.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'IncreaseFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow associated with an increase lifecycle event.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'InterestReturn',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow corresponding to the return of the interest rate portion of a derivative instrument that has different types of underlying assets, such as a total return swap.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PartialTerminationFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow associated with a partial termination lifecycle event.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Premium',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'The premium associated with an OTC contract such as an option or a cap/floor.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PriceReturn',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow corresponding to the return of the price portion of a derivative instrument that has different types of underlying assets, such as a total return swap.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PrincipalExchange',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow which amount typically corresponds to the notional of the contract and that is exchanged between the parties on trade inception and reverted back when the contract is terminated.'
    );
    v_count := v_count + 1;
    insert into ores.dq_cashflow_types_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TerminationFee',
        1,
        'FPML_CASHFLOW_TYPE',
        'FpML',
        'A cash flow associated with a termination lifecycle event.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_cashflow_types_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_cashflow_types_artefact' as entity, count(*) as count
from ores.dq_cashflow_types_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_cashflow_types_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Cftc Entity Classification Population Script
 *
 * Populates the dq_entity_classifications_artefact_tbl with reference data.
 * Dataset: fpml.cftc_entity_classification
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_entity_classifications() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Cftc Entity Classification
-- =============================================================================

\echo '--- DQ Artefact FpML Cftc Entity Classification ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.cftc_entity_classification'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.cftc_entity_classification not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_entity_classifications_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CommodityPool',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A commodity pool as defined in CFTC CEA § 2(h)(7)(C).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EmployeeBenefitPlan',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'An employee benefit plan as defined in paragraphs (3) and (32) of section 1002 of title 29 of the Commodity Exchange Act (CEA).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FinancialSectorPerson',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A person predominantly engaged in activities that are in the business of banking, or in activities that are financial in nature, as defined in section 1843(k) of title 12 of the Commodity Exchange Act (CEA).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MSBSP',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A major security based swap participant as defined in CFTC CEA § 2(h)(7)(C).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MSP',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A major swap participant as defined in CFTC CEA § 2(h)(7)(C).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'None',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'None of the listed in the scheme.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PrivateFund',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A private fund as defined in section 80b-2(a) of title 15 of the Commodity Exchange Act (CEA).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SBSD',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A security-based swap dealer as defined in CFTC CEA § 2(h)(7)(C).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SD',
        1,
        'FPML_CFTC_ENTITY_CLASSIFICATION',
        'FpML',
        'A swap dealer as defined in CFTC CEA § 2(h)(7)(C).'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_entity_classifications_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_entity_classifications_artefact' as entity, count(*) as count
from ores.dq_entity_classifications_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_entity_classifications_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Cftc Organization Type Population Script
 *
 * Populates the dq_entity_classifications_artefact_tbl with reference data.
 * Dataset: fpml.cftc_organization_type
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_entity_classifications() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Cftc Organization Type
-- =============================================================================

\echo '--- DQ Artefact FpML Cftc Organization Type ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.cftc_organization_type'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.cftc_organization_type not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_entity_classifications_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Agency',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'An agency as defined in 5 U.S.C. 551(1), a federal instrumentality, or a federal authority.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CharteredPursuantToFederalLaw',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'An entity chartered pursuant to federal law after formation (example: an organization listed in title 36 of the U.S. Code).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EstablishedByFederalEntity',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'An entity that was established by, or at the direction of, one or more of the entities listed in clause (1), or has an ultimate parent listed in its LEI reference data that is an entity listed in clause (1) or in the first part of this clause (2).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FederallyFundedResearchAndDevelopmentCenter',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'A federally funded research and development center on the master list referenced in 48 CFR 35.017-6.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GovernmentCorporation',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'A government corporation (examples: as such term is defined in 5 U.S.C. 103(1) or in 31 U.S.C. 9101).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GovernmentSponsoredEnterprise',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'A government-sponsored enterprise (example: as such term is defined in 2 U.S.C. 622(8)).'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USCListedExecutiveDepartment',
        1,
        'FPML_CFTC_ORGANIZATION_TYPE',
        'FpML',
        'An executive department listed in 5 U.S.C. 101.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_entity_classifications_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_entity_classifications_artefact' as entity, count(*) as count
from ores.dq_entity_classifications_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_entity_classifications_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Entity Type Population Script
 *
 * Populates the dq_entity_classifications_artefact_tbl with reference data.
 * Dataset: fpml.entity_type
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_entity_classifications() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Entity Type
-- =============================================================================

\echo '--- DQ Artefact FpML Entity Type ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.entity_type'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.entity_type not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_entity_classifications_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Asian',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Asian.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AustralianAndNewZealand',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Australian and New Zealand.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EuropeanEmergingMarkets',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of European Emerging Markets.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Japanese',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Japanese.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NorthAmericanHighYield',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of North American High Yield.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NorthAmericanInsurance',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of North American Insurance.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NorthAmericanInvestmentGrade',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of North American Investment Grade.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Singaporean',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Singaporean.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'WesternEuropean',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Western European.'
    );
    v_count := v_count + 1;
    insert into ores.dq_entity_classifications_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'WesternEuropeanInsurance',
        1,
        'FPML_ENTITY_TYPE',
        'FpML',
        'Entity Type of Western European Insurance.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_entity_classifications_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_entity_classifications_artefact' as entity, count(*) as count
from ores.dq_entity_classifications_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_entity_classifications_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Hkma Rewrite Party Relationship Type Population Script
 *
 * Populates the dq_party_relationships_artefact_tbl with reference data.
 * Dataset: fpml.hkma_rewrite_party_relationship_type
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_party_relationships() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Hkma Rewrite Party Relationship Type
-- =============================================================================

\echo '--- DQ Artefact FpML Hkma Rewrite Party Relationship Type ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.hkma_rewrite_party_relationship_type'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.hkma_rewrite_party_relationship_type not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_party_relationships_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_party_relationships_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Intragroup',
        1,
        'FPML_HKMA_REWRITE_PARTY_RELATIONSHIP_TYPE',
        'FpML',
        'Intragroup as defined by Hong Kong Monetary Authority (HKMA) Rewrite field 189 - Intragroup.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_relationships_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NotIntragroup',
        1,
        'FPML_HKMA_REWRITE_PARTY_RELATIONSHIP_TYPE',
        'FpML',
        'Not intragroup as defined by Hong Kong Monetary Authority (HKMA) Rewrite field 189 - Intragroup.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_party_relationships_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_party_relationships_artefact' as entity, count(*) as count
from ores.dq_party_relationships_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_party_relationships_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Hkma Rewrite Regulatory Corporate Sector Population Script
 *
 * Populates the dq_regulatory_corporate_sectors_artefact_tbl with reference data.
 * Dataset: fpml.hkma_rewrite_regulatory_corporate_sector
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_regulatory_corporate_sectors() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Hkma Rewrite Regulatory Corporate Sector
-- =============================================================================

\echo '--- DQ Artefact FpML Hkma Rewrite Regulatory Corporate Sector ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.hkma_rewrite_regulatory_corporate_sector'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.hkma_rewrite_regulatory_corporate_sector not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_regulatory_corporate_sectors_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AIFD',
        1,
        'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
        'HKMA',
        'Alternative Investment Fund.'
    );
    v_count := v_count + 1;
    insert into ores.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ASSU',
        1,
        'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
        'HKMA',
        'Assurance Undertaking.'
    );
    v_count := v_count + 1;
    insert into ores.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CCPS',
        1,
        'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
        'HKMA',
        'Central Counterparty.'
    );
    v_count := v_count + 1;
    insert into ores.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CDTI',
        1,
        'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
        'HKMA',
        'Credit Institution.'
    );
    v_count := v_count + 1;
    insert into ores.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CSDS',
        1,
        'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
        'HKMA',
        'Central Securities Depository.'
    );
    v_count := v_count + 1;
    insert into ores.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INUN',
        1,
        'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
        'HKMA',
        'Insurance Undertaking.'
    );
    v_count := v_count + 1;
    insert into ores.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INVF',
        1,
        'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
        'HKMA',
        'Investment Firm.'
    );
    v_count := v_count + 1;
    insert into ores.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ORPI',
        1,
        'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
        'HKMA',
        'Occupational Retirement Provision Institution.'
    );
    v_count := v_count + 1;
    insert into ores.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'OTHR',
        1,
        'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
        'HKMA',
        'Other.'
    );
    v_count := v_count + 1;
    insert into ores.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'REIN',
        1,
        'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
        'HKMA',
        'Reinsurance Undertaking.'
    );
    v_count := v_count + 1;
    insert into ores.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UCIT',
        1,
        'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
        'HKMA',
        'UCITS Management Company.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_regulatory_corporate_sectors_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_regulatory_corporate_sectors_artefact' as entity, count(*) as count
from ores.dq_regulatory_corporate_sectors_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_regulatory_corporate_sectors_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Local Jurisdiction Population Script
 *
 * Populates the dq_local_jurisdictions_artefact_tbl with reference data.
 * Dataset: fpml.local_jurisdiction
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_local_jurisdictions() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Local Jurisdiction
-- =============================================================================

\echo '--- DQ Artefact FpML Local Jurisdiction ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.local_jurisdiction'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.local_jurisdiction not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_local_jurisdictions_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Afghanistan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Afghan Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Applicable',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Follows Local Jurisdiction as per MCA to this Transaction.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Australia',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Australian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'China',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Chinese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HongKong',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Hong Kong Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'India',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Indian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Indonesia',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Indonesian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Japan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Japanese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Korea',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Korean Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Malaysia',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Malaysian Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NewZealand',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'New Zealand Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NotApplicable',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'No Local Jurisdiction applies to this Transaction.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Pakistan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Pakistani Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Philippines',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Philippine Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Singapore',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Singaporean Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Taiwan',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Taiwanese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Thailand',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Thai Local Jurisdiction applies.'
    );
    v_count := v_count + 1;
    insert into ores.dq_local_jurisdictions_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Vietnam',
        1,
        'FPML_LOCAL_JURISDICTION',
        'FpML',
        'Vietnamese Local Jurisdiction applies.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_local_jurisdictions_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_local_jurisdictions_artefact' as entity, count(*) as count
from ores.dq_local_jurisdictions_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_local_jurisdictions_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Party Relationship Type Population Script
 *
 * Populates the dq_party_relationships_artefact_tbl with reference data.
 * Dataset: fpml.party_relationship_type
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_party_relationships() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Party Relationship Type
-- =============================================================================

\echo '--- DQ Artefact FpML Party Relationship Type ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.party_relationship_type'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.party_relationship_type not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_party_relationships_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_party_relationships_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Affiliated',
        1,
        'FPML_PARTY_RELATIONSHIP_TYPE',
        'FpML',
        'Indicates whether the transaction is between two affiliated entities. It is referred to as Inter-affiliate under the Canadian CSA reporting regime.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_relationships_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Inter-Dealer',
        1,
        'FPML_PARTY_RELATIONSHIP_TYPE',
        'FpML',
        'Indicates the transaction is between two dealers.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_relationships_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Intragroup',
        1,
        'FPML_PARTY_RELATIONSHIP_TYPE',
        'FpML',
        'Indicates whether the contract was concluded as an intra-group transaction, defined in Article 3, 4(2), 11(6) to 11(10) of EMIR.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_party_relationships_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_party_relationships_artefact' as entity, count(*) as count
from ores.dq_party_relationships_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_party_relationships_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Party Role Population Script
 *
 * Populates the dq_party_roles_artefact_tbl with reference data.
 * Dataset: fpml.party_role
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_party_roles() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Party Role
-- =============================================================================

\echo '--- DQ Artefact FpML Party Role ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.party_role'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.party_role not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_party_roles_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Accountant',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Organization responsible for preparing the accounting for the trade.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AllocationAgent',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The organization responsible for supplying the allocations for a trade to be allocated to multiple accounts/organizations.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ArrangingBroker',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The organization that arranged the trade, i.e. brought together the counterparties. Synonyms/Alternatives: Inter-dealer broker, agent.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Beneficiary',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Organization that suffers the economic benefit of the trade. The beneficiary may be distinct from the principal/counterparty - an example occurs when a hedge fund trades via a prime broker; in this case the principal is the prime broker, but the beneficiary is the hedge fund. This can be represented as a payer/receiver account in the name of the hedge fund, but it is also possible to add the party role of "Beneficiary" at the partyTradeInformation level.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BookingParty',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The entity for which the organization supporting the trade''s processing has booked/recorded the trade. This is used in non-reporting workflows situations in which the trade doesn''t need to be reported but a firm still wants to specify their own side.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Buyer',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Acquirer of the legal title to the financial instrument. In the case of an option, the buyer is the holder of the option. In the case of a swap or forward, the buyer will be determined by industry best practice. This does not refer to an investor or investment manager or other organization on what is typically called the "Buy side"; for that, see the "Client" role. Corresponds to "Buyer" as defined in certain regulations such as ESMA MiFID II/MIFIR RTS 22 field 9.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BuyerDecisionMaker',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The party or person who, having legal authority to act on behalf of the trade counterparty acting as Buyer as defined in this coding scheme, made the decision to acquire the financial instrument. Corresponds to "buyer decision maker" as defined in ESMA''s MIFIR RTS 23 report. This does not refer to the decision maker for what is traditionally called the "Buy side"; for that, see the "Client Decision Maker" role.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ClearingClient',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'An organization that clears trades through a clearing house, via a clearing broker (member of the clearing house) who acts as an agent on its behalf. The term "client" refers to the organization''s role in the clearing process in relation to its clearing broker, and not whether it is a price maker or taker in the execution process.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ClearingExceptionParty',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'A party to the trade that claims a clearing exception, such as an end-user exception under Dodd-Frank Act provisions.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ClearingFirm',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Organization that submits the trade to a clearing house on behalf of the principal. Synonyms/alternates: Futures Commission Merchant (FCM), Clearing Broker, Clearing Member Firm. Some implementations use "Clearing Broker" as synonym.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ClearingOrganization',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The organization that acts as a central counterparty to clear a derivatives contract. This is used to represent the role of Central Counterparties (CCPs) or Derivative Clearing Organizations (DCOs). Sometimes called "ClearingService". Some implementations also use the term "Clearer".'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Client',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Client as defined under ESMA MIFIR. This is generally the investor or other client of an investment firm, and is synonymous with the Beneficiary in many circumstances.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ClientDecisionMaker',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The party or person who, having legal authority to act on behalf of a trade counterparty, made the decision to acquire or sell the financial instrument.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ConfirmationPlatform',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Organization serving as a financial intermediary for the purposes of electronic confirmation or providing services for post-processing of transactional data.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ContractualParty',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'A party to a contractual document. If the intended usage relates to the context of the trade lifecycle, more specific annotations have been defined which might be more appropriate.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Counterparty',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'An economic counterparty to the trade. Synonym: principal.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CounterPartyAffiliate',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Organization offiially attached to the counterparty. e.g. partner, branch, subsidiary.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CounterPartyUltimateParent',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The topmost entity or organization, within the corporate hierarchy, responsible for the reporting party.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CreditSupportProvider',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Organization that enhances the credit of another organization (similar to guarantor, but may not fully guarantee the obligation).'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Custodian',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Organization that maintains custody of the asset represented by the trade on behalf of the owner/principal.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DataSubmitter',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Entity submitting the transaction report to the competent authority.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DisputingParty',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Organization that is disputing the trade or transaction.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DocumentRepository',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'A marketplace organization which purpose is to maintain document records. If the intended usage relates to the context of the trade lifecycle, more specific annotations have been defined which might be more appropriate.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ExecutingBroker',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The (generally sell-side) organization that executed the trade; the price-making party.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ExecutingEntity',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Entity executing the transaction. If the transaction is executed directly by the reporting party, it will be the reporting party. If it is executed by an execution agent or an affiliated party on behalf of the reporting party, it will be that affiliate or agent.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ExecutionAgent',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The (generally buy-side) organization that acts to execute trades on behalf of an investor. Typically this is an investment manager or asset manager, and also makes the investment decisions for the investor. If required, a separate InvestmentDecision role can be specified to distinguish that the party making the investment decision is different.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ExecutionFacility',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The facility, exchange, or market where the trade was executed. Synonym: Swap Execution Facility, Designated Contract Market, Execution Venue.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Guarantor',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Organization that backs (guarantees) the credit risk of the trade.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MarginAffiliate',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Margin affiliate as defined by U.S. margin and capital rules §23.151.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'OrderTransmitter',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The entity transmitting the order to the reporting firm. Synonym: Transmitting Firm.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PrimeBroker',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The organization that takes on or took on the credit risk for this trade by stepping in between the two economic parties (without a central counterparty clearing mechanism).'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PriorTradeRepository',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The trade repository at which the trade was reported previous to the current trade repository.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PTRRCompressionProvider',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'A party providing a post trade risk reduction service in the form of compression.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PTRRRebalancingProvider',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'A party providing a post trade risk reduction service in the form of portfolio rebalancing.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PublicationVenue',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The reporting service (whether trade repository, market data service, or exchange/facility/venue data distribution service) that published the report of this trade.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ReportingParty',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The party with the regulatory responsibility to report this trade.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ReportingPartyAffiliate',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'Organization offiially attached to the reporting party e.g. partner, branch, subsidiary.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ReportingPartyUltimateParent',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The topmost entity or organization, within the corporate hierarchy, responsible for the reporting party.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Seller',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'A counterparty in a trade, which performs in one of the following capacities: 1) it transfers or agrees to transfer in the future an instrument or title to that instrument in exchange for payment, 2) it writes a derivatives instrument such as an option or a swap in which it provides risk protection to the buyer. This does not refer to the broker/dealer or other organization on what is typically called the "Sell side"; for that, see the "Executing Broker" role. Corresponds to "Seller" as defined in certain regulations such as ESMA MiFID II/MIFIR RTS 22 field 16.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SellerDecisionMaker',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The party or person who, having legal authority to act on behalf of the trade counterparty acting as Seller as defined in this coding scheme, made the decision to sell the financial instrument. Corresponds to "seller decision maker" as defined in ESMA''s MIFIR RTS 23 report. This does not refer to the decision maker for what is traditionally called the "Sell side"; for that, see the "Trader" person role.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SettlementAgent',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The organization that makes or receives payments on behalf of the given principal party.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TradeRepository',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'An organization that maintains records of the trade for regulatory reporting purposes.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TradeSource',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The organization that originally supplied the record of the trade. In the context of regulatory reporting, it is the submitter of the trade record to a regulator or TR.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TradingManager',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'The entity responsible for managing the assets/investments of this party. Synonnym: Asset Manager, Investment Manager, Trading Advisory.'
    );
    v_count := v_count + 1;
    insert into ores.dq_party_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TradingPartner',
        1,
        'FPML_PARTY_ROLE',
        'FpML',
        'An entity with which this party trades from time to time, ie. with which it acts as a counterparty on some transactions. This role is used for static reference data, not individual transactions.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_party_roles_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_party_roles_artefact' as entity, count(*) as count
from ores.dq_party_roles_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_party_roles_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Person Role Population Script
 *
 * Populates the dq_person_roles_artefact_tbl with reference data.
 * Dataset: fpml.person_role
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_person_roles() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Person Role
-- =============================================================================

\echo '--- DQ Artefact FpML Person Role ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.person_role'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.person_role not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_person_roles_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Broker',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'The person who arranged with a client to execute the trade.'
    );
    v_count := v_count + 1;
    insert into ores.dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Buyer',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Acquirer of the legal title to the financial instrument.'
    );
    v_count := v_count + 1;
    insert into ores.dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Custodian',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'The operational contact at the custodian.'
    );
    v_count := v_count + 1;
    insert into ores.dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DecisionMaker',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'The party or person with legal responsibility for authorization of the execution of the transaction.'
    );
    v_count := v_count + 1;
    insert into ores.dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ExecutionWithinFirm',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Person within the firm who is responsible for execution of the transaction.'
    );
    v_count := v_count + 1;
    insert into ores.dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'InvestmentDecisionMaker',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Person who is responsible for making the investment decision.'
    );
    v_count := v_count + 1;
    insert into ores.dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LoanCloser',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Individual responsible for managing the closing-related operational servicing of an asset.'
    );
    v_count := v_count + 1;
    insert into ores.dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LoanServicer',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Individual responsible for ongoing operational servicing of the asset. E.g. managing principal draws and repayments, interest and fee payments, etc.'
    );
    v_count := v_count + 1;
    insert into ores.dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Seller',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'Seller of the legal title to the financial instrument.'
    );
    v_count := v_count + 1;
    insert into ores.dq_person_roles_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Trader',
        1,
        'FPML_PERSON_ROLE',
        'FpML',
        'The person who executed the trade.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_person_roles_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_person_roles_artefact' as entity, count(*) as count
from ores.dq_person_roles_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_person_roles_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Reporting Regime Population Script
 *
 * Populates the dq_reporting_regimes_artefact_tbl with reference data.
 * Dataset: fpml.reporting_regime
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_reporting_regimes() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Reporting Regime
-- =============================================================================

\echo '--- DQ Artefact FpML Reporting Regime ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.reporting_regime'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.reporting_regime not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_reporting_regimes_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ASIC',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Australian Securities and Investments Commission Derivative Transaction Rules (Reporting)'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.Rule.91-507',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Rule 91-507 Derivatives: Trade Repositories and Derivatives Data. Harmonized rule adopted by Canadian provinces and territories.'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DoddFrankAct',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Dodd-Frank Act (US)'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EMIR',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'European Markets Infrastructure Regulation'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HKMA',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Hong Kong Monetary Authority'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JFSA',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Japan Financial Services Authority'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MAS',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'The Monetary Authority of Singapore'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MiFID',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Markets in Financial Instruments Directive'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MiFIDII',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Markets in Financial Instruments Directive II'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MiFIR',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Markets in Financial Instruments Regulation'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ODRF',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'OTC Derivatives Regulators Forum'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RussianFederation',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Russian regulatory reporting'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SFTR',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Securities Financing Transactions Regulation'
    );
    v_count := v_count + 1;
    insert into ores.dq_reporting_regimes_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UKEMIR',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'United Kingdom European Markets Infrastructure Regulation'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_reporting_regimes_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_reporting_regimes_artefact' as entity, count(*) as count
from ores.dq_reporting_regimes_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_reporting_regimes_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
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
 * DQ Artefact FpML Supervisory Body Population Script
 *
 * Populates the dq_supervisory_bodies_artefact_tbl with reference data.
 * Dataset: fpml.supervisory_body
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_supervisory_bodies() to publish to production.
 */

set schema 'ores';

-- =============================================================================
-- DQ Artefact FpML Supervisory Body
-- =============================================================================

\echo '--- DQ Artefact FpML Supervisory Body ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where code = 'fpml.supervisory_body'
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.supervisory_body not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores.dq_supervisory_bodies_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ASIC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Australian Securities and Investments Commission'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BankOfRussia',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Central Bank of the Russian Federation'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.AB.ASC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Alberta Securities Commission'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.BC.BCSC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'British Columbia Securities Commission'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.MB.MSC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'The Manitoba Securities Commission'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.NB.FCSC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Financial and Consumer Services Commission'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.NL.DSS',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Deputy Superintendent of Securities, Service Newfoundland and Labrador'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.NS.NSSC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Nova Scotia Securities Commission'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.NT.NTSO',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Northwest Territories Securities Office'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.NU.NSO',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Nunavut Securities Office, Government of Nunavut'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.ON.OSC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Ontario Securities Commission'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.PEI.OSS',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Office of the Superintendent of Securities'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.QC.AMF',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Autorite des marches financiers'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.SK.FCAA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Financial and Consumer Affairs Authority of Saskatchewan'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.YT.OSS',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Office of the Superintendent of Securities'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CFTC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Commodity Futures Trading Commission (US)'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ESMA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'European Securities and Markets Authority (European Union)'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FCA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Financial Conduct Authority (UK)'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Fed',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Federal Reserve (US)'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HKMA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Hong Kong Monetary Authority (China)'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JFSA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Japan Financial Services Authority (Japan)'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MAS',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'The Monetary Authority of Singapore'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ODRF',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'OTC Derivatives Regulators Forum'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SEC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Securities and Exchange Commission (US)'
    );
    v_count := v_count + 1;
    insert into ores.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UKFSA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Deprecated usage: FCA replaces UKFSA'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_supervisory_bodies_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_supervisory_bodies_artefact' as entity, count(*) as count
from ores.dq_supervisory_bodies_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores.dq_supervisory_bodies_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
