/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { AverageOisConvention } from '../domain/average_ois_convention.js';
import type { BaseCorrelationConfig } from '../domain/base_correlation_config.js';
import type { BmaBasisSwapConvention } from '../domain/bma_basis_swap_convention.js';
import type { BondFutureVolatilityConfig } from '../domain/bond_future_volatility_config.js';
import type { BondYieldConvention } from '../domain/bond_yield_convention.js';
import type { CapFloorVolatilityConfig } from '../domain/cap_floor_volatility_config.js';
import type { CdsConvention } from '../domain/cds_convention.js';
import type { CdsVolatilityConfig } from '../domain/cds_volatility_config.js';
import type { CdsVolatilityTerm } from '../domain/cds_volatility_term.js';
import type { CmsSpreadOptionConvention } from '../domain/cms_spread_option_convention.js';
import type { CommodityCurveConfig } from '../domain/commodity_curve_config.js';
import type { CommodityForwardConvention } from '../domain/commodity_forward_convention.js';
import type { CommodityFutureConvention } from '../domain/commodity_future_convention.js';
import type { CommodityPriceSegment } from '../domain/commodity_price_segment.js';
import type { CommodityVolatilityConfig } from '../domain/commodity_volatility_config.js';
import type { CrossCurrencyBasisConvention } from '../domain/cross_currency_basis_convention.js';
import type { CrossCurrencyFixFloatConvention } from '../domain/cross_currency_fix_float_convention.js';
import type { CurrencyPair } from '../domain/currency_pair.js';
import type { CurrencyPairConvention } from '../domain/currency_pair_convention.js';
import type { CurveBootstrapConfig } from '../domain/curve_bootstrap_config.js';
import type { CurveConfiguration } from '../domain/curve_configuration.js';
import type { CurveConfigurationSection } from '../domain/curve_configuration_section.js';
import type { CurveCorrelationConfig } from '../domain/curve_correlation_config.js';
import type { CurveDefinition } from '../domain/curve_definition.js';
import type { CurveGlobalReport } from '../domain/curve_global_report.js';
import type { CurveParametricSmile } from '../domain/curve_parametric_smile.js';
import type { CurveParametricSmileParameter } from '../domain/curve_parametric_smile_parameter.js';
import type { CurveQuote } from '../domain/curve_quote.js';
import type { CurveReportConfiguration } from '../domain/curve_report_configuration.js';
import type { CurveSecurityConfig } from '../domain/curve_security_config.js';
import type { CurveSegment } from '../domain/curve_segment.js';
import type { CurveSegmentCurve } from '../domain/curve_segment_curve.js';
import type { CurveVolatilityConfig } from '../domain/curve_volatility_config.js';
import type { DefaultCurveConfig } from '../domain/default_curve_config.js';
import type { DefaultCurveConfiguration } from '../domain/default_curve_configuration.js';
import type { DepositConvention } from '../domain/deposit_convention.js';
import type { EquityCurveConfig } from '../domain/equity_curve_config.js';
import type { EquityVolatilityConfig } from '../domain/equity_volatility_config.js';
import type { FraConvention } from '../domain/fra_convention.js';
import type { FutureConvention } from '../domain/future_convention.js';
import type { FxOptionConvention } from '../domain/fx_option_convention.js';
import type { FxVolatilityConfig } from '../domain/fx_volatility_config.js';
import type { IborIndexConvention } from '../domain/ibor_index_convention.js';
import type { InflationCapFloorVolatilityConfig } from '../domain/inflation_cap_floor_volatility_config.js';
import type { InflationCurveConfig } from '../domain/inflation_curve_config.js';
import type { InflationSeasonalityFactor } from '../domain/inflation_seasonality_factor.js';
import type { InflationSwapConvention } from '../domain/inflation_swap_convention.js';
import type { IntradayPowerCurveConfig } from '../domain/intraday_power_curve_config.js';
import type { IntradayPowerLoadConvention } from '../domain/intraday_power_load_convention.js';
import type { OisConvention } from '../domain/ois_convention.js';
import type { OvernightIndexConvention } from '../domain/overnight_index_convention.js';
import type { SwapConvention } from '../domain/swap_convention.js';
import type { SwapIndexConvention } from '../domain/swap_index_convention.js';
import type { SwaptionVolatilityConfig } from '../domain/swaption_volatility_config.js';
import type { TenorBasisSwapConvention } from '../domain/tenor_basis_swap_convention.js';
import type { TenorBasisTwoSwapConvention } from '../domain/tenor_basis_two_swap_convention.js';
import type { YieldCurveConfig } from '../domain/yield_curve_config.js';
import type { YieldVolatilityConfig } from '../domain/yield_volatility_config.js';
import type { ZeroConvention } from '../domain/zero_convention.js';
import type { ZeroInflationIndexConvention } from '../domain/zero_inflation_index_convention.js';

/**
 * @brief An FX convention as a conventions document carries it: the pair, its
 * convention, and the advance calendars the document lists.
 */
export interface FxConvention {
    pair: CurrencyPair;
    convention: CurrencyPairConvention;
    spot_days: number;
    advance_calendars: string[];
}

/**
 * @brief One ORE conventions document as the rows refdata stores.
 *
 * The instrument conventions belong to a party. The index and FX conventions
 * are world data, which every party in the tenant shares.
 */
export interface ConventionsDocument {
    zero: ZeroConvention[];
    average_ois: AverageOisConvention[];
    bma_basis_swap: BmaBasisSwapConvention[];
    cross_currency_basis: CrossCurrencyBasisConvention[];
    cross_currency_fix_float: CrossCurrencyFixFloatConvention[];
    tenor_basis_swap: TenorBasisSwapConvention[];
    tenor_basis_two_swap: TenorBasisTwoSwapConvention[];
    deposit: DepositConvention[];
    swap: SwapConvention[];
    swap_index: SwapIndexConvention[];
    future: FutureConvention[];
    fx_option: FxOptionConvention[];
    inflation_swap: InflationSwapConvention[];
    intraday_power_load: IntradayPowerLoadConvention[];
    ois: OisConvention[];
    fra: FraConvention[];
    ibor_index: IborIndexConvention[];
    overnight_index: OvernightIndexConvention[];
    zero_inflation_index: ZeroInflationIndexConvention[];
    fx: FxConvention[];
    cds: CdsConvention[];
    cms_spread_option: CmsSpreadOptionConvention[];
    commodity_future: CommodityFutureConvention[];
    commodity_forward: CommodityForwardConvention[];
    bond_yield: BondYieldConvention[];
}

/**
 * @brief One ORE curve configuration document as the rows refdata stores.
 *
 * The header row and every child row the document maps to, grouped by table.
 * Refdata stores and reads the document whole; a caller in another component
 * reaches it through these operations, never refdata's tables.
 */
export interface CurveConfigurationDocument {
    config: CurveConfiguration;
    sections: CurveConfigurationSection[];
    definitions: CurveDefinition[];
    yield_curves: YieldCurveConfig[];
    equity_curves: EquityCurveConfig[];
    inflation_curves: InflationCurveConfig[];
    default_curves: DefaultCurveConfig[];
    commodity_curves: CommodityCurveConfig[];
    fx_volatilities: FxVolatilityConfig[];
    yield_volatilities: YieldVolatilityConfig[];
    base_correlations: BaseCorrelationConfig[];
    correlations: CurveCorrelationConfig[];
    report_configurations: CurveReportConfiguration[];
    cds_volatilities: CdsVolatilityConfig[];
    cds_volatility_terms: CdsVolatilityTerm[];
    volatility_configs: CurveVolatilityConfig[];
    inflation_cap_floor_volatilities: InflationCapFloorVolatilityConfig[];
    swaption_volatilities: SwaptionVolatilityConfig[];
    cap_floor_volatilities: CapFloorVolatilityConfig[];
    parametric_smiles: CurveParametricSmile[];
    parametric_smile_parameters: CurveParametricSmileParameter[];
    equity_volatilities: EquityVolatilityConfig[];
    commodity_volatilities: CommodityVolatilityConfig[];
    bond_future_volatilities: BondFutureVolatilityConfig[];
    global_reports: CurveGlobalReport[];
    commodity_price_segments: CommodityPriceSegment[];
    default_curve_configurations: DefaultCurveConfiguration[];
    seasonality_factors: InflationSeasonalityFactor[];
    securities: CurveSecurityConfig[];
    intraday_power_curves: IntradayPowerCurveConfig[];
    bootstrap_configs: CurveBootstrapConfig[];
    segments: CurveSegment[];
    segment_curves: CurveSegmentCurve[];
    quotes: CurveQuote[];
}

/**
 * @brief Stores a curve configuration document.
 *
 * The session must act for a party, which owns every row. The header carries the reporting
 * configuration it fills, which is what a get finds it by.
 */
export interface SaveCurveConfigurationDocumentRequest {
    document: CurveConfigurationDocument;
}

/**
 * @brief The id of the header the save stored.
 */
export interface SaveCurveConfigurationDocumentResponse {
    success: boolean;
    message: string;
    id: string;
}

/**
 * @brief Reads a curve configuration document by the reporting configuration it fills.
 */
export interface GetCurveConfigurationDocumentRequest {
    configuration_id: string;
    /** When the session acts for no party, as a workflow step's does, the party whose rows to read. */
    party_id: string;
}

/**
 * @brief A curve configuration document, when one fills the configuration.
 */
export interface GetCurveConfigurationDocumentResponse {
    success: boolean;
    message: string;
    document: CurveConfigurationDocument;
}

/**
 * @brief Deletes a curve configuration document that fills a reporting configuration.
 *
 * A run import's compensation sends it, to undo a save. Deleting a
 * configuration no document fills succeeds, so a compensation can run twice.
 */
export interface DeleteCurveConfigurationDocumentRequest {
    configuration_id: string;
}

/**
 * @brief Whether the delete succeeded.
 */
export interface DeleteCurveConfigurationDocumentResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Stores a conventions document.
 *
 * The instrument conventions belong to the session's party and replace any
 * it holds under the same id. A world convention the tenant lacks is added;
 * one it holds is left as it is.
 */
export interface SaveConventionsDocumentRequest {
    document: ConventionsDocument;
}

/**
 * @brief What the save did not store, and why.
 */
export interface SaveConventionsDocumentResponse {
    success: boolean;
    message: string;
    /** World conventions the tenant already held, left unchanged. */
    world_kept: string[];
    /** FX conventions, by ORE id, which have no store yet. */
    fx_skipped: string[];
}

/**
 * @brief Reads every convention the party sees: its instrument conventions
 * and the tenant's world conventions.
 */
export interface GetConventionsDocumentRequest {
    /** When the session acts for no party, as a workflow step's does, the party whose rows to read. */
    party_id: string;
}

/**
 * @brief The conventions the party sees.
 */
export interface GetConventionsDocumentResponse {
    success: boolean;
    message: string;
    document: ConventionsDocument;
}

export const subjects = {
    save_curve_configuration_document_request: 'refdata.v1.curve_configuration_documents.put',
    get_curve_configuration_document_request: 'refdata.v1.curve_configuration_documents.get',
    delete_curve_configuration_document_request: 'refdata.v1.curve_configuration_documents.delete',
    save_conventions_document_request: 'refdata.v1.conventions_documents.put',
    get_conventions_document_request: 'refdata.v1.conventions_documents.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    save_curve_configuration_document_request: true,
    get_curve_configuration_document_request: true,
    delete_curve_configuration_document_request: true,
    save_conventions_document_request: true,
    get_conventions_document_request: true,
} as const;
