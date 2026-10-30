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
import type { BaseCorrelationConfig } from '../domain/base_correlation_config.js';
import type { BondFutureVolatilityConfig } from '../domain/bond_future_volatility_config.js';
import type { CapFloorProxyConfig } from '../domain/cap_floor_proxy_config.js';
import type { CapFloorVolatilityConfig } from '../domain/cap_floor_volatility_config.js';
import type { CdsVolatilityConfig } from '../domain/cds_volatility_config.js';
import type { CdsVolatilityTerm } from '../domain/cds_volatility_term.js';
import type { CommodityCurveConfig } from '../domain/commodity_curve_config.js';
import type { CommodityPriceSegment } from '../domain/commodity_price_segment.js';
import type { CommodityVolatilityConfig } from '../domain/commodity_volatility_config.js';
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
import type { CurveVolatilityConstant } from '../domain/curve_volatility_constant.js';
import type { CurveVolatilityCurve } from '../domain/curve_volatility_curve.js';
import type { CurveVolatilityDeltaSurface } from '../domain/curve_volatility_delta_surface.js';
import type { CurveVolatilityKind } from '../domain/curve_volatility_kind.js';
import type { CurveVolatilityProxySurface } from '../domain/curve_volatility_proxy_surface.js';
import type { CurveVolatilityStrikeSurface } from '../domain/curve_volatility_strike_surface.js';
import type { DefaultCurveConfig } from '../domain/default_curve_config.js';
import type { DefaultCurveConfiguration } from '../domain/default_curve_configuration.js';
import type { EquityCurveConfig } from '../domain/equity_curve_config.js';
import type { EquityVolatilityConfig } from '../domain/equity_volatility_config.js';
import type { FxVolatilityConfig } from '../domain/fx_volatility_config.js';
import type { FxVolatilitySmile } from '../domain/fx_volatility_smile.js';
import type { InflationCapFloorVolatilityConfig } from '../domain/inflation_cap_floor_volatility_config.js';
import type { InflationCurveConfig } from '../domain/inflation_curve_config.js';
import type { InflationSeasonalityFactor } from '../domain/inflation_seasonality_factor.js';
import type { IntradayPowerCurveConfig } from '../domain/intraday_power_curve_config.js';
import type { SwaptionProxyConfig } from '../domain/swaption_proxy_config.js';
import type { SwaptionVolatilityConfig } from '../domain/swaption_volatility_config.js';
import type { YieldCurveConfig } from '../domain/yield_curve_config.js';
import type { YieldVolatilityConfig } from '../domain/yield_volatility_config.js';

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
    fx_volatility_smiles: FxVolatilitySmile[];
    yield_volatilities: YieldVolatilityConfig[];
    base_correlations: BaseCorrelationConfig[];
    correlations: CurveCorrelationConfig[];
    report_configurations: CurveReportConfiguration[];
    cds_volatilities: CdsVolatilityConfig[];
    cds_volatility_terms: CdsVolatilityTerm[];
    volatility_configs: CurveVolatilityConfig[];
    volatility_kinds: CurveVolatilityKind[];
    volatility_constants: CurveVolatilityConstant[];
    volatility_curves: CurveVolatilityCurve[];
    volatility_strike_surfaces: CurveVolatilityStrikeSurface[];
    volatility_delta_surfaces: CurveVolatilityDeltaSurface[];
    volatility_proxy_surfaces: CurveVolatilityProxySurface[];
    inflation_cap_floor_volatilities: InflationCapFloorVolatilityConfig[];
    swaption_volatilities: SwaptionVolatilityConfig[];
    swaption_proxy_configs: SwaptionProxyConfig[];
    cap_floor_volatilities: CapFloorVolatilityConfig[];
    cap_floor_proxy_configs: CapFloorProxyConfig[];
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
    /**
     * @brief The reason every row this save writes carries.
     *
     * An amendment states why it was made, and the rows of the new version take
     * that reason rather than the one the document was imported with. Empty
     * leaves the store's own reason, which is the create case.
     */
    reason_code: string;
    /** The words that go with the reason, empty when the reason needs none. */
    commentary: string;
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

export const subjects = {
    save_curve_configuration_document_request: 'refdata.v1.curve_configuration_documents.put',
    get_curve_configuration_document_request: 'refdata.v1.curve_configuration_documents.get',
    delete_curve_configuration_document_request: 'refdata.v1.curve_configuration_documents.delete',
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
} as const;
