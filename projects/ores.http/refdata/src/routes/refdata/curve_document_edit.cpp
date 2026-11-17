/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#include "ores.http/routes/refdata/curve_document_edit.hpp"
#include "ores.refdata.api/domain/base_correlation_config_codec.hpp"
#include "ores.refdata.api/domain/bond_future_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/cap_floor_proxy_config_codec.hpp"
#include "ores.refdata.api/domain/cap_floor_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/cds_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/cds_volatility_term_codec.hpp"
#include "ores.refdata.api/domain/commodity_curve_basis_codec.hpp"
#include "ores.refdata.api/domain/commodity_curve_config_codec.hpp"
#include "ores.refdata.api/domain/commodity_curve_cross_currency_codec.hpp"
#include "ores.refdata.api/domain/commodity_curve_direct_codec.hpp"
#include "ores.refdata.api/domain/commodity_curve_interpolation_codec.hpp"
#include "ores.refdata.api/domain/commodity_price_segment_codec.hpp"
#include "ores.refdata.api/domain/commodity_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/curve_bootstrap_config_codec.hpp"
#include "ores.refdata.api/domain/curve_configuration_codec.hpp"
#include "ores.refdata.api/domain/curve_configuration_section_codec.hpp"
#include "ores.refdata.api/domain/curve_correlation_config_codec.hpp"
#include "ores.refdata.api/domain/curve_definition_codec.hpp"
#include "ores.refdata.api/domain/curve_global_report_codec.hpp"
#include "ores.refdata.api/domain/curve_parametric_smile_codec.hpp"
#include "ores.refdata.api/domain/curve_parametric_smile_parameter_codec.hpp"
#include "ores.refdata.api/domain/curve_quote_codec.hpp"
#include "ores.refdata.api/domain/curve_report_configuration_codec.hpp"
#include "ores.refdata.api/domain/curve_security_config_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_convention_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_cross_currency_terms_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_curve_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_discount_ratio_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_extrapolation_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_ibor_fallback_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_pillar_rule_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_projection_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_reference_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_tenor_basis_curves_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_weighted_average_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_constant_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_curve_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_delta_surface_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_kind_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_moneyness_surface_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_proxy_surface_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_smile_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_smile_parameter_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_strike_surface_codec.hpp"
#include "ores.refdata.api/domain/default_curve_benchmark_codec.hpp"
#include "ores.refdata.api/domain/default_curve_cds_terms_codec.hpp"
#include "ores.refdata.api/domain/default_curve_config_codec.hpp"
#include "ores.refdata.api/domain/default_curve_configuration_codec.hpp"
#include "ores.refdata.api/domain/default_curve_market_terms_codec.hpp"
#include "ores.refdata.api/domain/default_curve_transition_matrix_codec.hpp"
#include "ores.refdata.api/domain/default_curve_yield_reinterpretation_codec.hpp"
#include "ores.refdata.api/domain/equity_curve_config_codec.hpp"
#include "ores.refdata.api/domain/equity_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/fx_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/fx_volatility_smile_codec.hpp"
#include "ores.refdata.api/domain/inflation_cap_floor_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/inflation_cap_floor_volatility_strikes_codec.hpp"
#include "ores.refdata.api/domain/inflation_curve_config_codec.hpp"
#include "ores.refdata.api/domain/inflation_seasonality_codec.hpp"
#include "ores.refdata.api/domain/inflation_seasonality_factor_codec.hpp"
#include "ores.refdata.api/domain/intraday_power_curve_config_codec.hpp"
#include "ores.refdata.api/domain/swaption_proxy_config_codec.hpp"
#include "ores.refdata.api/domain/swaption_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/swaption_volatility_smile_codec.hpp"
#include "ores.refdata.api/domain/yield_curve_config_codec.hpp"
#include "ores.refdata.api/domain/yield_volatility_config_codec.hpp"
#include <charconv>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

namespace ores::http::routes::refdata::document {

namespace {

/// Writes the column of the row this key names, and answers whether it found one.
template <typename Row>
bool write_keyed(std::vector<Row>& rows,
                 std::string_view key_column,
                 std::string_view key,
                 std::string_view column,
                 const cell& value) {
    for (auto& row : rows)
        if (find_cell(row, key_column) == cell(std::string(key)))
            return set_cell(row, column, value);
    return false;
}

/// Runs one call on the vector this name carries, in one document.
template <typename Document, typename F>
bool with_vector(Document& document, std::string_view vector, F&& f) {
    if (vector == "sections")
        return f(document.sections);
    if (vector == "definitions")
        return f(document.definitions);
    if (vector == "yield_curves")
        return f(document.yield_curves);
    if (vector == "equity_curves")
        return f(document.equity_curves);
    if (vector == "inflation_curves")
        return f(document.inflation_curves);
    if (vector == "default_curves")
        return f(document.default_curves);
    if (vector == "commodity_curves")
        return f(document.commodity_curves);
    if (vector == "commodity_curve_interpolations")
        return f(document.commodity_curve_interpolations);
    if (vector == "commodity_curve_directs")
        return f(document.commodity_curve_directs);
    if (vector == "commodity_curve_cross_currencies")
        return f(document.commodity_curve_cross_currencies);
    if (vector == "commodity_curve_bases")
        return f(document.commodity_curve_bases);
    if (vector == "fx_volatilities")
        return f(document.fx_volatilities);
    if (vector == "fx_volatility_smiles")
        return f(document.fx_volatility_smiles);
    if (vector == "yield_volatilities")
        return f(document.yield_volatilities);
    if (vector == "base_correlations")
        return f(document.base_correlations);
    if (vector == "correlations")
        return f(document.correlations);
    if (vector == "report_configurations")
        return f(document.report_configurations);
    if (vector == "cds_volatilities")
        return f(document.cds_volatilities);
    if (vector == "cds_volatility_terms")
        return f(document.cds_volatility_terms);
    if (vector == "volatility_configs")
        return f(document.volatility_configs);
    if (vector == "volatility_kinds")
        return f(document.volatility_kinds);
    if (vector == "volatility_constants")
        return f(document.volatility_constants);
    if (vector == "volatility_curves")
        return f(document.volatility_curves);
    if (vector == "volatility_strike_surfaces")
        return f(document.volatility_strike_surfaces);
    if (vector == "volatility_delta_surfaces")
        return f(document.volatility_delta_surfaces);
    if (vector == "volatility_moneyness_surfaces")
        return f(document.volatility_moneyness_surfaces);
    if (vector == "volatility_proxy_surfaces")
        return f(document.volatility_proxy_surfaces);
    if (vector == "volatility_smiles")
        return f(document.volatility_smiles);
    if (vector == "volatility_smile_parameters")
        return f(document.volatility_smile_parameters);
    if (vector == "inflation_cap_floor_volatilities")
        return f(document.inflation_cap_floor_volatilities);
    if (vector == "inflation_volatility_strikes")
        return f(document.inflation_volatility_strikes);
    if (vector == "swaption_volatilities")
        return f(document.swaption_volatilities);
    if (vector == "swaption_proxy_configs")
        return f(document.swaption_proxy_configs);
    if (vector == "swaption_volatility_smiles")
        return f(document.swaption_volatility_smiles);
    if (vector == "cap_floor_volatilities")
        return f(document.cap_floor_volatilities);
    if (vector == "cap_floor_proxy_configs")
        return f(document.cap_floor_proxy_configs);
    if (vector == "parametric_smiles")
        return f(document.parametric_smiles);
    if (vector == "parametric_smile_parameters")
        return f(document.parametric_smile_parameters);
    if (vector == "equity_volatilities")
        return f(document.equity_volatilities);
    if (vector == "commodity_volatilities")
        return f(document.commodity_volatilities);
    if (vector == "bond_future_volatilities")
        return f(document.bond_future_volatilities);
    if (vector == "global_reports")
        return f(document.global_reports);
    if (vector == "commodity_price_segments")
        return f(document.commodity_price_segments);
    if (vector == "default_curve_configurations")
        return f(document.default_curve_configurations);
    if (vector == "default_curve_market_terms")
        return f(document.default_curve_market_terms);
    if (vector == "default_curve_cds_terms")
        return f(document.default_curve_cds_terms);
    if (vector == "default_curve_benchmarks")
        return f(document.default_curve_benchmarks);
    if (vector == "default_curve_transition_matrices")
        return f(document.default_curve_transition_matrices);
    if (vector == "default_curve_yield_reinterpretations")
        return f(document.default_curve_yield_reinterpretations);
    if (vector == "inflation_seasonalities")
        return f(document.inflation_seasonalities);
    if (vector == "seasonality_factors")
        return f(document.seasonality_factors);
    if (vector == "securities")
        return f(document.securities);
    if (vector == "intraday_power_curves")
        return f(document.intraday_power_curves);
    if (vector == "bootstrap_configs")
        return f(document.bootstrap_configs);
    if (vector == "segments")
        return f(document.segments);
    if (vector == "segment_curves")
        return f(document.segment_curves);
    if (vector == "segment_conventions")
        return f(document.segment_conventions);
    if (vector == "segment_pillar_rules")
        return f(document.segment_pillar_rules);
    if (vector == "segment_projections")
        return f(document.segment_projections);
    if (vector == "segment_tenor_basis_curves")
        return f(document.segment_tenor_basis_curves);
    if (vector == "segment_cross_currency_terms")
        return f(document.segment_cross_currency_terms);
    if (vector == "segment_references")
        return f(document.segment_references);
    if (vector == "segment_weighted_averages")
        return f(document.segment_weighted_averages);
    if (vector == "segment_discount_ratios")
        return f(document.segment_discount_ratios);
    if (vector == "segment_extrapolations")
        return f(document.segment_extrapolations);
    if (vector == "segment_ibor_fallbacks")
        return f(document.segment_ibor_fallbacks);
    if (vector == "quotes")
        return f(document.quotes);
    return false;
}

}

bool write_column(ores::refdata::messaging::curve_configuration_document& document,
                  std::string_view vector,
                  std::string_view key_column,
                  std::string_view key,
                  std::string_view column,
                  const cell& value) {
    return with_vector(document, vector, [&](auto& rows) {
        return write_keyed(rows, key_column, key, column, value);
    });
}

bool copy_vector(const ores::refdata::messaging::curve_configuration_document& from,
                 std::string_view vector,
                 ores::refdata::messaging::curve_configuration_document& into) {
    if (vector == "sections") {
        into.sections = from.sections;
        return true;
    }
    if (vector == "definitions") {
        into.definitions = from.definitions;
        return true;
    }
    if (vector == "yield_curves") {
        into.yield_curves = from.yield_curves;
        return true;
    }
    if (vector == "equity_curves") {
        into.equity_curves = from.equity_curves;
        return true;
    }
    if (vector == "inflation_curves") {
        into.inflation_curves = from.inflation_curves;
        return true;
    }
    if (vector == "default_curves") {
        into.default_curves = from.default_curves;
        return true;
    }
    if (vector == "commodity_curves") {
        into.commodity_curves = from.commodity_curves;
        return true;
    }
    if (vector == "commodity_curve_interpolations") {
        into.commodity_curve_interpolations = from.commodity_curve_interpolations;
        return true;
    }
    if (vector == "commodity_curve_directs") {
        into.commodity_curve_directs = from.commodity_curve_directs;
        return true;
    }
    if (vector == "commodity_curve_cross_currencies") {
        into.commodity_curve_cross_currencies = from.commodity_curve_cross_currencies;
        return true;
    }
    if (vector == "commodity_curve_bases") {
        into.commodity_curve_bases = from.commodity_curve_bases;
        return true;
    }
    if (vector == "fx_volatilities") {
        into.fx_volatilities = from.fx_volatilities;
        return true;
    }
    if (vector == "fx_volatility_smiles") {
        into.fx_volatility_smiles = from.fx_volatility_smiles;
        return true;
    }
    if (vector == "yield_volatilities") {
        into.yield_volatilities = from.yield_volatilities;
        return true;
    }
    if (vector == "base_correlations") {
        into.base_correlations = from.base_correlations;
        return true;
    }
    if (vector == "correlations") {
        into.correlations = from.correlations;
        return true;
    }
    if (vector == "report_configurations") {
        into.report_configurations = from.report_configurations;
        return true;
    }
    if (vector == "cds_volatilities") {
        into.cds_volatilities = from.cds_volatilities;
        return true;
    }
    if (vector == "cds_volatility_terms") {
        into.cds_volatility_terms = from.cds_volatility_terms;
        return true;
    }
    if (vector == "volatility_configs") {
        into.volatility_configs = from.volatility_configs;
        return true;
    }
    if (vector == "volatility_kinds") {
        into.volatility_kinds = from.volatility_kinds;
        return true;
    }
    if (vector == "volatility_constants") {
        into.volatility_constants = from.volatility_constants;
        return true;
    }
    if (vector == "volatility_curves") {
        into.volatility_curves = from.volatility_curves;
        return true;
    }
    if (vector == "volatility_strike_surfaces") {
        into.volatility_strike_surfaces = from.volatility_strike_surfaces;
        return true;
    }
    if (vector == "volatility_delta_surfaces") {
        into.volatility_delta_surfaces = from.volatility_delta_surfaces;
        return true;
    }
    if (vector == "volatility_moneyness_surfaces") {
        into.volatility_moneyness_surfaces = from.volatility_moneyness_surfaces;
        return true;
    }
    if (vector == "volatility_proxy_surfaces") {
        into.volatility_proxy_surfaces = from.volatility_proxy_surfaces;
        return true;
    }
    if (vector == "volatility_smiles") {
        into.volatility_smiles = from.volatility_smiles;
        return true;
    }
    if (vector == "volatility_smile_parameters") {
        into.volatility_smile_parameters = from.volatility_smile_parameters;
        return true;
    }
    if (vector == "inflation_cap_floor_volatilities") {
        into.inflation_cap_floor_volatilities = from.inflation_cap_floor_volatilities;
        return true;
    }
    if (vector == "inflation_volatility_strikes") {
        into.inflation_volatility_strikes = from.inflation_volatility_strikes;
        return true;
    }
    if (vector == "swaption_volatilities") {
        into.swaption_volatilities = from.swaption_volatilities;
        return true;
    }
    if (vector == "swaption_proxy_configs") {
        into.swaption_proxy_configs = from.swaption_proxy_configs;
        return true;
    }
    if (vector == "swaption_volatility_smiles") {
        into.swaption_volatility_smiles = from.swaption_volatility_smiles;
        return true;
    }
    if (vector == "cap_floor_volatilities") {
        into.cap_floor_volatilities = from.cap_floor_volatilities;
        return true;
    }
    if (vector == "cap_floor_proxy_configs") {
        into.cap_floor_proxy_configs = from.cap_floor_proxy_configs;
        return true;
    }
    if (vector == "parametric_smiles") {
        into.parametric_smiles = from.parametric_smiles;
        return true;
    }
    if (vector == "parametric_smile_parameters") {
        into.parametric_smile_parameters = from.parametric_smile_parameters;
        return true;
    }
    if (vector == "equity_volatilities") {
        into.equity_volatilities = from.equity_volatilities;
        return true;
    }
    if (vector == "commodity_volatilities") {
        into.commodity_volatilities = from.commodity_volatilities;
        return true;
    }
    if (vector == "bond_future_volatilities") {
        into.bond_future_volatilities = from.bond_future_volatilities;
        return true;
    }
    if (vector == "global_reports") {
        into.global_reports = from.global_reports;
        return true;
    }
    if (vector == "commodity_price_segments") {
        into.commodity_price_segments = from.commodity_price_segments;
        return true;
    }
    if (vector == "default_curve_configurations") {
        into.default_curve_configurations = from.default_curve_configurations;
        return true;
    }
    if (vector == "default_curve_market_terms") {
        into.default_curve_market_terms = from.default_curve_market_terms;
        return true;
    }
    if (vector == "default_curve_cds_terms") {
        into.default_curve_cds_terms = from.default_curve_cds_terms;
        return true;
    }
    if (vector == "default_curve_benchmarks") {
        into.default_curve_benchmarks = from.default_curve_benchmarks;
        return true;
    }
    if (vector == "default_curve_transition_matrices") {
        into.default_curve_transition_matrices = from.default_curve_transition_matrices;
        return true;
    }
    if (vector == "default_curve_yield_reinterpretations") {
        into.default_curve_yield_reinterpretations = from.default_curve_yield_reinterpretations;
        return true;
    }
    if (vector == "inflation_seasonalities") {
        into.inflation_seasonalities = from.inflation_seasonalities;
        return true;
    }
    if (vector == "seasonality_factors") {
        into.seasonality_factors = from.seasonality_factors;
        return true;
    }
    if (vector == "securities") {
        into.securities = from.securities;
        return true;
    }
    if (vector == "intraday_power_curves") {
        into.intraday_power_curves = from.intraday_power_curves;
        return true;
    }
    if (vector == "bootstrap_configs") {
        into.bootstrap_configs = from.bootstrap_configs;
        return true;
    }
    if (vector == "segments") {
        into.segments = from.segments;
        return true;
    }
    if (vector == "segment_curves") {
        into.segment_curves = from.segment_curves;
        return true;
    }
    if (vector == "segment_conventions") {
        into.segment_conventions = from.segment_conventions;
        return true;
    }
    if (vector == "segment_pillar_rules") {
        into.segment_pillar_rules = from.segment_pillar_rules;
        return true;
    }
    if (vector == "segment_projections") {
        into.segment_projections = from.segment_projections;
        return true;
    }
    if (vector == "segment_tenor_basis_curves") {
        into.segment_tenor_basis_curves = from.segment_tenor_basis_curves;
        return true;
    }
    if (vector == "segment_cross_currency_terms") {
        into.segment_cross_currency_terms = from.segment_cross_currency_terms;
        return true;
    }
    if (vector == "segment_references") {
        into.segment_references = from.segment_references;
        return true;
    }
    if (vector == "segment_weighted_averages") {
        into.segment_weighted_averages = from.segment_weighted_averages;
        return true;
    }
    if (vector == "segment_discount_ratios") {
        into.segment_discount_ratios = from.segment_discount_ratios;
        return true;
    }
    if (vector == "segment_extrapolations") {
        into.segment_extrapolations = from.segment_extrapolations;
        return true;
    }
    if (vector == "segment_ibor_fallbacks") {
        into.segment_ibor_fallbacks = from.segment_ibor_fallbacks;
        return true;
    }
    if (vector == "quotes") {
        into.quotes = from.quotes;
        return true;
    }
    return false;
}

}
