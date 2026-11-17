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
#include "ores.http/routes/refdata/curve_document_cells.hpp"
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
#include <type_traits>
#include <utility>

namespace ores::http::routes::refdata::document {

namespace {

/// One vector of the document, as rows under the name the document gives it.
template <typename T>
void flatten(std::map<std::string, std::vector<row>, std::less<>>& into,
             std::string_view vector,
             const std::vector<T>& source) {
    auto& rows = into[std::string(vector)];
    rows.reserve(source.size());
    for (const auto& entry : source)
        rows.push_back(row_of(entry));
}

}

cells::cells(const ores::refdata::messaging::curve_configuration_document& source)
    : header_(row_of(source.config)) {
    flatten(rows_, "sections", source.sections);
    flatten(rows_, "definitions", source.definitions);
    flatten(rows_, "yield_curves", source.yield_curves);
    flatten(rows_, "equity_curves", source.equity_curves);
    flatten(rows_, "inflation_curves", source.inflation_curves);
    flatten(rows_, "default_curves", source.default_curves);
    flatten(rows_, "commodity_curves", source.commodity_curves);
    flatten(rows_, "commodity_curve_interpolations", source.commodity_curve_interpolations);
    flatten(rows_, "commodity_curve_directs", source.commodity_curve_directs);
    flatten(rows_, "commodity_curve_cross_currencies", source.commodity_curve_cross_currencies);
    flatten(rows_, "commodity_curve_bases", source.commodity_curve_bases);
    flatten(rows_, "fx_volatilities", source.fx_volatilities);
    flatten(rows_, "fx_volatility_smiles", source.fx_volatility_smiles);
    flatten(rows_, "yield_volatilities", source.yield_volatilities);
    flatten(rows_, "base_correlations", source.base_correlations);
    flatten(rows_, "correlations", source.correlations);
    flatten(rows_, "report_configurations", source.report_configurations);
    flatten(rows_, "cds_volatilities", source.cds_volatilities);
    flatten(rows_, "cds_volatility_terms", source.cds_volatility_terms);
    flatten(rows_, "volatility_configs", source.volatility_configs);
    flatten(rows_, "volatility_kinds", source.volatility_kinds);
    flatten(rows_, "volatility_constants", source.volatility_constants);
    flatten(rows_, "volatility_curves", source.volatility_curves);
    flatten(rows_, "volatility_strike_surfaces", source.volatility_strike_surfaces);
    flatten(rows_, "volatility_delta_surfaces", source.volatility_delta_surfaces);
    flatten(rows_, "volatility_moneyness_surfaces", source.volatility_moneyness_surfaces);
    flatten(rows_, "volatility_proxy_surfaces", source.volatility_proxy_surfaces);
    flatten(rows_, "volatility_smiles", source.volatility_smiles);
    flatten(rows_, "volatility_smile_parameters", source.volatility_smile_parameters);
    flatten(rows_, "inflation_cap_floor_volatilities", source.inflation_cap_floor_volatilities);
    flatten(rows_, "inflation_volatility_strikes", source.inflation_volatility_strikes);
    flatten(rows_, "swaption_volatilities", source.swaption_volatilities);
    flatten(rows_, "swaption_proxy_configs", source.swaption_proxy_configs);
    flatten(rows_, "swaption_volatility_smiles", source.swaption_volatility_smiles);
    flatten(rows_, "cap_floor_volatilities", source.cap_floor_volatilities);
    flatten(rows_, "cap_floor_proxy_configs", source.cap_floor_proxy_configs);
    flatten(rows_, "parametric_smiles", source.parametric_smiles);
    flatten(rows_, "parametric_smile_parameters", source.parametric_smile_parameters);
    flatten(rows_, "equity_volatilities", source.equity_volatilities);
    flatten(rows_, "commodity_volatilities", source.commodity_volatilities);
    flatten(rows_, "bond_future_volatilities", source.bond_future_volatilities);
    flatten(rows_, "global_reports", source.global_reports);
    flatten(rows_, "commodity_price_segments", source.commodity_price_segments);
    flatten(rows_, "default_curve_configurations", source.default_curve_configurations);
    flatten(rows_, "default_curve_market_terms", source.default_curve_market_terms);
    flatten(rows_, "default_curve_cds_terms", source.default_curve_cds_terms);
    flatten(rows_, "default_curve_benchmarks", source.default_curve_benchmarks);
    flatten(rows_, "default_curve_transition_matrices", source.default_curve_transition_matrices);
    flatten(rows_,
            "default_curve_yield_reinterpretations",
            source.default_curve_yield_reinterpretations);
    flatten(rows_, "inflation_seasonalities", source.inflation_seasonalities);
    flatten(rows_, "seasonality_factors", source.seasonality_factors);
    flatten(rows_, "securities", source.securities);
    flatten(rows_, "intraday_power_curves", source.intraday_power_curves);
    flatten(rows_, "bootstrap_configs", source.bootstrap_configs);
    flatten(rows_, "segments", source.segments);
    flatten(rows_, "segment_curves", source.segment_curves);
    flatten(rows_, "segment_conventions", source.segment_conventions);
    flatten(rows_, "segment_pillar_rules", source.segment_pillar_rules);
    flatten(rows_, "segment_projections", source.segment_projections);
    flatten(rows_, "segment_tenor_basis_curves", source.segment_tenor_basis_curves);
    flatten(rows_, "segment_cross_currency_terms", source.segment_cross_currency_terms);
    flatten(rows_, "segment_references", source.segment_references);
    flatten(rows_, "segment_weighted_averages", source.segment_weighted_averages);
    flatten(rows_, "segment_discount_ratios", source.segment_discount_ratios);
    flatten(rows_, "segment_extrapolations", source.segment_extrapolations);
    flatten(rows_, "segment_ibor_fallbacks", source.segment_ibor_fallbacks);
    flatten(rows_, "quotes", source.quotes);
}

const std::vector<row>* cells::find(std::string_view vector) const {
    const auto found = rows_.find(vector);
    return found == rows_.end() ? nullptr : &found->second;
}

const std::vector<row>& cells::at(std::string_view vector) const {
    static const std::vector<row> none;
    const auto rows = find(vector);
    return rows == nullptr ? none : *rows;
}


}
