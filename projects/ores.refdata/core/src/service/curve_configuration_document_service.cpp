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
#include "ores.refdata.core/service/curve_configuration_document_service.hpp"
#include "ores.database/repository/document_operations.hpp"
#include "ores.refdata.core/repository/base_correlation_config_repository.hpp"
#include "ores.refdata.core/repository/bond_future_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/cap_floor_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/cds_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/cds_volatility_term_repository.hpp"
#include "ores.refdata.core/repository/commodity_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/repository/curve_configuration_repository.hpp"
#include "ores.refdata.core/repository/curve_configuration_section_repository.hpp"
#include "ores.refdata.core/repository/curve_correlation_config_repository.hpp"
#include "ores.refdata.core/repository/curve_definition_repository.hpp"
#include "ores.refdata.core/repository/curve_global_report_repository.hpp"
#include "ores.refdata.core/repository/curve_parametric_smile_parameter_repository.hpp"
#include "ores.refdata.core/repository/curve_parametric_smile_repository.hpp"
#include "ores.refdata.core/repository/curve_quote_repository.hpp"
#include "ores.refdata.core/repository/curve_report_configuration_repository.hpp"
#include "ores.refdata.core/repository/curve_security_config_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_curve_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_repository.hpp"
#include "ores.refdata.core/repository/curve_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/default_curve_config_repository.hpp"
#include "ores.refdata.core/repository/default_curve_configuration_repository.hpp"
#include "ores.refdata.core/repository/equity_curve_config_repository.hpp"
#include "ores.refdata.core/repository/equity_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/fx_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/inflation_cap_floor_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/inflation_curve_config_repository.hpp"
#include "ores.refdata.core/repository/inflation_seasonality_factor_repository.hpp"
#include "ores.refdata.core/repository/intraday_power_curve_config_repository.hpp"
#include "ores.refdata.core/repository/swaption_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/yield_curve_config_repository.hpp"
#include <set>

namespace ores::refdata::service {

using namespace ores::refdata::repository;
using ores::database::repository::read_one;
using ores::database::repository::read_where;
using ores::database::repository::stamp_party;

curve_configuration_document_service::curve_configuration_document_service(context ctx)
    : ctx_(std::move(ctx)) {}

void curve_configuration_document_service::save(domain::curve_configuration_document v) {
    stamp_party(ctx_, v);
    curve_configuration_repository().write(ctx_, v.config);

    curve_configuration_section_repository().write(ctx_, v.sections);
    curve_global_report_repository().write(ctx_, v.global_reports);
    curve_definition_repository().write(ctx_, v.definitions);
    yield_curve_config_repository().write(ctx_, v.yield_curves);
    equity_curve_config_repository().write(ctx_, v.equity_curves);
    default_curve_config_repository().write(ctx_, v.default_curves);
    default_curve_configuration_repository().write(ctx_, v.default_curve_configurations);
    inflation_curve_config_repository().write(ctx_, v.inflation_curves);
    inflation_seasonality_factor_repository().write(ctx_, v.seasonality_factors);
    curve_security_config_repository().write(ctx_, v.securities);
    intraday_power_curve_config_repository().write(ctx_, v.intraday_power_curves);
    fx_volatility_config_repository().write(ctx_, v.fx_volatilities);
    base_correlation_config_repository().write(ctx_, v.base_correlations);
    curve_correlation_config_repository().write(ctx_, v.correlations);
    curve_report_configuration_repository().write(ctx_, v.report_configurations);
    cds_volatility_config_repository().write(ctx_, v.cds_volatilities);
    cds_volatility_term_repository().write(ctx_, v.cds_volatility_terms);
    curve_volatility_config_repository().write(ctx_, v.volatility_configs);
    inflation_cap_floor_volatility_config_repository().write(ctx_,
                                                             v.inflation_cap_floor_volatilities);
    swaption_volatility_config_repository().write(ctx_, v.swaption_volatilities);
    cap_floor_volatility_config_repository().write(ctx_, v.cap_floor_volatilities);
    curve_parametric_smile_repository().write(ctx_, v.parametric_smiles);
    curve_parametric_smile_parameter_repository().write(ctx_, v.parametric_smile_parameters);
    equity_volatility_config_repository().write(ctx_, v.equity_volatilities);
    commodity_volatility_config_repository().write(ctx_, v.commodity_volatilities);
    bond_future_volatility_config_repository().write(ctx_, v.bond_future_volatilities);
    curve_bootstrap_config_repository().write(ctx_, v.bootstrap_configs);
    curve_segment_repository().write(ctx_, v.segments);
    curve_segment_curve_repository().write(ctx_, v.segment_curves);
    curve_quote_repository().write(ctx_, v.quotes);
}

domain::curve_configuration_document
curve_configuration_document_service::get(const boost::uuids::uuid& config_id) {
    domain::curve_configuration_document r;
    r.config = read_one(ctx_, curve_configuration_repository(), "curve configuration", config_id);
    const auto of_config = [&](const auto& row) {
        return row.curve_configuration_id == config_id;
    };
    r.sections = read_where(ctx_, curve_configuration_section_repository(), of_config);
    r.global_reports = read_where(ctx_, curve_global_report_repository(), of_config);
    r.definitions = read_where(ctx_, curve_definition_repository(), of_config);
    std::set<boost::uuids::uuid> definitions;
    for (const auto& d : r.definitions)
        definitions.insert(d.id);
    const auto of_definition = [&](const auto& row) {
        return definitions.contains(row.curve_definition_id);
    };
    r.yield_curves = read_where(ctx_, yield_curve_config_repository(), of_definition);
    r.equity_curves = read_where(ctx_, equity_curve_config_repository(), of_definition);
    r.default_curves = read_where(ctx_, default_curve_config_repository(), of_definition);
    r.default_curve_configurations =
        read_where(ctx_, default_curve_configuration_repository(), of_definition);
    r.inflation_curves = read_where(ctx_, inflation_curve_config_repository(), of_definition);
    r.seasonality_factors =
        read_where(ctx_, inflation_seasonality_factor_repository(), of_definition);
    r.securities = read_where(ctx_, curve_security_config_repository(), of_definition);
    r.intraday_power_curves =
        read_where(ctx_, intraday_power_curve_config_repository(), of_definition);
    r.fx_volatilities = read_where(ctx_, fx_volatility_config_repository(), of_definition);
    r.base_correlations = read_where(ctx_, base_correlation_config_repository(), of_definition);
    r.correlations = read_where(ctx_, curve_correlation_config_repository(), of_definition);
    r.report_configurations =
        read_where(ctx_, curve_report_configuration_repository(), of_definition);
    r.cds_volatilities = read_where(ctx_, cds_volatility_config_repository(), of_definition);
    r.cds_volatility_terms = read_where(ctx_, cds_volatility_term_repository(), of_definition);
    r.volatility_configs = read_where(ctx_, curve_volatility_config_repository(), of_definition);
    r.inflation_cap_floor_volatilities =
        read_where(ctx_, inflation_cap_floor_volatility_config_repository(), of_definition);
    r.swaption_volatilities =
        read_where(ctx_, swaption_volatility_config_repository(), of_definition);
    r.cap_floor_volatilities =
        read_where(ctx_, cap_floor_volatility_config_repository(), of_definition);
    r.parametric_smiles = read_where(ctx_, curve_parametric_smile_repository(), of_definition);
    r.parametric_smile_parameters =
        read_where(ctx_, curve_parametric_smile_parameter_repository(), of_definition);
    r.equity_volatilities = read_where(ctx_, equity_volatility_config_repository(), of_definition);
    r.commodity_volatilities =
        read_where(ctx_, commodity_volatility_config_repository(), of_definition);
    r.bond_future_volatilities =
        read_where(ctx_, bond_future_volatility_config_repository(), of_definition);
    r.bootstrap_configs = read_where(ctx_, curve_bootstrap_config_repository(), of_definition);
    r.segments = read_where(ctx_, curve_segment_repository(), of_definition);
    r.quotes = read_where(ctx_, curve_quote_repository(), of_definition);
    std::set<boost::uuids::uuid> segments;
    for (const auto& s : r.segments)
        segments.insert(s.id);
    r.segment_curves = read_where(ctx_, curve_segment_curve_repository(), [&](const auto& row) {
        return segments.contains(row.curve_segment_id);
    });
    return r;
}

std::optional<boost::uuids::uuid> curve_configuration_document_service::find_by_configuration(
    const boost::uuids::uuid& configuration_id) {
    for (const auto& h : curve_configuration_repository().read_latest(ctx_))
        if (h.configuration_id == configuration_id)
            return h.id;
    return std::nullopt;
}

}
