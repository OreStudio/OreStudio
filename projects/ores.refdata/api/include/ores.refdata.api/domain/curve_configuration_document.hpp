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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_CONFIGURATION_DOCUMENT_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_CONFIGURATION_DOCUMENT_HPP

#include "ores.refdata.api/domain/base_correlation_config.hpp"
#include "ores.refdata.api/domain/bond_future_volatility_config.hpp"
#include "ores.refdata.api/domain/cap_floor_volatility_config.hpp"
#include "ores.refdata.api/domain/cds_volatility_config.hpp"
#include "ores.refdata.api/domain/cds_volatility_term.hpp"
#include "ores.refdata.api/domain/commodity_curve_config.hpp"
#include "ores.refdata.api/domain/commodity_price_segment.hpp"
#include "ores.refdata.api/domain/commodity_volatility_config.hpp"
#include "ores.refdata.api/domain/curve_bootstrap_config.hpp"
#include "ores.refdata.api/domain/curve_configuration.hpp"
#include "ores.refdata.api/domain/curve_configuration_section.hpp"
#include "ores.refdata.api/domain/curve_correlation_config.hpp"
#include "ores.refdata.api/domain/curve_definition.hpp"
#include "ores.refdata.api/domain/curve_global_report.hpp"
#include "ores.refdata.api/domain/curve_parametric_smile.hpp"
#include "ores.refdata.api/domain/curve_parametric_smile_parameter.hpp"
#include "ores.refdata.api/domain/curve_quote.hpp"
#include "ores.refdata.api/domain/curve_report_configuration.hpp"
#include "ores.refdata.api/domain/curve_security_config.hpp"
#include "ores.refdata.api/domain/curve_segment.hpp"
#include "ores.refdata.api/domain/curve_segment_curve.hpp"
#include "ores.refdata.api/domain/curve_volatility_config.hpp"
#include "ores.refdata.api/domain/default_curve_config.hpp"
#include "ores.refdata.api/domain/default_curve_configuration.hpp"
#include "ores.refdata.api/domain/equity_curve_config.hpp"
#include "ores.refdata.api/domain/equity_volatility_config.hpp"
#include "ores.refdata.api/domain/fx_volatility_config.hpp"
#include "ores.refdata.api/domain/inflation_cap_floor_volatility_config.hpp"
#include "ores.refdata.api/domain/inflation_curve_config.hpp"
#include "ores.refdata.api/domain/inflation_seasonality_factor.hpp"
#include "ores.refdata.api/domain/intraday_power_curve_config.hpp"
#include "ores.refdata.api/domain/swaption_volatility_config.hpp"
#include "ores.refdata.api/domain/yield_curve_config.hpp"
#include "ores.refdata.api/domain/yield_volatility_config.hpp"
#include <string>
#include <vector>

namespace ores::refdata::domain {

/**
 * @brief One ORE curve configuration document as the rows refdata stores.
 *
 * The header row and every child row the document maps to, grouped by table.
 * Refdata stores and reads the document whole; a caller in another component
 * reaches it through refdata's operations, never its tables.
 */
struct curve_configuration_document {
    curve_configuration config;
    std::vector<curve_configuration_section> sections;
    std::vector<curve_definition> definitions;
    std::vector<yield_curve_config> yield_curves;
    std::vector<equity_curve_config> equity_curves;
    std::vector<inflation_curve_config> inflation_curves;
    std::vector<default_curve_config> default_curves;
    std::vector<commodity_curve_config> commodity_curves;
    std::vector<fx_volatility_config> fx_volatilities;
    std::vector<yield_volatility_config> yield_volatilities;
    std::vector<base_correlation_config> base_correlations;
    std::vector<curve_correlation_config> correlations;
    std::vector<curve_report_configuration> report_configurations;
    std::vector<cds_volatility_config> cds_volatilities;
    std::vector<cds_volatility_term> cds_volatility_terms;
    std::vector<curve_volatility_config> volatility_configs;
    std::vector<inflation_cap_floor_volatility_config> inflation_cap_floor_volatilities;
    std::vector<swaption_volatility_config> swaption_volatilities;
    std::vector<cap_floor_volatility_config> cap_floor_volatilities;
    std::vector<curve_parametric_smile> parametric_smiles;
    std::vector<curve_parametric_smile_parameter> parametric_smile_parameters;
    std::vector<equity_volatility_config> equity_volatilities;
    std::vector<commodity_volatility_config> commodity_volatilities;
    std::vector<bond_future_volatility_config> bond_future_volatilities;
    std::vector<curve_global_report> global_reports;
    std::vector<commodity_price_segment> commodity_price_segments;
    std::vector<default_curve_configuration> default_curve_configurations;
    std::vector<inflation_seasonality_factor> seasonality_factors;
    std::vector<curve_security_config> securities;
    std::vector<intraday_power_curve_config> intraday_power_curves;
    std::vector<curve_bootstrap_config> bootstrap_configs;
    std::vector<curve_segment> segments;
    std::vector<curve_segment_curve> segment_curves;
    std::vector<curve_quote> quotes;

    friend bool operator==(const curve_configuration_document&,
                           const curve_configuration_document&) = default;
};

}

#endif
