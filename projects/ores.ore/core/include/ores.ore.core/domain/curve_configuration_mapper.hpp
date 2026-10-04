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
#ifndef ORES_ORE_CORE_DOMAIN_CURVE_CONFIGURATION_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_CURVE_CONFIGURATION_MAPPER_HPP

#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
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
#include <vector>

namespace ores::ore::domain {

/**
 * @brief One ORE CurveConfiguration document, mapped to the refdata entities.
 *
 * The document is a header and the section elements it writes; each entry of a
 * section is a curve definition, with its section's settings on a detail row
 * and its lists on child rows.
 */
struct mapped_curve_configuration {
    refdata::domain::curve_configuration config;
    std::vector<refdata::domain::curve_configuration_section> sections;
    std::vector<refdata::domain::curve_definition> definitions;
    std::vector<refdata::domain::yield_curve_config> yield_curves;
    std::vector<refdata::domain::equity_curve_config> equity_curves;
    std::vector<refdata::domain::inflation_curve_config> inflation_curves;
    std::vector<refdata::domain::default_curve_config> default_curves;
    std::vector<refdata::domain::commodity_curve_config> commodity_curves;
    std::vector<refdata::domain::fx_volatility_config> fx_volatilities;
    std::vector<refdata::domain::yield_volatility_config> yield_volatilities;
    std::vector<refdata::domain::base_correlation_config> base_correlations;
    std::vector<refdata::domain::curve_correlation_config> correlations;
    std::vector<refdata::domain::curve_report_configuration> report_configurations;
    std::vector<refdata::domain::cds_volatility_config> cds_volatilities;
    std::vector<refdata::domain::cds_volatility_term> cds_volatility_terms;
    std::vector<refdata::domain::curve_volatility_config> volatility_configs;
    std::vector<refdata::domain::inflation_cap_floor_volatility_config>
        inflation_cap_floor_volatilities;
    std::vector<refdata::domain::swaption_volatility_config> swaption_volatilities;
    std::vector<refdata::domain::cap_floor_volatility_config> cap_floor_volatilities;
    std::vector<refdata::domain::curve_parametric_smile> parametric_smiles;
    std::vector<refdata::domain::curve_parametric_smile_parameter> parametric_smile_parameters;
    std::vector<refdata::domain::equity_volatility_config> equity_volatilities;
    std::vector<refdata::domain::commodity_volatility_config> commodity_volatilities;
    std::vector<refdata::domain::bond_future_volatility_config> bond_future_volatilities;
    std::vector<refdata::domain::curve_global_report> global_reports;
    std::vector<refdata::domain::commodity_price_segment> commodity_price_segments;
    std::vector<refdata::domain::default_curve_configuration> default_curve_configurations;
    std::vector<refdata::domain::inflation_seasonality_factor> seasonality_factors;
    std::vector<refdata::domain::curve_security_config> securities;
    std::vector<refdata::domain::intraday_power_curve_config> intraday_power_curves;
    std::vector<refdata::domain::curve_bootstrap_config> bootstrap_configs;
    std::vector<refdata::domain::curve_segment> segments;
    std::vector<refdata::domain::curve_segment_curve> segment_curves;
    std::vector<refdata::domain::curve_quote> quotes;
};

/**
 * @brief Maps between an ORE CurveConfiguration document and the refdata curve
 * entities.
 *
 * Every curve section and the document's report configuration are mapped.
 * Elements of an entry the mapper cannot hold yet are refused rather than
 * dropped, because a dropped element would pass as a round trip while losing
 * data.
 *
 * Segments are restored in the order the document wrote them within each
 * segment element. The binding keeps one list per element, so the order across
 * elements is not part of the parsed document.
 */
class ORES_ORE_CORE_EXPORT curve_configuration_mapper {
public:
    /**
     * @brief Maps an ORE CurveConfiguration document to the refdata entities.
     *
     * @throws std::runtime_error for a report configuration or a section with
     * entries the mapper does not model yet.
     */
    static mapped_curve_configuration map(const curveconfiguration& v);

    /**
     * @brief Reconstructs an ORE CurveConfiguration document from mapped
     * entities.
     *
     * @throws std::runtime_error for a row the document has no place for: an
     * unknown section or segment type, a detail or child row whose parent is
     * absent, or a quote directly on an entry whose section holds quotes only on
     * its segments.
     */
    static curveconfiguration reverse(const mapped_curve_configuration& v);
};

}

#endif
