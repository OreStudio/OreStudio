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
#ifndef ORES_REPORTING_CORE_SERVICE_RUN_DOCUMENT_GENERATOR_HPP
#define ORES_REPORTING_CORE_SERVICE_RUN_DOCUMENT_GENERATOR_HPP

#include "ores.reporting.api/domain/risk_report_config.hpp"
#include "ores.reporting.api/messaging/run_document_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief What a run document is built from.
 *
 * Every field comes from the definition, its risk configuration or its
 * bindings. The as-of date is the one value that changes from run to run.
 */
struct run_document_inputs {
    boost::uuids::uuid report_definition_id;
    boost::uuids::uuid party_id;

    /**
     * @brief The date the run values the portfolio at, as ORE writes it.
     */
    std::string asof_date;

    /**
     * @brief The definition's risk configuration, which holds the base currency,
     * the thread count, the observation model and the market data of the run.
     */
    ores::reporting::domain::risk_report_config risk;

    /**
     * @brief The analytics of the risk configuration with their parameters, in
     * the order the run executes them.
     */
    std::vector<messaging::run_analytic> analytics;

    /**
     * @brief The configuration type codes the definition binds.
     */
    std::vector<std::string> bound_types;

    /**
     * @brief The name of the today's market configuration the run prices
     * against.
     */
    std::string market_configuration = "default";
};

/**
 * @brief Builds the run document for a definition.
 *
 * The document names one input file for each configuration type the definition
 * binds, and nothing for a type it does not bind, so the export writes exactly
 * the files that exist. It carries the analytics of the configuration with
 * their parameters, and binds the market role to the named today's market
 * configuration. A run with no analytic that is on is refused, because an ORE
 * run with none does nothing.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT messaging::run_document
generate_run_document(const run_document_inputs& in);

}

#endif
