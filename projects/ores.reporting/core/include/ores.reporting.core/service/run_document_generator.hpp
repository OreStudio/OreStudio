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

#include "ores.reporting.api/domain/configuration_market_binding.hpp"
#include "ores.reporting.api/domain/file_type.hpp"
#include "ores.reporting.api/domain/risk_report_config.hpp"
#include "ores.reporting.api/messaging/run_document_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief What a run document is built from.
 *
 * Every field comes from the risk report configuration the definition binds or
 * from the bindings of the definition.
 */
struct run_document_inputs {
    /**
     * @brief The risk report configuration, which holds the choices of the run:
     * the base currency, the thread count, the observation model, the folders,
     * the flags and the market data of the run.
     */
    ores::reporting::domain::risk_report_config risk;

    /**
     * @brief The date the run is built on, as an ISO 8601 date. A live
     * configuration values the portfolio on it and an end of day one on the
     * weekday before it. A configuration that names its own date ignores it.
     */
    std::string run_date;

    /**
     * @brief The analytics of the risk configuration with their parameters, in
     * the order the run executes them.
     */
    std::vector<messaging::run_analytic> analytics;

    /**
     * @brief The market bindings of the configuration, in the order the run
     * writes them.
     */
    std::vector<ores::reporting::domain::configuration_market_binding> market_bindings;

    /**
     * @brief The configuration type codes the definition binds.
     */
    std::vector<std::string> bound_types;

    /**
     * @brief The file types of the ORE universe. Every file name in the run
     * document comes from here, so no name is written twice.
     */
    std::vector<ores::reporting::domain::file_type> file_types;
};

/**
 * @brief Builds the run document for a definition.
 *
 * The document carries the choices, the analytics and the market bindings of the
 * configuration as they are. It names one input file for each configuration type
 * the definition binds, and nothing for a type it does not bind, so the export
 * writes exactly the files that exist. A bound type that no file type names is
 * refused, because the export could not name its file.
 *
 * Each analytic also names the files its file types mark as defaults: the files
 * it writes, and the files it reads that another analytic of the run writes. A
 * parameter the configuration states for the same file overrides the default. A
 * reader takes the name its writer has, so two analytics that share a file read
 * one name. The settings of the document always name
 * the date the run values the portfolio at, resolved from the configuration's
 * market data type. A run with no analytic that is on is
 * refused, because an ORE run with none does nothing.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT messaging::run_document
generate_run_document(const run_document_inputs& in);

}

#endif
