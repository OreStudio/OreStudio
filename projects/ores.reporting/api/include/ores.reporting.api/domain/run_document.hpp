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
#ifndef ORES_REPORTING_API_DOMAIN_RUN_DOCUMENT_HPP
#define ORES_REPORTING_API_DOMAIN_RUN_DOCUMENT_HPP

#include "ores.reporting.api/domain/report_analytic.hpp"
#include "ores.reporting.api/domain/report_market_binding.hpp"
#include "ores.reporting.api/domain/report_run_setup.hpp"
#include <string>
#include <vector>

namespace ores::reporting::domain {

/**
 * @brief One ORE run document as reporting stores it against a report
 * definition: the setup, the analytics with their parameters, and the market
 * bindings.
 */
/**
 * @brief One parameter of an analytic, by the name the run document uses.
 *
 * The stored parameter holds a definition id and a value, and the id is a
 * database concern. The document is one step earlier, where the parameter is
 * still the name ORE wrote, so the name rides beside the value and the store
 * resolves it.
 */
struct run_parameter {
    std::string name;
    std::string value;
    int position = 0;

    friend bool operator==(const run_parameter&, const run_parameter&) = default;
};

/**
 * @brief An analytic of the run and the parameters it sets.
 *
 * The active flag is a parameter in ORE's schema and a column on the analytic,
 * so it lives on the analytic; the remaining parameters stay beside it, in the
 * order the document wrote them.
 */
struct run_analytic {
    report_analytic analytic;
    std::vector<run_parameter> parameters;

    friend bool operator==(const run_analytic&, const run_analytic&) = default;
};

struct run_document {
    report_run_setup setup;
    std::vector<run_analytic> analytics;
    std::vector<report_market_binding> market_bindings;

    friend bool operator==(const run_document&, const run_document&) = default;
};

}

#endif
