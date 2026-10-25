/** -*- mode: c++; tab-width: 4; indent-tabs-mode: nil -*-
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
 *
 */
#ifndef ORES_REPORTING_CORE_SERVICE_RUN_RESULTS_HPP
#define ORES_REPORTING_CORE_SERVICE_RUN_RESULTS_HPP

#include "ores.reporting.core/export.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief One trade's result, as the engine wrote it into npv.csv.
 *
 * The figures stay text. The engine writes them in a currency and a precision
 * of its own choosing, and a screen that formatted them here would be deciding
 * what the reader sees before the reader has asked.
 */
struct run_result_row {
    std::string trade_id;
    std::string trade_type;
    std::string npv;
    std::string npv_base;
    std::string notional;
    std::string counterparty;
};

/**
 * @brief What reading a run's result found.
 *
 * A run whose archive holds no npv.csv produced no result to read, which is a
 * fact about the run rather than an exception, so it is a message here.
 */
struct run_results_read {
    std::vector<run_result_row> rows;
    std::string error;

    [[nodiscard]] bool ok() const {
        return error.empty();
    }
};

/**
 * @brief Reads the per-trade results from the output archive of one run.
 *
 * The archive is the one the compute wrapper uploads, and npv.csv is the file
 * the npv analytic writes into it. The columns are read by name rather than by
 * position, so a column the engine adds or moves does not shift the figures.
 */
ORES_REPORTING_CORE_EXPORT run_results_read
read_run_results_from_archive(std::span<const std::byte> archive);

}

#endif
