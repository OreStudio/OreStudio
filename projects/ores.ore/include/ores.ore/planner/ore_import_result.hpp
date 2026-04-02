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
#ifndef ORES_ORE_PLANNER_ORE_IMPORT_RESULT_HPP
#define ORES_ORE_PLANNER_ORE_IMPORT_RESULT_HPP

#include <string>
#include <vector>
#include "ores.ore/planner/ore_instrument_error.hpp"

namespace ores::ore::planner {

/**
 * @brief Outcome of a completed ORE import execution.
 *
 * Failures for currencies, portfolios, books, and trades are fatal: success
 * is false and error carries the reason. Instrument save failures are
 * non-fatal: success may still be true while instrument_errors is non-empty.
 */
struct ore_import_result {
    /**
     * @brief True when currencies, portfolios, books, and trades were all saved.
     *
     * Instrument failures do not set this to false; check instrument_errors.
     */
    bool success = false;

    /**
     * @brief Fatal error message when success is false.
     */
    std::string error;

    int currencies  = 0;
    int portfolios  = 0;
    int books       = 0;
    int trades      = 0;
    int instruments = 0;

    /**
     * @brief One entry per instrument save failure, in save order.
     *
     * Non-empty even when success is true (instrument failures are non-fatal).
     */
    std::vector<ore_instrument_error> instrument_errors;
};

}

#endif
