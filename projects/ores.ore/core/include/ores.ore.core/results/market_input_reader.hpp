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
#ifndef ORES_ORE_CORE_RESULTS_MARKET_INPUT_READER_HPP
#define ORES_ORE_CORE_RESULTS_MARKET_INPUT_READER_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::results {

/**
 * @brief One value of the market a run read: a quote, a fixing or a dividend.
 *
 * The engine writes back the market data it loaded, so a run states what it
 * priced against. The three files share one shape: a date, the thing the value
 * is for, and the value, with a payment date for a dividend.
 */
struct market_input_row {
    /**
     * @brief The date the value is for: a quote's date, a fixing's date or a dividend's ex date.
     */
    std::string as_of;
    /**
     * @brief What the value is for: a quote's datum id, a fixing's index or a dividend's equity.
     */
    std::string datum_id;
    /**
     * @brief The value as the engine stated it.
     */
    double value = 0.0;
    /**
     * @brief The day a dividend is paid, empty for a quote or a fixing.
     */
    std::string payment_date;
    /**
     * @brief The line of the file that states the value, counting the heading as line one.
     */
    int line = 0;
};

/**
 * @brief What a read produced, and what it refused.
 */
struct market_input_read_outcome {
    /**
     * @brief The values the file stated.
     */
    std::vector<market_input_row> rows;
    /**
     * @brief The reasons the file could not be read whole.
     */
    std::vector<std::string> refusals;
};

/**
 * @brief Whether a family states the market a run read: marketdata, fixings or dividends.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT bool is_market_input_family(const output_family& family);

/**
 * @brief Reads a family of market inputs, one row for each line.
 *
 * A row that names no date or nothing to value is refused, as is a value that is
 * not a figure, and the line is named.
 *
 * @param csv     the file's bytes, header included.
 * @param family  the family the catalogue gives the file.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT market_input_read_outcome
read_market_inputs(std::string_view csv, const output_family& family);

}

#endif
