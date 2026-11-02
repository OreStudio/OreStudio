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
#ifndef ORES_ORE_CORE_RESULTS_CALIBRATION_READER_HPP
#define ORES_ORE_CORE_RESULTS_CALIBRATION_READER_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::results {

/**
 * @brief One value the engine stated about a market object it calibrated.
 *
 * A calibration states what a curve, a surface or a model came out as: a pillar's
 * rate, a day counter, a currency. The value is a number as often as it is text,
 * and the row says which, so the text is kept as stated and the number beside it
 * when there is one.
 */
struct calibration_row {
    /**
     * @brief The kind of market object, such as yieldCurve.
     */
    std::string market_object_type;
    /**
     * @brief The engine's name for the market object, such as USD3M.
     */
    std::string market_object_id;
    /**
     * @brief The engine's name for the value, such as time or dayCounter.
     */
    std::string result_name;
    /**
     * @brief The first key that places the value within the object, such as a pillar date.
     */
    std::string result_key_1;
    /**
     * @brief The second key, such as the instrument the pillar was built from.
     */
    std::string result_key_2;
    /**
     * @brief The third key, empty when the object needs only two.
     */
    std::string result_key_3;
    /**
     * @brief The type the engine stated for the value, such as double or string.
     */
    std::string result_type;
    /**
     * @brief The value as the engine wrote it.
     */
    std::string value;
    /**
     * @brief The value as a number, when the engine stated it as one.
     */
    std::optional<double> figure;
    /**
     * @brief The line of the file that states the value, counting the heading as line one.
     */
    int line = 0;
};

/**
 * @brief What a read produced, and what it refused.
 */
struct calibration_read_outcome {
    /**
     * @brief The values the file stated.
     */
    std::vector<calibration_row> rows;
    /**
     * @brief The reasons the file could not be read whole.
     */
    std::vector<std::string> refusals;
};

/**
 * @brief Whether a family states the calibration of the market the run built.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT bool is_calibration_family(const output_family& family);

/**
 * @brief Reads a family of calibration values, one row for each line.
 *
 * A row that names no market object is refused, because its value belongs to
 * nothing. A row whose type is double and whose value is not a number is
 * refused, and the line is named.
 *
 * @param csv     the file's bytes, header included.
 * @param family  the family the catalogue gives the file.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT calibration_read_outcome
read_calibration(std::string_view csv, const output_family& family);

}

#endif
