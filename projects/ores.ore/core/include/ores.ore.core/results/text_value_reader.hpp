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
#ifndef ORES_ORE_CORE_RESULTS_TEXT_VALUE_READER_HPP
#define ORES_ORE_CORE_RESULTS_TEXT_VALUE_READER_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::results {

/**
 * @brief One value a run stated as text rather than as a figure.
 *
 * Some outputs state what a run was about rather than what it computed: the
 * pricing engine a trade used, the market objects and risk factors a portfolio
 * depends on, the calibrated model. Each value is a name and its text, about a
 * trade when the file names one, with the number beside it when the engine
 * stated one.
 */
struct text_value_row {
    /**
     * @brief The kind of record the value is about, trade or empty when the file names none.
     */
    std::string subject_type;
    /**
     * @brief The engine's identifier for that record, empty when the file names none.
     */
    std::string subject;
    /**
     * @brief What the value is, such as PricingConfigEngine or DiscountCurve.
     */
    std::string name;
    /**
     * @brief The type the engine stated for the value, or what the file implies: text or xml.
     */
    std::string value_type;
    /**
     * @brief The value as the engine wrote it.
     */
    std::string value;
    /**
     * @brief The value as a number, when the engine stated it as a double.
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
struct text_value_read_outcome {
    /**
     * @brief The values the file stated.
     */
    std::vector<text_value_row> rows;
    /**
     * @brief The reasons the file could not be read whole.
     */
    std::vector<std::string> refusals;
};

/**
 * @brief Whether a family states text values: additional results, the lists of
 * portfolio details, or the calibrated model.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT bool is_text_value_family(const output_family& family);

/**
 * @brief Reads a family of text values, one row for each line.
 *
 * The calibrated model is one XML document under one heading, and the engine
 * does not quote it, so everything after the heading is the one value.
 *
 * @param csv     the file's bytes, header included.
 * @param family  the family the catalogue gives the file.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT text_value_read_outcome
read_text_values(std::string_view csv, const output_family& family);

}

#endif
