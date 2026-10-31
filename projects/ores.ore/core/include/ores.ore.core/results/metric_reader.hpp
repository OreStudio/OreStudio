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
#ifndef ORES_ORE_CORE_RESULTS_METRIC_READER_HPP
#define ORES_ORE_CORE_RESULTS_METRIC_READER_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::results {

/**
 * @brief One figure a run produced, at one subject and one observation date.
 *
 * The measure is the engine's own name for the figure: a column heading such as
 * =NPV=, and also a curve or a risk factor, because a configuration-driven
 * family states each of its figures as a column.
 */
struct metric {
    /**
     * @brief The kind of record the figure is about: a trade, a netting set, a portfolio.
     */
    std::string subject_type;
    /**
     * @brief The engine's own identifier for that record.
     */
    std::string subject;
    /**
     * @brief The observation date or time step the figure belongs to, empty when the row states
     * none.
     */
    std::string as_of;
    /**
     * @brief The engine's name for the figure.
     */
    std::string measure;
    /**
     * @brief The currency the figure is stated in, empty when it states none.
     */
    std::string currency;
    /**
     * @brief The context of the figure, in the order the file states it.
     */
    std::vector<std::pair<std::string, std::string>> qualifiers;
    /**
     * @brief The figure as the engine stated it.
     */
    double value = 0.0;
    /**
     * @brief The file the metric came from.
     */
    std::string source;
};

/**
 * @brief What a read produced, and what it refused.
 *
 * A refusal is a row or a heading the reader will not guess at. Any refusal
 * means the result must not be loaded, because a partial load reads as a
 * complete one.
 */
struct metric_read_outcome {
    /**
     * @brief The metrics the file stated.
     */
    std::vector<metric> metrics;
    /**
     * @brief The reasons the file could not be read whole.
     */
    std::vector<std::string> refusals;
};

/**
 * @brief Reads a family whose row is keys, qualifiers and figures.
 *
 * Every column states its role in the catalogue. A key identifies the row and
 * must be stated, or the row is refused. A qualifier travels with each metric.
 * Each measure column becomes one metric named by its heading, so a curve, a
 * risk factor and a quantile need no reader of their own.
 *
 * A figure the engine states as its null string is not a metric and is not a
 * refusal: there is nothing to load. A figure that is not a number is refused,
 * and the heading it was found under is named.
 *
 * In a family whose measures are open, a heading the catalogue does not state is
 * read as a measure. In a family whose measures are closed it is refused,
 * because there the shape is known and a stranger means the reader is wrong.
 *
 * @param csv     the file's bytes, header included.
 * @param family  the family the catalogue gives the file.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT metric_read_outcome read_metrics(std::string_view csv,
                                                                    const output_family& family);

/**
 * @brief Reads a file by its name, asking the catalogue which family it is.
 *
 * A name no family covers is refused rather than guessed at.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT metric_read_outcome
read_metrics_for_file(std::string_view csv, std::string_view file_name);

}

#endif
