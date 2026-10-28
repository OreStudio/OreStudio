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
#ifndef ORES_ORE_CORE_RESULTS_CANONICAL_ROWS_HPP
#define ORES_ORE_CORE_RESULTS_CANONICAL_ROWS_HPP

#include "ores.ore.core/export.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::results {

/**
 * @brief One figure an analytic wrote, in the shape the result table holds.
 *
 * @see marshal_npv_csv
 */
struct canonical_row {
    /**
     * @brief The kind of record the figure is about, =trade=.
     */
    std::string subject_type;
    /**
     * @brief The engine's own identifier for the record.
     */
    std::string subject_id;
    /**
     * @brief The engine's name for the figure, =npv=, =npv_base= or =notional=.
     */
    std::string measure;
    /**
     * @brief The currency the figure is stated in, empty when it has none.
     */
    std::string currency;
    /**
     * @brief The counterparty the engine stated beside the trade, empty when it stated none.
     */
    std::string counterparty_id;
    /**
     * @brief The figure as the engine stated it.
     */
    double figure = 0.0;
    /**
     * @brief The file the row came from.
     */
    std::string source;
};

/**
 * @brief What a projection produced, and what it refused.
 *
 * A refusal is a file or a row the projection will not guess at. Any refusal
 * means the result must not be loaded, because a partial load reads as a
 * complete one.
 */
struct marshal_outcome {
    /**
     * @brief The rows the file stated.
     */
    std::vector<canonical_row> rows;
    /**
     * @brief The reasons the file or its rows could not be read.
     */
    std::vector<std::string> refusals;
};

/**
 * @brief Projects an =npv.csv= onto the canonical rows.
 *
 * The projection renames the engine's headings rather than interpreting them:
 * =TradeId= becomes the subject, =NPV=, =NPV(Base)= and =Notional= each become a
 * row of their own measure, and the currency beside each figure is carried with
 * it. It refuses a file whose headings it does not know, a row with no trade id,
 * and a figure it cannot read.
 *
 * @param csv          the file's bytes, header included.
 * @param source_file  the file name, written to every row's source.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT marshal_outcome marshal_npv_csv(std::string_view csv,
                                                                   std::string_view source_file);

/**
 * @brief The canonical rows as the newline-delimited CSV the loader reads.
 *
 * The header is =subject_type,subject_id,measure,currency,figure,source=, and a
 * field is quoted only when it holds a comma, a quote or a newline.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::string
canonical_csv(const std::vector<canonical_row>& rows);

}

#endif
