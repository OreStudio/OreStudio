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
#ifndef ORES_REPORTING_CORE_RESULTS_CANONICAL_READER_HPP
#define ORES_REPORTING_CORE_RESULTS_CANONICAL_READER_HPP

#include "ores.reporting.core/export.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::results {

/**
 * @brief One figure a run produced, as the loader reads it.
 *
 * @see read_canonical_csv
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
     * @brief The engine's name for the figure.
     */
    std::string measure;
    /**
     * @brief The currency the figure is stated in, empty when it has none.
     */
    std::string currency;
    /**
     * @brief The figure.
     */
    double figure = 0.0;
    /**
     * @brief The file the row came from.
     */
    std::string source;
};

/**
 * @brief What the loader read, and what it refused.
 *
 * Any refusal means the run's rows must not be loaded: a partial load reads as
 * a complete one.
 */
struct read_outcome {
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
 * @brief Reads the canonical rows a node wrote.
 *
 * The file is the contract: =subject_type,subject_id,measure,currency,figure,
 * source=, one row per line. The reader refuses a file whose header it does not
 * know, a row whose subject or measure is empty, a row whose figure it cannot
 * read, and a row with more fields than the header names, rather than loading
 * part of a run's result.
 *
 * @param csv  the file's bytes.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT read_outcome read_canonical_csv(std::string_view csv);

}

#endif
