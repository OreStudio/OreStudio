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
#ifndef ORES_ORE_CORE_RESULTS_PAYLOAD_INDEX_HPP
#define ORES_ORE_CORE_RESULTS_PAYLOAD_INDEX_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include <cstddef>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::results {

/**
 * @brief The most distinct values recorded for one key of a payload.
 *
 * A scenario cube holds a value for every date, scenario and factor of a run, so
 * the list is a map of what the file covers and not a copy of it. The cap keeps
 * the index small enough to read with the payload; a file that exceeds it says
 * so, and a reader that needs the exact set reads the payload.
 */
inline constexpr std::size_t payload_index_value_limit = 1000;

/**
 * @brief What a market-state file holds, so a reader can find its slice.
 *
 * The cube families are wide: =stress_scenarios.csv= states 829 columns, one for
 * every curve and tenor the run built. A row per value would be millions of rows
 * per run, and a read that needs one factor would unpack all of them, so the file
 * is stored as it is and this index says what it covers.
 */
struct payload_index {
    /**
     * @brief The headings the file states, in the order it states them.
     */
    std::vector<std::string> columns;
    /**
     * @brief How many rows the file states, not counting the heading.
     */
    std::size_t rows = 0;
    /**
     * @brief The distinct values of each key column, in the catalogue's order.
     *
     * The dates, scenarios, factors or market objects the file covers, which is
     * what a reader filters on before it decides to unpack.
     */
    std::vector<std::pair<std::string, std::vector<std::string>>> keys;
    /**
     * @brief Whether a key held more values than the index records.
     */
    bool truncated = false;
};

/**
 * @brief Reads the headings and the key values of a market-state file.
 *
 * The catalogue says which of a family's columns are keys, so the index is the
 * distinct values of those columns and nothing else: the figures are the
 * payload's own and are not read here.
 *
 * The file is not refused for a figure it states badly, because no figure is
 * read. It is refused for stating no heading, and a key column the file does not
 * state is left out of the index rather than guessed at.
 *
 * @param csv     the file's bytes, header included.
 * @param family  the family the catalogue gives the file.
 * @param refusals the reasons the file could not be indexed.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT payload_index index_payload(std::string_view csv,
                                                               const output_family& family,
                                                               std::vector<std::string>& refusals);

/**
 * @brief Indexes a file by its name, asking the catalogue which family it is.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT payload_index index_payload_for_file(
    std::string_view csv, std::string_view file_name, std::vector<std::string>& refusals);

}

#endif
