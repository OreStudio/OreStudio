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
#include "ores.marketdata.core/repository/as_of_rows.hpp"
#include <stdexcept>

namespace ores::marketdata::repository {

namespace {

constexpr std::size_t observation_columns = 10;

}

market_observation_entity
as_of_observation(const as_of_row& row, std::size_t first, std::string_view read) {
    if (row.size() < first + observation_columns)
        throw std::runtime_error(std::string(read) + ": a row has " + std::to_string(row.size()) +
                                 " columns, expected " +
                                 std::to_string(first + observation_columns));

    market_observation_entity e;
    e.id = row[first].value_or("");
    e.tenant_id = row[first + 1].value_or("");
    e.party_id = row[first + 2].value_or("");
    e.series_id = row[first + 3].value_or("");
    e.observation_datetime = row[first + 4].value_or("");
    e.oresmd_uri = row[first + 5].value_or("");
    e.value = row[first + 6].value_or("");
    e.source = row[first + 7];
    e.valid_from = row[first + 8].value_or("");
    e.valid_to = row[first + 9].value_or("");
    return e;
}

std::size_t as_of_bucket_ordinal(const as_of_row& row, std::size_t bucket_count) {
    if (row.empty() || !row[0])
        throw std::runtime_error("read_as_of_buckets: a row has no bucket ordinal");

    std::size_t ordinal = 0;
    try {
        std::size_t used = 0;
        ordinal = std::stoul(*row[0], &used);
        if (used != row[0]->size())
            throw std::invalid_argument("trailing characters");
    } catch (const std::logic_error&) {
        throw std::runtime_error("read_as_of_buckets: bucket ordinal '" + *row[0] +
                                 "' is not a number");
    }
    if (ordinal >= bucket_count)
        throw std::runtime_error("read_as_of_buckets: bucket ordinal " + *row[0] +
                                 " is outside the " + std::to_string(bucket_count) +
                                 " buckets requested");
    return ordinal;
}

}
