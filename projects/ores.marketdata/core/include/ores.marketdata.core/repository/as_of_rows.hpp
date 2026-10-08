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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_AS_OF_ROWS_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_AS_OF_ROWS_HPP

#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/market_observation_entity.hpp"
#include <chrono>
#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief One row of a raw multi-column read, as the database returns it.
 */
using as_of_row = std::vector<std::optional<std::string>>;

/**
 * @brief One point of an as-of snapshot, with the row's record time.
 *
 * The domain type carries no bitemporal tail, so an observed value cannot say
 * when it was recorded. A view that shows how stale a term structure is needs
 * both facts together, and a reading that carried them apart could pair a value
 * with a record time that never belonged to it.
 */
struct observation_record final {
    domain::market_observation observation;

    /**
     * @brief The row's bitemporal valid_from: when this value was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief The observation an as-of read row carries, from column @p first on.
 *
 * The ten observation columns run from @p first in table order. A row with
 * fewer columns would leave a curve snapshot short a pillar with no sign, so it
 * throws, naming @p read.
 *
 * @throws std::runtime_error if the row is too short.
 */
ORES_MARKETDATA_CORE_EXPORT market_observation_entity as_of_observation(const as_of_row& row,
                                                                        std::size_t first,
                                                                        std::string_view read);

/**
 * @brief The record time an as-of read row carries, from column @p first on.
 *
 * The row's valid_from is the last of the ten observation columns. It is a
 * non-nullable bitemporal column, so a read that does not select it is a
 * mistake rather than a null, and this throws, naming @p read.
 *
 * @throws std::runtime_error if the row is too short or carries no record time.
 */
ORES_MARKETDATA_CORE_EXPORT std::chrono::system_clock::time_point
as_of_recorded_at(const as_of_row& row, std::size_t first, std::string_view read);

/**
 * @brief The bucket a bucketed as-of row belongs to, from its first column.
 *
 * @throws std::runtime_error if the ordinal is missing, is not all digits, or
 * is not below @p bucket_count.
 */
ORES_MARKETDATA_CORE_EXPORT std::size_t as_of_bucket_ordinal(const as_of_row& row,
                                                             std::size_t bucket_count);

}

#endif
