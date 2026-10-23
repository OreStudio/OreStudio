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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_TICK_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_TICK_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstddef>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief One tick of a series: a value of one coordinate at one time.
 *
 * A tick has no id and no source, and nobody corrects it, so two ticks with one coordinate and
 * one time are the same tick.
 */
struct tick final {
    std::chrono::system_clock::time_point at;
    std::string oresmd_uri;
    double value = 0.0;
};

/**
 * @brief The reads of the tick table.
 *
 * The tick table is append-only and has one index, on the series and the time, so a read
 * of one series in a span of time is one probe of that index. The party policy of the table
 * decides which ticks a session sees.
 */
class ORES_MARKETDATA_CORE_EXPORT tick_repository {
private:
    inline static std::string_view logger_name = "ores.marketdata.repository.tick_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief The ticks of @p series_id from @p from, inclusive, to @p to, exclusive.
     *
     * The order is by time and then by the number of the coordinate in the series dictionary.
     * The read stops after @p limit ticks, so a caller that asks for one more than its ceiling
     * can tell that the span holds more. The cut is by time alone, so when the span holds more
     * than @p limit ticks, which ticks of the last instant are in the answer is not defined.
     * A span that holds no more than @p limit ticks is answered whole.
     *
     * @param coordinates The coordinate URIs to read, or empty for every coordinate.
     */
    std::vector<tick> read_range(context ctx,
                                 const boost::uuids::uuid& series_id,
                                 std::chrono::system_clock::time_point from,
                                 std::chrono::system_clock::time_point to,
                                 const std::vector<std::string>& coordinates,
                                 std::size_t limit);
};

}

#endif
