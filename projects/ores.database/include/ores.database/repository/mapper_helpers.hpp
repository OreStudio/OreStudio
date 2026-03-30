/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DATABASE_REPOSITORY_MAPPER_HELPERS_HPP
#define ORES_DATABASE_REPOSITORY_MAPPER_HELPERS_HPP

#include <vector>
#include <chrono>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.platform/time/time_utils.hpp"

namespace ores::database::repository {

/**
 * @brief Maps a vector of source objects to a vector of destination objects.
 *
 * This template function provides a generic way to map vectors using std::ranges::transform.
 * It handles logging, memory pre-allocation, and the transformation in a consistent way.
 *
 * @tparam Source The source vector element type
 * @tparam Dest The destination vector element type
 * @tparam MapFunc The mapping function type (typically a lambda or function pointer)
 * @param source The source vector to map from
 * @param map_func The function to map individual elements (Source -> Dest)
 * @param lg The logger to use for logging
 * @param log_prefix The prefix for log messages (e.g., "db entities" or "domain entities")
 * @return A vector of destination objects
 *
 * @example
 * auto domain_vec = map_vector<entity_type, domain_type>(
 *     entities,
 *     [](const auto& e) { return mapper::map(e); },
 *     lg,
 *     "db entities"
 * );
 */
template<typename Source, typename Dest, typename MapFunc>
std::vector<Dest> map_vector(
    const std::vector<Source>& source,
    MapFunc&& map_func,
    logging::logger_t& lg,
    const std::string& log_prefix) {

    using namespace ores::logging;

    BOOST_LOG_SEV(lg, debug) << "Mapping " << log_prefix
                             << ". Total: " << source.size();

    std::vector<Dest> result;
    result.reserve(source.size());
    std::ranges::transform(source, std::back_inserter(result),
        std::forward<MapFunc>(map_func));

    BOOST_LOG_SEV(lg, debug) << "Mapped " << log_prefix << ".";
    return result;
}

/**
 * @brief Converts a database timestamp string to a system_clock::time_point.
 *
 * The string is expected in the format "%Y-%m-%d %H:%M:%S" in the database
 * session's local timezone (i.e. what libpq returns from a timestamptz column
 * after stripping the timezone offset via rfl::Timestamp). Parsing with
 * parse_time_point (mktime) converts from session-local time to UTC, which is
 * correct for any session timezone — BST, JST, EST, etc.
 *
 * @param timestamp_str The timestamp string to convert
 * @return A system_clock::time_point representing the same instant in time
 *
 * @example
 * auto tp = timestamp_to_timepoint("2026-03-30 12:34:56"); // BST input → UTC tp
 */
inline std::chrono::system_clock::time_point
timestamp_to_timepoint(std::string_view timestamp_str) {
    return platform::time::datetime::parse_time_point(
        std::string{timestamp_str});
}

/**
 * @brief Converts a sqlgen Timestamp to a std::chrono::system_clock::time_point.
 *
 * rfl::Timestamp already holds the parsed std::tm internally. This overload
 * converts directly via to_time_point_local, avoiding the wasteful round-trip
 * through str() → re-parse string that the string_view overload would do.
 *
 * @param ts The sqlgen Timestamp to convert (holds session-local time)
 * @return A system_clock::time_point representing the same instant in time
 *
 * @example
 * auto tp = timestamp_to_timepoint(entity.last_login);
 */
inline std::chrono::system_clock::time_point
timestamp_to_timepoint(const sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">& ts) {
    return platform::time::time_utils::to_time_point_local(ts.get());
}

/**
 * @brief Converts a std::chrono::system_clock::time_point to a sqlgen Timestamp.
 *
 * Formats a C++ chrono time_point as a UTC timestamp string in the format
 * "%Y-%m-%d %H:%M:%S" suitable for database storage.
 *
 * @param tp The time_point to convert
 * @param lg The logger to use for logging
 * @return A sqlgen Timestamp, or an empty Timestamp if conversion fails
 *
 * @example
 * entity.last_login = timepoint_to_timestamp(std::chrono::system_clock::now(), lg);
 */
inline sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">
timepoint_to_timestamp(const std::chrono::system_clock::time_point& tp,
    logging::logger_t& lg) {
    using namespace ores::logging;

    const auto s = platform::time::datetime::format_time_point_utc(tp);
    const auto r = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">::from_string(s);
    if (!r) {
        BOOST_LOG_SEV(lg, error) << "Error converting timepoint to timestamp";
        return {};
    }
    return r.value();
}

}

#endif
