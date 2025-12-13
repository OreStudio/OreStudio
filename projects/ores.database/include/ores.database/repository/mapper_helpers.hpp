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
#include <iomanip>
#include <sstream>
#include <format>
#include <sqlgen/postgres.hpp>
#include "ores.utility/log/make_logger.hpp"

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
    utility::log::logger_t& lg,
    const std::string& log_prefix) {

    using namespace ores::utility::log;

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
 * @brief Converts a sqlgen Timestamp to a std::chrono::system_clock::time_point.
 *
 * Parses a timestamp string in the format "%Y-%m-%d %H:%M:%S" and converts
 * it to a C++ chrono time_point for easier time manipulation.
 *
 * @param ts The sqlgen Timestamp to convert
 * @return A system_clock::time_point representing the same instant in time
 *
 * @example
 * auto tp = timestamp_to_timepoint(entity.last_login);
 */
inline std::chrono::system_clock::time_point
timestamp_to_timepoint(const sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">& ts) {
    const auto str = ts.str();
    std::tm tm = {};
    std::istringstream ss(str);
    ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");
    return std::chrono::system_clock::from_time_t(std::mktime(&tm));
}

/**
 * @brief Converts a std::chrono::system_clock::time_point to a sqlgen Timestamp.
 *
 * Formats a C++ chrono time_point as a timestamp string in the format
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
    utility::log::logger_t& lg) {
    using namespace ores::utility::log;

    const auto s = std::format("{:%Y-%m-%d %H:%M:%S}", tp);
    const auto r = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">::from_string(s);
    if (!r) {
        BOOST_LOG_SEV(lg, error) << "Error converting timepoint to timestamp";
        return {};
    }
    return r.value();
}

}

#endif
