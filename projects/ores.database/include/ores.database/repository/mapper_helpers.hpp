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

#include "ores.database/repository/db_types.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.platform/time/time_utils.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

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
template <typename Source, typename Dest, typename MapFunc>
std::vector<Dest> map_vector(const std::vector<Source>& source,
                             MapFunc&& map_func,
                             logging::logger_t& lg,
                             const std::string& log_prefix) {

    using namespace ores::logging;

    BOOST_LOG_SEV(lg, debug) << "Mapping " << log_prefix << ". Total: " << source.size();

    std::vector<Dest> result;
    result.reserve(source.size());
    std::ranges::transform(source, std::back_inserter(result), std::forward<MapFunc>(map_func));

    BOOST_LOG_SEV(lg, debug) << "Mapped " << log_prefix << ".";
    return result;
}

/**
 * @brief Converts a database timestamp string to a system_clock::time_point.
 *
 * The string must include a UTC designator (Z, +00, or +00:00). All DB sessions
 * are forced to UTC via SET TIME ZONE 'UTC', so PostgreSQL always returns
 * timestamptz values as "YYYY-MM-DD HH:MM:SS+00".
 *
 * @param timestamp_str The timestamp string to convert (must have UTC designator)
 * @return A system_clock::time_point representing the UTC instant
 * @throws std::invalid_argument if the string has no UTC designator
 *
 * @example
 * auto tp = timestamp_to_timepoint("2026-03-30 12:34:56+00"); // UTC input → UTC tp
 */
/**
 * @brief Reads a PostgreSQL boolean column returned as text.
 *
 * @details A boolean read through the text protocol arrives as
 * @c t or @c f, never as @c true or @c 0. Both of those turn up
 * anyway: @c true from a column a query casts to text explicitly, and
 * @c 1 from one cast to integer or still stored as an integer flag.
 * All three true spellings are accepted so a caller need not know
 * which its query produces. Anything else is false, including a null
 * column — a boolean that is absent is not true.
 *
 * Takes the optional a text-protocol row holds rather than a string,
 * so there is one signature and no ambiguity at the call site.
 *
 * Exists so that reading a boolean is spelled one way. It was spelled
 * three before: a bare @c "t" comparison, a three-way comparison, and
 * @c std::stoi, which throws on the @c t PostgreSQL actually returns.
 *
 * @example
 * entity.is_active = text_to_bool(row[10]);
 */
inline bool text_to_bool(const std::optional<std::string>& text) {
    if (!text)
        return false;
    return *text == "t" || *text == "true" || *text == "1";
}

inline std::chrono::system_clock::time_point
timestamp_to_timepoint(std::string_view timestamp_str) {
    return platform::time::datetime::from_iso8601_utc(std::string{timestamp_str});
}

/**
 * @brief The instant a bitemporal row stopped being valid, when it has stopped.
 *
 * A row that is current ends at the clock's maximum, which stands for "never"
 * and is not an instant anyone means; the database stores it as 9999-12-31
 * 23:59:59, and the layer's codecs read that back as the maximum. A row whose
 * end is any other value was closed or replaced at that instant. Reading the
 * end through here keeps the sentinel in one place, so a caller never compares
 * a timestamp with it.
 *
 * @param valid_to The row's valid_to, as the entity holds it
 * @return The instant the row ended, or nothing while it is current
 *
 * @example
 * row.closed_at = validity_end(entity.valid_to);
 */
inline std::optional<std::chrono::system_clock::time_point>
validity_end(const db_timestamp& valid_to) {
    if (valid_to == db_timestamp::max())
        return std::nullopt;
    return valid_to;
}

}

#endif
