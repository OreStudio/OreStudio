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
#ifndef ORES_UTILITY_REPOSITORY_HELPERS_HPP
#define ORES_UTILITY_REPOSITORY_HELPERS_HPP

#include <string>
#include <format>
#include <boost/exception/diagnostic_information.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.utility/log/severity_level.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/repository/repository_exception.hpp"

namespace ores::utility::repository {

/**
 * @brief Maximum timestamp used for bitemporal records (represents "infinity").
 *
 * This constant is used to mark records that are currently valid (not yet ended).
 * In bitemporal databases, a record with valid_to = MAX_TIMESTAMP indicates
 * that the record is still current and has not been superseded or deleted.
 */
inline constexpr const char* MAX_TIMESTAMP = "9999-12-31 23:59:59";

/**
 * @brief Ensures a repository operation result is successful, throwing an exception if not.
 *
 * This helper checks if a repository operation returned a successful result.
 * If the result indicates an error, it logs the error and throws a repository_exception
 * with details about the failure.
 *
 * @tparam T The type of the result (must support operator bool and have an error() method)
 * @param result The result to check
 * @throws repository_exception if the result indicates failure
 *
 * @example
 * auto result = session.and_then(query);
 * ensure_success(result); // Throws if query failed
 */
template<typename T>
void ensure_success(const T& result, utility::log::logger_t& lg) {
    using namespace ores::utility::log;

    if (!result) {
        BOOST_LOG_SEV(lg, severity_level::error) << result.error().what();
        BOOST_THROW_EXCEPTION(
            repository_exception(std::format("Repository error: {}",
                    result.error().what())));
    }
}

/**
 * @brief Converts a string to a sqlgen Timestamp, throwing an exception if conversion fails.
 *
 * This helper parses a timestamp string in the format "%Y-%m-%d %H:%M:%S"
 * and returns a sqlgen::Timestamp object. If parsing fails, it logs the error
 * and throws a repository_exception.
 *
 * @param s The timestamp string to convert (format: "YYYY-MM-DD HH:MM:SS")
 * @return Expected<Timestamp> on success
 * @throws repository_exception if timestamp parsing fails
 *
 * @example
 * auto ts = make_timestamp("2025-11-25 12:30:45");
 */
inline auto make_timestamp(const std::string& s, utility::log::logger_t& lg) {
    using namespace ores::utility::log;

    const auto r = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">::from_string(s);
    if (!r) {
        BOOST_LOG_SEV(lg, error) << "Error converting timestamp: '" << s
                                 << "'. Error: " << r.error().what();
        BOOST_THROW_EXCEPTION(
            repository_exception(
                std::format("Timestamp conversion error: {}", s)));
    }
    return r;
}

/**
 * @brief Generates the CREATE TABLE SQL statement for a given entity type.
 *
 * This helper creates a sqlgen CREATE TABLE IF NOT EXISTS query for the specified
 * entity type and converts it to SQL. The SQL is logged at debug level and returned.
 *
 * @tparam EntityType The database entity type for which to generate the CREATE TABLE SQL
 * @param logger_name The name to use for logging
 * @return The SQL string for creating the table
 *
 * @example
 * std::string sql = generate_create_table_sql<account_entity>(
 *     "ores.accounts.repository.account_repository");
 */
template<typename EntityType>
std::string generate_create_table_sql(utility::log::logger_t& lg) {
    using namespace ores::utility::log;
    using namespace sqlgen;

    const auto query = create_table<EntityType> | if_not_exists;
    const auto sql = postgres::to_sql(query);

    BOOST_LOG_SEV(lg, debug) << sql;

    return sql;
}

}

#endif
