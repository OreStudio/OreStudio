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
#include "ores.iam/service/database_name_service.hpp"

#include <sstream>
#include <stdexcept>
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::iam::service {

using namespace ores::telemetry::log;

namespace {

/**
 * @brief Builds a PostgreSQL array literal from a vector of strings.
 */
std::string build_array_literal(const std::vector<std::string>& values) {
    if (values.empty()) {
        return "ARRAY[]::TEXT[]";
    }

    std::ostringstream oss;
    oss << "ARRAY[";
    for (size_t i = 0; i < values.size(); ++i) {
        if (i > 0) oss << ", ";
        // Escape single quotes by doubling them
        std::string escaped;
        for (char c : values[i]) {
            if (c == '\'') escaped += "''";
            else escaped += c;
        }
        oss << "'" << escaped << "'";
    }
    oss << "]::TEXT[]";
    return oss.str();
}

} // anonymous namespace

database_name_service::database_name_service(database::context ctx)
    : ctx_(ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Constructed database_name_service";
}

std::string database_name_service::execute_scalar_string_query(
    const std::string& sql) {
    BOOST_LOG_SEV(lg(), debug) << "Executing scalar query: " << sql;

    auto rows = ores::database::repository::execute_raw_multi_column_query(
        ctx_, sql, lg(), "Executing scalar string query");

    if (rows.empty() || rows[0].empty() || !rows[0][0].has_value()) {
        throw std::runtime_error("Query returned no results or a NULL value.");
    }

    std::string result = *rows[0][0];
    BOOST_LOG_SEV(lg(), debug) << "Query result: " << result;
    return result;
}

std::string database_name_service::generate_whimsical_name() {
    return execute_scalar_string_query(
        "SELECT ores.generate_whimsical_name()");
}

std::string database_name_service::generate_whimsical_name(bool with_suffix) {
    const std::string sql = with_suffix
        ? "SELECT ores.generate_whimsical_name(true)"
        : "SELECT ores.generate_whimsical_name(false)";
    return execute_scalar_string_query(sql);
}

std::string database_name_service::generate_database_name(bool with_suffix) {
    const std::string sql = with_suffix
        ? "SELECT ores.generate_database_name(true)"
        : "SELECT ores.generate_database_name(false)";
    return execute_scalar_string_query(sql);
}

std::string database_name_service::generate_unique_database_name(
    const std::vector<std::string>& existing_names, int max_attempts) {

    const std::string array_literal = build_array_literal(existing_names);
    const std::string sql =
        "SELECT ores.generate_unique_database_name(" +
        array_literal + ", " + std::to_string(max_attempts) + ")";
    return execute_scalar_string_query(sql);
}

std::string database_name_service::generate_unique_database_name_from_server() {
    return execute_scalar_string_query(
        "SELECT ores.generate_unique_database_name_from_server()");
}

}
