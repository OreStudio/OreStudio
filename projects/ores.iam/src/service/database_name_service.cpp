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

#include <libpq-fe.h>
#include <sstream>
#include <stdexcept>

namespace ores::iam::service {

using namespace ores::utility::log;

namespace {

/**
 * @brief RAII wrapper for PGconn to ensure proper cleanup.
 */
struct pg_connection_guard {
    PGconn* conn;
    explicit pg_connection_guard(PGconn* c) : conn(c) {}
    ~pg_connection_guard() { if (conn) PQfinish(conn); }
    pg_connection_guard(const pg_connection_guard&) = delete;
    pg_connection_guard& operator=(const pg_connection_guard&) = delete;
};

/**
 * @brief RAII wrapper for PGresult to ensure proper cleanup.
 */
struct pg_result_guard {
    PGresult* result;
    explicit pg_result_guard(PGresult* r) : result(r) {}
    ~pg_result_guard() { if (result) PQclear(result); }
    pg_result_guard(const pg_result_guard&) = delete;
    pg_result_guard& operator=(const pg_result_guard&) = delete;
};

/**
 * @brief Builds connection string from credentials.
 */
std::string build_connection_string(const sqlgen::postgres::Credentials& creds) {
    std::ostringstream oss;
    oss << "host=" << creds.host
        << " port=" << creds.port
        << " dbname=" << creds.dbname
        << " user=" << creds.user
        << " password=" << creds.password;
    return oss.str();
}

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

    const auto conn_str = build_connection_string(ctx_.credentials());
    pg_connection_guard conn_guard(PQconnectdb(conn_str.c_str()));

    if (PQstatus(conn_guard.conn) != CONNECTION_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg(), error) << "Connection failed: " << err_msg;
        throw std::runtime_error("Database connection failed: " + err_msg);
    }

    pg_result_guard result_guard(PQexec(conn_guard.conn, sql.c_str()));

    if (PQresultStatus(result_guard.result) != PGRES_TUPLES_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg(), error) << "Query failed: " << err_msg;
        throw std::runtime_error("Query execution failed: " + err_msg);
    }

    const int num_rows = PQntuples(result_guard.result);
    if (num_rows == 0) {
        throw std::runtime_error("Query returned no results");
    }

    if (PQgetisnull(result_guard.result, 0, 0)) {
        throw std::runtime_error("Query returned NULL");
    }

    std::string result = PQgetvalue(result_guard.result, 0, 0);
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
