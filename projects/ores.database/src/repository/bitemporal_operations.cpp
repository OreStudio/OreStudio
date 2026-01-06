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
#include "ores.database/repository/bitemporal_operations.hpp"

#include <libpq-fe.h>
#include <stdexcept>
#include <sstream>

namespace ores::database::repository {

using namespace ores::logging;

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

} // anonymous namespace

std::vector<std::string> execute_raw_string_query(context ctx,
    const std::string& sql, logging::logger_t& lg,
    const std::string& operation_desc) {

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". SQL: " << sql;

    std::vector<std::string> result;

    // Create direct libpq connection using credentials from context
    const auto conn_str = build_connection_string(ctx.credentials());
    pg_connection_guard conn_guard(PQconnectdb(conn_str.c_str()));

    if (PQstatus(conn_guard.conn) != CONNECTION_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Connection failed: " << err_msg;
        throw std::runtime_error("Database connection failed: " + err_msg);
    }

    // Execute the query
    pg_result_guard result_guard(PQexec(conn_guard.conn, sql.c_str()));

    if (PQresultStatus(result_guard.result) != PGRES_TUPLES_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Query failed: " << err_msg;
        throw std::runtime_error("Query execution failed: " + err_msg);
    }

    // Extract results
    const int num_rows = PQntuples(result_guard.result);
    result.reserve(num_rows);

    for (int i = 0; i < num_rows; ++i) {
        if (!PQgetisnull(result_guard.result, i, 0)) {
            result.push_back(PQgetvalue(result_guard.result, i, 0));
        }
    }

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". Total: " << result.size();
    return result;
}

std::map<std::string, std::vector<std::string>> execute_raw_grouped_query(
    context ctx, const std::string& sql, logging::logger_t& lg,
    const std::string& operation_desc) {

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". SQL: " << sql;

    std::map<std::string, std::vector<std::string>> result;

    // Create direct libpq connection using credentials from context
    const auto conn_str = build_connection_string(ctx.credentials());
    pg_connection_guard conn_guard(PQconnectdb(conn_str.c_str()));

    if (PQstatus(conn_guard.conn) != CONNECTION_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Connection failed: " << err_msg;
        throw std::runtime_error("Database connection failed: " + err_msg);
    }

    // Execute the query
    pg_result_guard result_guard(PQexec(conn_guard.conn, sql.c_str()));

    if (PQresultStatus(result_guard.result) != PGRES_TUPLES_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Query failed: " << err_msg;
        throw std::runtime_error("Query execution failed: " + err_msg);
    }

    // Extract results - expecting at least 2 columns (key, value)
    const int num_rows = PQntuples(result_guard.result);
    const int num_cols = PQnfields(result_guard.result);

    if (num_cols < 2) {
        throw std::runtime_error("Expected at least 2 columns for grouped query");
    }

    for (int i = 0; i < num_rows; ++i) {
        if (!PQgetisnull(result_guard.result, i, 0) &&
            !PQgetisnull(result_guard.result, i, 1)) {
            const std::string key = PQgetvalue(result_guard.result, i, 0);
            const std::string value = PQgetvalue(result_guard.result, i, 1);
            result[key].push_back(value);
        }
    }

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". Total keys: " << result.size();
    return result;
}

std::vector<std::vector<std::optional<std::string>>> execute_raw_multi_column_query(
    context ctx, const std::string& sql, logging::logger_t& lg,
    const std::string& operation_desc) {

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". SQL: " << sql;

    std::vector<std::vector<std::optional<std::string>>> result;

    // Create direct libpq connection using credentials from context
    const auto conn_str = build_connection_string(ctx.credentials());
    pg_connection_guard conn_guard(PQconnectdb(conn_str.c_str()));

    if (PQstatus(conn_guard.conn) != CONNECTION_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Connection failed: " << err_msg;
        throw std::runtime_error("Database connection failed: " + err_msg);
    }

    // Execute the query
    pg_result_guard result_guard(PQexec(conn_guard.conn, sql.c_str()));

    if (PQresultStatus(result_guard.result) != PGRES_TUPLES_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Query failed: " << err_msg;
        throw std::runtime_error("Query execution failed: " + err_msg);
    }

    // Extract results
    const int num_rows = PQntuples(result_guard.result);
    const int num_cols = PQnfields(result_guard.result);
    result.reserve(num_rows);

    for (int i = 0; i < num_rows; ++i) {
        std::vector<std::optional<std::string>> row;
        row.reserve(num_cols);
        for (int j = 0; j < num_cols; ++j) {
            if (PQgetisnull(result_guard.result, i, j)) {
                row.push_back(std::nullopt);
            } else {
                row.push_back(std::string(PQgetvalue(result_guard.result, i, j)));
            }
        }
        result.push_back(std::move(row));
    }

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". Total rows: " << result.size();
    return result;
}

}
