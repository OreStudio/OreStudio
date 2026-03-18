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

#include <array>
#include <libpq-fe.h>
#include <stdexcept>
#include <sstream>
#include <string_view>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::database::repository {

using namespace ores::logging;

namespace {

auto& bitemporal_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.utility.database.postgres");
    return instance;
}

/**
 * @brief libpq notice processor that routes NOTICE/WARNING messages to
 * Boost.Log instead of stdout. The arg parameter is unused (we use a
 * module-level logger).
 */
void pg_notice_to_log(void* /*arg*/, const char* msg) {
    using namespace ores::logging;
    std::string_view sv(msg);
    if (!sv.empty() && sv.back() == '\n')
        sv.remove_suffix(1);
    BOOST_LOG_SEV(bitemporal_lg(), info) << sv;
}

/**
 * @brief RAII wrapper for PGconn to ensure proper cleanup.
 *
 * Also installs the Boost.Log notice processor so that RAISE NOTICE /
 * RAISE WARNING messages from PL/pgSQL functions are captured by the
 * logging framework rather than printed to stdout.
 */
struct pg_connection_guard {
    PGconn* conn;
    explicit pg_connection_guard(PGconn* c) : conn(c) {
        if (conn) PQsetNoticeProcessor(conn, &pg_notice_to_log, nullptr);
    }
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
 * @brief Executes a single SET_CONFIG SQL on a raw connection.
 *
 * Extracted to avoid repeating the PQexec + error-check pattern.
 */
void set_pg_config(PGconn* conn, const std::string& key,
    const std::string& value, logging::logger_t& lg) {
    const std::string sql =
        "SELECT set_config('" + key + "', '" + value + "', false)";
    pg_result_guard r(PQexec(conn, sql.c_str()));
    if (PQresultStatus(r.result) != PGRES_TUPLES_OK) {
        const std::string err = PQerrorMessage(conn);
        BOOST_LOG_SEV(lg, error) << "Failed to set " << key << ": " << err;
        throw std::runtime_error("Failed to set " + key + ": " + err);
    }
}

/**
 * @brief Sets the tenant context on a connection for RLS policies.
 *
 * This mirrors what tenant_aware_pool does when acquiring a connection.
 * Must be called after connecting but before executing queries.
 */
void set_tenant_context(PGconn* conn,
    const utility::uuid::tenant_id& tenant_id,
    logging::logger_t& lg) {

    const auto tenant_id_str = tenant_id.to_string();

    // Use parameterized query to prevent SQL injection
    std::array<const char*, 1> param_values = {tenant_id_str.c_str()};
    std::array<int, 1> param_lengths = {static_cast<int>(tenant_id_str.length())};
    std::array<int, 1> param_formats = {0}; // text format

    const std::string sql = "SELECT set_config('app.current_tenant_id', $1, false)";

    pg_result_guard result(PQexecParams(conn, sql.c_str(), 1, nullptr,
        param_values.data(), param_lengths.data(), param_formats.data(), 0));
    if (PQresultStatus(result.result) != PGRES_TUPLES_OK) {
        const std::string err_msg = PQerrorMessage(conn);
        BOOST_LOG_SEV(lg, error) << "Failed to set tenant context: " << err_msg;
        throw std::runtime_error("Failed to set tenant context: " + err_msg);
    }

    BOOST_LOG_SEV(lg, debug) << "Set tenant context to: " << tenant_id_str;
}

/**
 * @brief Sets the full session context on a raw connection, mirroring
 * tenant_aware_pool::acquire(). Sets tenant, party, visible_party_ids,
 * and actor from the context object.
 */
void set_full_context(PGconn* conn, const context& ctx, logging::logger_t& lg) {
    set_tenant_context(conn, ctx.tenant_id(), lg);

    if (ctx.party_id().has_value()) {
        set_pg_config(conn, "app.current_party_id",
            boost::uuids::to_string(*ctx.party_id()), lg);
    }

    const auto& vpids = ctx.visible_party_ids();
    if (!vpids.empty()) {
        std::string ids = "{";
        for (std::size_t i = 0; i < vpids.size(); ++i) {
            if (i > 0) ids += ",";
            ids += boost::uuids::to_string(vpids[i]);
        }
        ids += "}";
        set_pg_config(conn, "app.visible_party_ids", ids, lg);
    }

    const auto& actor = ctx.actor();
    if (!actor.empty()) {
        set_pg_config(conn, "app.current_actor", actor, lg);
    }
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

    // Set full session context for RLS policies (tenant + party + actor)
    set_full_context(conn_guard.conn, ctx, lg);

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

    // Set full session context for RLS policies (tenant + party + actor)
    set_full_context(conn_guard.conn, ctx, lg);

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

    // Set full session context for RLS policies (tenant + party + actor)
    set_full_context(conn_guard.conn, ctx, lg);

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

void execute_raw_command(context ctx, const std::string& sql,
    logging::logger_t& lg, const std::string& operation_desc) {

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". SQL: " << sql;

    // Create direct libpq connection using credentials from context
    const auto conn_str = build_connection_string(ctx.credentials());
    pg_connection_guard conn_guard(PQconnectdb(conn_str.c_str()));

    if (PQstatus(conn_guard.conn) != CONNECTION_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Connection failed: " << err_msg;
        throw std::runtime_error("Database connection failed: " + err_msg);
    }

    // Set full session context for RLS policies (tenant + party + actor)
    set_full_context(conn_guard.conn, ctx, lg);

    // Begin transaction
    pg_result_guard begin_guard(PQexec(conn_guard.conn, "BEGIN"));
    if (PQresultStatus(begin_guard.result) != PGRES_COMMAND_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "BEGIN failed: " << err_msg;
        throw std::runtime_error("Transaction begin failed: " + err_msg);
    }

    // Execute the command
    pg_result_guard result_guard(PQexec(conn_guard.conn, sql.c_str()));

    if (PQresultStatus(result_guard.result) != PGRES_COMMAND_OK &&
        PQresultStatus(result_guard.result) != PGRES_TUPLES_OK) {
        // PQresultErrorMessage(nullptr) returns "" when PQexecParams returns
        // NULL. Fall back to PQerrorMessage(conn) for connection-level errors.
        // Must capture BEFORE rollback, which clears the connection error state.
        const std::string err_msg = (result_guard.result != nullptr)
            ? std::string(PQresultErrorMessage(result_guard.result))
            : std::string(PQerrorMessage(conn_guard.conn));
        pg_result_guard rollback_guard(PQexec(conn_guard.conn, "ROLLBACK"));
        BOOST_LOG_SEV(lg, error) << "Command failed: " << err_msg;
        throw std::runtime_error("Command execution failed: " + err_msg);
    }

    // Commit transaction
    pg_result_guard commit_guard(PQexec(conn_guard.conn, "COMMIT"));
    if (PQresultStatus(commit_guard.result) != PGRES_COMMAND_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "COMMIT failed: " << err_msg;
        throw std::runtime_error("Transaction commit failed: " + err_msg);
    }

    BOOST_LOG_SEV(lg, debug) << "Finished " << operation_desc << ".";
}

std::vector<std::string> execute_parameterized_string_query(context ctx,
    const std::string& sql, const std::vector<std::string>& params,
    logging::logger_t& lg, const std::string& operation_desc) {

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". SQL: " << sql
                             << " with " << params.size() << " parameters";

    std::vector<std::string> result;

    // Create direct libpq connection using credentials from context
    const auto conn_str = build_connection_string(ctx.credentials());
    pg_connection_guard conn_guard(PQconnectdb(conn_str.c_str()));

    if (PQstatus(conn_guard.conn) != CONNECTION_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Connection failed: " << err_msg;
        throw std::runtime_error("Database connection failed: " + err_msg);
    }

    // Set full session context for RLS policies (tenant + party + actor)
    set_full_context(conn_guard.conn, ctx, lg);

    // Build parameter arrays for PQexecParams
    std::vector<const char*> param_values;
    param_values.reserve(params.size());
    for (const auto& p : params) {
        param_values.push_back(p.c_str());
    }

    // Execute the parameterized query
    pg_result_guard result_guard(PQexecParams(
        conn_guard.conn,
        sql.c_str(),
        static_cast<int>(params.size()),
        nullptr,  // Let the server infer parameter types
        param_values.data(),
        nullptr,  // Parameter lengths (null-terminated strings)
        nullptr,  // Parameter formats (text)
        0         // Result format (text)
    ));

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

std::vector<std::vector<std::optional<std::string>>> execute_raw_multi_column_query(
    const sqlgen::postgres::Credentials& creds,
    const std::string& sql, logging::logger_t& lg,
    const std::string& operation_desc) {

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". SQL: " << sql;

    std::vector<std::vector<std::optional<std::string>>> result;

    // Create direct libpq connection using the provided credentials.
    // No tenant context is set — this is for administrative databases (e.g. postgres).
    const auto conn_str = build_connection_string(creds);
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

std::vector<std::string> execute_parameterized_string_query(
    const sqlgen::postgres::Credentials& creds,
    const std::string& sql, const std::vector<std::string>& params,
    logging::logger_t& lg, const std::string& operation_desc) {

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". SQL: " << sql
                             << " with " << params.size() << " parameters";

    std::vector<std::string> result;

    // Create direct libpq connection using the provided credentials.
    // No tenant context is set — this is for administrative databases (e.g. postgres).
    const auto conn_str = build_connection_string(creds);
    pg_connection_guard conn_guard(PQconnectdb(conn_str.c_str()));

    if (PQstatus(conn_guard.conn) != CONNECTION_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Connection failed: " << err_msg;
        throw std::runtime_error("Database connection failed: " + err_msg);
    }

    // Build parameter arrays for PQexecParams
    std::vector<const char*> param_values;
    param_values.reserve(params.size());
    for (const auto& p : params) {
        param_values.push_back(p.c_str());
    }

    // Execute the parameterized query
    pg_result_guard result_guard(PQexecParams(
        conn_guard.conn,
        sql.c_str(),
        static_cast<int>(params.size()),
        nullptr,  // Let the server infer parameter types
        param_values.data(),
        nullptr,  // Parameter lengths (null-terminated strings)
        nullptr,  // Parameter formats (text)
        0         // Result format (text)
    ));

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

void execute_parameterized_command(context ctx, const std::string& sql,
    const std::vector<std::string>& params, logging::logger_t& lg,
    const std::string& operation_desc) {

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". SQL: " << sql
                             << " with " << params.size() << " parameters";

    // Create direct libpq connection using credentials from context
    const auto conn_str = build_connection_string(ctx.credentials());
    pg_connection_guard conn_guard(PQconnectdb(conn_str.c_str()));

    if (PQstatus(conn_guard.conn) != CONNECTION_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Connection failed: " << err_msg;
        throw std::runtime_error("Database connection failed: " + err_msg);
    }

    // Set full session context for RLS policies (tenant + party + actor)
    set_full_context(conn_guard.conn, ctx, lg);

    // Build parameter arrays for PQexecParams
    std::vector<const char*> param_values;
    param_values.reserve(params.size());
    for (const auto& p : params) {
        param_values.push_back(p.c_str());
    }

    // Begin transaction
    pg_result_guard begin_guard(PQexec(conn_guard.conn, "BEGIN"));
    if (PQresultStatus(begin_guard.result) != PGRES_COMMAND_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "BEGIN failed: " << err_msg;
        throw std::runtime_error("Transaction begin failed: " + err_msg);
    }

    // Execute the parameterized command
    pg_result_guard result_guard(PQexecParams(
        conn_guard.conn,
        sql.c_str(),
        static_cast<int>(params.size()),
        nullptr,  // Let the server infer parameter types
        param_values.data(),
        nullptr,  // Parameter lengths (null-terminated strings)
        nullptr,  // Parameter formats (text)
        0         // Result format (text)
    ));

    if (PQresultStatus(result_guard.result) != PGRES_COMMAND_OK &&
        PQresultStatus(result_guard.result) != PGRES_TUPLES_OK) {
        // PQresultErrorMessage(nullptr) returns "" when PQexecParams returns
        // NULL. Fall back to PQerrorMessage(conn) for connection-level errors.
        // Must capture BEFORE rollback, which clears the connection error state.
        const std::string err_msg = (result_guard.result != nullptr)
            ? std::string(PQresultErrorMessage(result_guard.result))
            : std::string(PQerrorMessage(conn_guard.conn));
        pg_result_guard rollback_guard(PQexec(conn_guard.conn, "ROLLBACK"));
        BOOST_LOG_SEV(lg, error) << "Command failed: " << err_msg;
        throw std::runtime_error("Command execution failed: " + err_msg);
    }

    // Commit transaction
    pg_result_guard commit_guard(PQexec(conn_guard.conn, "COMMIT"));
    if (PQresultStatus(commit_guard.result) != PGRES_COMMAND_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "COMMIT failed: " << err_msg;
        throw std::runtime_error("Transaction commit failed: " + err_msg);
    }

    BOOST_LOG_SEV(lg, debug) << "Finished " << operation_desc << ".";
}

std::vector<std::vector<std::optional<std::string>>> execute_parameterized_multi_column_query(
    context ctx, const std::string& sql, const std::vector<std::string>& params,
    logging::logger_t& lg, const std::string& operation_desc) {

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". SQL: " << sql
                             << " with " << params.size() << " parameters";

    std::vector<std::vector<std::optional<std::string>>> result;

    const auto conn_str = build_connection_string(ctx.credentials());
    pg_connection_guard conn_guard(PQconnectdb(conn_str.c_str()));

    if (PQstatus(conn_guard.conn) != CONNECTION_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Connection failed: " << err_msg;
        throw std::runtime_error("Database connection failed: " + err_msg);
    }

    set_full_context(conn_guard.conn, ctx, lg);

    std::vector<const char*> param_values;
    param_values.reserve(params.size());
    for (const auto& p : params) {
        param_values.push_back(p.c_str());
    }

    pg_result_guard result_guard(PQexecParams(
        conn_guard.conn,
        sql.c_str(),
        static_cast<int>(params.size()),
        nullptr,
        param_values.data(),
        nullptr,
        nullptr,
        0
    ));

    if (PQresultStatus(result_guard.result) != PGRES_TUPLES_OK) {
        const std::string err_msg = PQerrorMessage(conn_guard.conn);
        BOOST_LOG_SEV(lg, error) << "Query failed: " << err_msg;
        throw std::runtime_error("Query execution failed: " + err_msg);
    }

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
