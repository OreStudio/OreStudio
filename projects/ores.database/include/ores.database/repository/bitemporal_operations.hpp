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
#ifndef ORES_DATABASE_REPOSITORY_BITEMPORAL_OPERATIONS_HPP
#define ORES_DATABASE_REPOSITORY_BITEMPORAL_OPERATIONS_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/export.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <map>
#include <optional>
#include <vector>

namespace ores::database::repository {

/**
 * @brief The duration, in milliseconds, from which a read is logged as a warning.
 *
 * A read below it logs at debug level. A read at or above it names itself and its
 * duration in the service log, so a slow statement does not hide in the total of a
 * request.
 */
inline constexpr long long slow_statement_ms = 250;

/**
 * @brief Executes a raw SQL query that returns a single column of strings.
 *
 * This helper is useful for complex queries with JOINs that cannot be expressed
 * through the query builder.
 *
 * @param ctx The repository context
 * @param sql The raw SQL query string
 * @param lg The logger to use
 * @param operation_desc Description of the operation for logging
 * @return A vector of strings from the first column of the result
 *
 * @example
 * auto permissions = execute_raw_string_query(ctx_,
 *     "SELECT DISTINCT p.code FROM permissions p JOIN ...",
 *     lg(), "Reading effective permissions");
 */
ORES_DATABASE_EXPORT std::vector<std::string> execute_raw_string_query(
    context ctx, const std::string& sql, logging::logger_t& lg, const std::string& operation_desc);

/**
 * @brief Executes a raw SQL query that returns a grouped map of strings.
 *
 * This helper executes a query with two columns (key, value) and returns
 * a map where each key maps to a vector of associated values. Useful for
 * batch loading one-to-many relationships in a single query.
 *
 * @param ctx The repository context
 * @param sql The raw SQL query string (must return exactly 2 columns)
 * @param lg The logger to use
 * @param operation_desc Description of the operation for logging
 * @return A map from first column to vector of second column values
 *
 * @example
 * auto role_perms = execute_raw_grouped_query(ctx_,
 *     "SELECT rp.role_id, p.code FROM role_permissions rp JOIN ...",
 *     lg(), "Reading all role permission codes");
 */
ORES_DATABASE_EXPORT std::map<std::string, std::vector<std::string>> execute_raw_grouped_query(
    context ctx, const std::string& sql, logging::logger_t& lg, const std::string& operation_desc);

/**
 * @brief Executes a raw SQL query that returns multiple columns per row.
 *
 * This helper executes a query and returns all rows as vectors of optional
 * string values. Each inner vector represents a row, with column values
 * as optional strings (nullopt for NULL values).
 *
 * @param ctx The repository context
 * @param sql The raw SQL query string
 * @param lg The logger to use
 * @param operation_desc Description of the operation for logging
 * @return A vector of rows, where each row is a vector of optional<string>
 *
 * @example
 * auto rows = execute_raw_multi_column_query(ctx_,
 *     "SELECT id, name, description FROM roles WHERE ...",
 *     lg(), "Reading role data");
 * for (const auto& row : rows) {
 *     auto id = row[0].value_or("");
 *     auto name = row[1].value_or("");
 * }
 */
ORES_DATABASE_EXPORT std::vector<std::vector<std::optional<std::string>>>
execute_raw_multi_column_query(context ctx,
                               const std::string& sql,
                               logging::logger_t& lg,
                               const std::string& operation_desc);

/**
 * @brief Executes a raw SQL command that doesn't return results.
 *
 * This helper is useful for INSERT, UPDATE, DELETE, or calling functions
 * that don't return a result set. Uses a transaction for atomicity.
 *
 * @param ctx The repository context
 * @param sql The raw SQL command string
 * @param lg The logger to use
 * @param operation_desc Description of the operation for logging
 *
 * @example
 * execute_raw_command(ctx_,
 *     "INSERT INTO publications_tbl (dataset_id) VALUES ('...')",
 *     lg(), "Recording publication");
 */
ORES_DATABASE_EXPORT void execute_raw_command(context ctx,
                                              const std::string& sql,
                                              logging::logger_t& lg,
                                              const std::string& operation_desc);

/**
 * @brief Executes a parameterized SQL query that returns a single column of strings.
 *
 * This helper uses PQexecParams for proper SQL parameter binding, preventing
 * SQL injection. Parameters are passed separately from the query template.
 *
 * @param ctx The repository context
 * @param sql The SQL query with $1, $2, etc. placeholders
 * @param params Vector of parameter values
 * @param lg The logger to use
 * @param operation_desc Description of the operation for logging
 * @return A vector of strings from the first column of the result
 *
 * @example
 * auto results = execute_parameterized_string_query(ctx_,
 *     "SELECT ores_iam_tenant_by_code_fn($1)::text",
 *     {"system"},
 *     lg(), "Looking up tenant by code");
 */
ORES_DATABASE_EXPORT std::vector<std::string>
execute_parameterized_string_query(context ctx,
                                   const std::string& sql,
                                   const std::vector<std::string>& params,
                                   logging::logger_t& lg,
                                   const std::string& operation_desc);

/**
 * @brief Executes a parameterized SQL command that doesn't return results.
 *
 * This helper uses PQexecParams for proper SQL parameter binding, preventing
 * SQL injection. Parameters are passed separately from the query template.
 * Uses a transaction for atomicity.
 *
 * @param ctx The repository context
 * @param sql The SQL command with $1, $2, etc. placeholders
 * @param params Vector of parameter values
 * @param lg The logger to use
 * @param operation_desc Description of the operation for logging
 *
 * @example
 * execute_parameterized_command(ctx_,
 *     "SELECT ores_iam_provision_tenant_fn($1, $2, $3, $4, $5)",
 *     {"production", "tenant_code", "tenant_name", "domain", "description"},
 *     lg(), "Provisioning tenant");
 */
ORES_DATABASE_EXPORT void execute_parameterized_command(context ctx,
                                                        const std::string& sql,
                                                        const std::vector<std::string>& params,
                                                        logging::logger_t& lg,
                                                        const std::string& operation_desc);

/**
 * @brief Executes a parameterized SQL query that returns multiple columns per row.
 *
 * Like execute_raw_multi_column_query but with PQexecParams for safe parameter
 * binding. Each inner vector represents a row, with column values as optional
 * strings (nullopt for NULL values).
 *
 * A parameter that is @c std::nullopt binds as SQL NULL, so a read that narrows
 * on a value the caller may not state passes NULL rather than a placeholder
 * text that the statement then has to recognise and undo. The statement
 * compares such a parameter with @c is\ null and needs no cast-guard. A plain
 * string converts, so a parameter list states an absent value only where it has
 * one.
 *
 * @param ctx The repository context
 * @param sql The SQL query with $1, $2, etc. placeholders
 * @param params Vector of parameter values, where a nullopt is a SQL NULL
 * @param lg The logger to use
 * @param operation_desc Description of the operation for logging
 * @return A vector of rows, where each row is a vector of optional<string>
 *
 * @example
 * auto rows = execute_parameterized_multi_column_query(ctx_,
 *     "SELECT id, status FROM ores_mq_messages_tbl WHERE queue_id = $1::uuid",
 *     {queue_id},
 *     lg(), "Reading queue messages");
 *
 * @example
 * // A window bound the caller stated no value for:
 * auto rows = execute_parameterized_multi_column_query(ctx_,
 *     "SELECT id FROM t WHERE ($1::timestamptz is null or at >= $1::timestamptz)",
 *     {from_time},
 *     lg(), "Reading by a window");
 */
ORES_DATABASE_EXPORT std::vector<std::vector<std::optional<std::string>>>
execute_parameterized_multi_column_query(context ctx,
                                         const std::string& sql,
                                         const std::vector<std::optional<std::string>>& params,
                                         logging::logger_t& lg,
                                         const std::string& operation_desc);

}

#endif
