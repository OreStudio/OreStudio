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

#include <map>
#include <vector>
#include <iterator>
#include <sqlgen/postgres.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/helpers.hpp"

namespace ores::database::repository {

/**
 * @brief Executes a read query and maps the results to domain objects.
 *
 * This helper encapsulates the common pattern of:
 * 1. Executing a query on a session
 * 2. Ensuring success
 * 3. Logging the result count
 * 4. Mapping entities to domain objects
 *
 * @tparam EntityType The database entity type
 * @tparam DomainType The domain model type
 * @tparam QueryType The sqlgen query type
 * @tparam MapperFunc The mapper function type
 * @param ctx The repository context
 * @param query The pre-built sqlgen query
 * @param mapper The function to map entities to domain objects
 * @param logger_name The name to use for logging
 * @param operation_desc Description of the operation for logging
 * @return A vector of domain objects
 *
 * @example
 * const auto query = sqlgen::read<std::vector<account_entity>> |
 *     where("valid_to"_c == max.value()) |
 *     order_by("valid_from"_c.desc());
 * return execute_read_query<account_entity, domain::account>(
 *     ctx_, query,
 *     [](const auto& entities) { return account_mapper::map(entities); },
 *     "ores.accounts.repository.account_repository",
 *     "Read latest accounts");
 */
template<typename EntityType, typename DomainType, typename QueryType, typename MapperFunc>
std::vector<DomainType> execute_read_query(context ctx, const QueryType& query,
    MapperFunc&& mapper, utility::log::logger_t& lg, const std::string& operation_desc) {

    using namespace ores::utility::log;
    using namespace sqlgen;

    BOOST_LOG_SEV(lg, debug) << operation_desc << ".";

    const auto r = session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg);

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". Total: " << r->size();
    return std::forward<MapperFunc>(mapper)(*r);
}

/**
 * @brief Executes a write operation within a transaction.
 *
 * This helper encapsulates the common pattern of:
 * 1. Starting a session
 * 2. Beginning a transaction
 * 3. Executing an insert/update query
 * 4. Committing the transaction
 * 5. Ensuring success
 *
 * @tparam EntityType The database entity type
 * @param ctx The repository context
 * @param entity The entity or vector of entities to write
 * @param logger_name The name to use for logging
 * @param operation_desc Description of the operation for logging
 *
 * @example
 * execute_write_query(ctx_,
 *     account_mapper::map(account),
 *     "ores.accounts.repository.account_repository",
 *     "Writing account to database");
 */
template<typename EntityType>
void execute_write_query(context ctx, const EntityType& entity,
    utility::log::logger_t& lg, const std::string& operation_desc) {

    using namespace ores::utility::log;
    using namespace sqlgen;

    BOOST_LOG_SEV(lg, debug) << operation_desc << ".";

    const auto r = session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(entity))
        .and_then(commit);
    ensure_success(r, lg);

    BOOST_LOG_SEV(lg, debug) << "Finished " << operation_desc << ".";
}

/**
 * @brief Executes a delete operation within a transaction.
 *
 * For bitemporal tables, this typically triggers a database function that sets
 * valid_to = current_timestamp instead of actually deleting the record.
 *
 * @tparam QueryType The sqlgen delete query type
 * @param ctx The repository context
 * @param query The pre-built delete query
 * @param logger_name The name to use for logging
 * @param operation_desc Description of the operation for logging
 *
 * @example
 * const auto query = sqlgen::delete_from<account_entity> |
 *     where("id"_c == id_str);
 * execute_delete_query(ctx_, query,
 *     "ores.accounts.repository.account_repository",
 *     "Removing account from database");
 */
template<typename QueryType>
void execute_delete_query(context ctx, const QueryType& query,
    utility::log::logger_t& lg, const std::string& operation_desc) {

    using namespace ores::utility::log;
    using namespace sqlgen;

    BOOST_LOG_SEV(lg, debug) << operation_desc << ".";

    const auto r = session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(query)
        .and_then(commit);
    ensure_success(r, lg);

    BOOST_LOG_SEV(lg, debug) << "Finished " << operation_desc << ".";
}

/**
 * @brief Executes a raw SQL query that returns a single column of strings.
 *
 * This helper is useful for complex queries with JOINs that cannot be expressed
 * through sqlgen's ORM interface.
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
inline std::vector<std::string> execute_raw_string_query(context ctx,
    const std::string& sql, utility::log::logger_t& lg,
    const std::string& operation_desc) {

    using namespace ores::utility::log;
    using namespace sqlgen;

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". SQL: " << sql;

    std::vector<std::string> result;

    const auto execute_query = [&](auto&& session) {
        using std::begin;
        using std::end;
        auto query_result = session->execute(sql);
        for (const auto& row : query_result) {
            if (!row.empty() && !row[0].is_null()) {
                result.push_back(row[0].template as<std::string>());
            }
        }
        return query_result;
    };

    const auto r = session(ctx.connection_pool()).and_then(execute_query);
    ensure_success(r, lg);

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". Total: " << result.size();
    return result;
}

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
inline std::map<std::string, std::vector<std::string>> execute_raw_grouped_query(
    context ctx, const std::string& sql, utility::log::logger_t& lg,
    const std::string& operation_desc) {

    using namespace ores::utility::log;
    using namespace sqlgen;

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". SQL: " << sql;

    std::map<std::string, std::vector<std::string>> result;

    const auto execute_query = [&](auto&& session) {
        using std::begin;
        using std::end;
        auto query_result = session->execute(sql);
        for (const auto& row : query_result) {
            if (row.size() >= 2 && !row[0].is_null() && !row[1].is_null()) {
                result[row[0].template as<std::string>()].push_back(
                    row[1].template as<std::string>());
            }
        }
        return query_result;
    };

    const auto r = session(ctx.connection_pool()).and_then(execute_query);
    ensure_success(r, lg);

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". Total keys: " << result.size();
    return result;
}

}

#endif
