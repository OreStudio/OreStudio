/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_DATABASE_QUERY_BUILDER_EXECUTE_HPP
#define ORES_DATABASE_QUERY_BUILDER_EXECUTE_HPP

#include "ores.database/domain/context_transact.hpp"
#include "ores.database/export.hpp"
#include "ores.database/query_builder/query.hpp"
#include "ores.database/repository/entity_write_observer.hpp"
#include "ores.database/repository/repository_exception.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/exception/exception.hpp>
#include <boost/throw_exception.hpp>
#include <chrono>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

/*
 * How a generated repository's queries run: each call is one transaction on
 * the context's libpq pool, as each sqlgen call was one session. A statement
 * whose text is fixed is prepared once per connection; one whose text varies
 * with a list or a request's filter runs unnamed. A store error reaches the
 * caller as the repository_exception sqlgen's errors became.
 */
namespace ores::database::query_builder {

namespace detail {

/**
 * @brief The SQL with each '?' replaced by $1, $2, ... in order.
 */
ORES_DATABASE_EXPORT std::string numbered(const std::string& sql);

/**
 * @brief The prepared statement for a fixed text, one for the process.
 */
ORES_DATABASE_EXPORT const pq::text_statement& prepared(const std::string& sql);

/**
 * @brief Runs one statement in a transaction and returns its text result.
 */
ORES_DATABASE_EXPORT pq::result run(pq::transaction& tx,
                                    const std::string& sql,
                                    const std::vector<pq::param>& params,
                                    bool variadic);

[[noreturn]] ORES_DATABASE_EXPORT void refused(const pq::store_error& e, logging::logger_t& lg);

ORES_DATABASE_EXPORT void finished(logging::logger_t& lg,
                                   const std::string& what,
                                   std::size_t rows,
                                   std::chrono::steady_clock::time_point began);

template <typename Entity>
const std::string& table() {
    static const std::string t = std::string(Entity::schema) + "." + Entity::tablename;
    return t;
}

template <typename Entity>
const std::string& select_list() {
    static const std::string list = [] {
        std::string out;
        std::apply(
            [&](const auto&... b) {
                ((out += (out.empty() ? "" : ", ") + std::string(b.name)), ...);
            },
            Entity::columns().items);
        return out;
    }();
    return list;
}

template <typename Entity>
const std::string& insert_sql(bool replace) {
    static const auto build = [](bool upsert) {
        std::string names, values, keys, sets;
        std::apply(
            [&](const auto&... b) {
                (([&] {
                     const std::string n(b.name);
                     names += (names.empty() ? "" : ", ") + n;
                     values += values.empty() ? "?" : ", ?";
                     if (b.key)
                         keys += (keys.empty() ? "" : ", ") + n;
                     else
                         sets += (sets.empty() ? "" : ", ") + n + " = excluded." + n;
                 }()),
                 ...);
            },
            Entity::columns().items);
        std::string sql =
            "insert into " + table<Entity>() + " (" + names + ") values (" + values + ")";
        if (upsert)
            sql += " on conflict (" + keys + ") do update set " + sets;
        return numbered(sql);
    };
    static const std::string plain = build(false);
    static const std::string upsert = build(true);
    return replace ? upsert : plain;
}

template <typename Entity>
std::vector<pq::param> row_params(const Entity& e) {
    std::vector<pq::param> out;
    std::apply([&](const auto&... b) { (out.push_back(pq::to_text(e.*(b.member))), ...); },
               Entity::columns().items);
    return out;
}

template <typename Entity>
std::vector<Entity> entity_rows(const pq::result& r) {
    std::vector<Entity> out(static_cast<std::size_t>(r.rows()));
    for (int row = 0; row < r.rows(); ++row) {
        int column = 0;
        std::apply(
            [&](const auto&... b) {
                ((out[static_cast<std::size_t>(row)].*(b.member) =
                      pq::from_text<std::remove_cvref_t<decltype(out[0].*(b.member))>>(
                          r, row, column++)),
                 ...);
            },
            Entity::columns().items);
    }
    return out;
}

inline std::string order_sql(const order& o) {
    std::string sql;
    for (const auto& item : o) {
        sql += sql.empty() ? " order by " : ", ";
        sql += item.column;
        if (item.desc)
            sql += " desc";
    }
    return sql;
}

template <typename Entity>
condition select_sql(const std::optional<condition>& filter,
                     const order& sorting,
                     std::optional<std::uint32_t> max_rows,
                     std::optional<std::uint32_t> skip) {
    condition c{"select " + select_list<Entity>() + " from " + table<Entity>(), {}};
    if (filter) {
        c.sql += " where " + filter->sql;
        c.params = filter->params;
        c.variadic = filter->variadic;
    }
    c.sql += order_sql(sorting);
    if (max_rows) {
        c.sql += " limit ?";
        c.params.push_back(pq::to_text(*max_rows));
    }
    if (skip) {
        c.sql += " offset ?";
        c.params.push_back(pq::to_text(*skip));
    }
    return c;
}

template <typename Entity>
std::vector<Entity> select(context ctx, const condition& statement) {
    return database::transact(ctx, [&](pq::transaction& tx) {
        return entity_rows<Entity>(
            run(tx, numbered(statement.sql), statement.params, statement.variadic));
    });
}

}

/**
 * @brief Runs hand-written SQL, numbered $1.. already, with text parameters, and
 * returns every row as text, a missing value as nothing.
 */
ORES_DATABASE_EXPORT std::vector<std::vector<std::optional<std::string>>>
execute_parameterized_multi_column_query(context ctx,
                                         const std::string& sql,
                                         const std::vector<std::optional<std::string>>& params,
                                         logging::logger_t& lg,
                                         const std::string& operation_desc);

/**
 * @brief Runs hand-written SQL with text parameters and returns the first
 * column of each row.
 */
ORES_DATABASE_EXPORT std::vector<std::string>
execute_parameterized_string_query(context ctx,
                                   const std::string& sql,
                                   const std::vector<std::string>& params,
                                   logging::logger_t& lg,
                                   const std::string& operation_desc);

/**
 * @brief The same, for SQL with no parameters.
 */
ORES_DATABASE_EXPORT std::vector<std::vector<std::optional<std::string>>>
execute_raw_multi_column_query(context ctx,
                               const std::string& sql,
                               logging::logger_t& lg,
                               const std::string& operation_desc);

/**
 * @brief The text of the insert a repository writes, for its sql().
 */
template <typename Entity>
std::string generate_create_table_sql(logging::logger_t&) {
    return detail::insert_sql<Entity>(false);
}

template <typename Entity, typename Domain, typename Mapper>
std::vector<Domain> execute_read_query(context ctx,
                                       const read_query<Entity>& query,
                                       Mapper&& mapper,
                                       logging::logger_t& lg,
                                       const std::string& operation_desc) {
    using namespace ores::logging;
    BOOST_LOG_SEV(lg, debug) << operation_desc << ".";
    const auto began = std::chrono::steady_clock::now();
    try {
        const auto rows = detail::select<Entity>(
            ctx,
            detail::select_sql<Entity>(query.filter, query.sorting, query.max_rows, query.skip));
        detail::finished(lg, operation_desc, rows.size(), began);
        return std::forward<Mapper>(mapper)(rows);
    } catch (const pq::store_error& e) {
        detail::refused(e, lg);
    }
}

template <typename Entity, typename Domain, typename Mapper>
std::vector<Domain> execute_ordered_read_query(context ctx,
                                               const read_query<Entity>& query,
                                               order sorting,
                                               std::optional<condition> filter,
                                               Mapper&& mapper,
                                               logging::logger_t& lg,
                                               const std::string& operation_desc) {
    using namespace ores::logging;
    BOOST_LOG_SEV(lg, debug) << operation_desc << ".";
    const auto began = std::chrono::steady_clock::now();
    std::optional<condition> where = query.filter;
    if (filter)
        where = where ? std::move(*where) && std::move(*filter) : std::move(*filter);
    auto statement = detail::select_sql<Entity>(
        where, sorting.empty() ? query.sorting : sorting, query.max_rows, query.skip);
    statement.variadic = true;
    try {
        const auto rows = detail::select<Entity>(ctx, statement);
        detail::finished(lg, operation_desc, rows.size(), began);
        return std::forward<Mapper>(mapper)(rows);
    } catch (const pq::store_error& e) {
        detail::refused(e, lg);
    }
}

template <typename Entity>
std::uint32_t execute_count_query(context ctx,
                                  const read_query<Entity>& query,
                                  std::optional<condition> filter,
                                  logging::logger_t& lg,
                                  const std::string& operation_desc) {
    using namespace ores::logging;
    BOOST_LOG_SEV(lg, debug) << operation_desc << ".";
    std::optional<condition> where = query.filter;
    if (filter)
        where = where ? std::move(*where) && std::move(*filter) : std::move(*filter);
    condition statement{"select count(*) from " + detail::table<Entity>(), {}, true};
    if (where) {
        statement.sql += " where " + where->sql;
        statement.params = where->params;
    }
    try {
        return database::transact(ctx, [&](pq::transaction& tx) {
            const auto r = detail::run(tx, detail::numbered(statement.sql), statement.params, true);
            return static_cast<std::uint32_t>(pq::from_text<std::int64_t>(r, 0, 0));
        });
    } catch (const pq::store_error& e) {
        detail::refused(e, lg);
    }
}

template <typename Entity>
void execute_write_op(context ctx,
                      const write_query<Entity>& query,
                      logging::logger_t& lg,
                      const std::string& operation_desc) {
    static_assert(!repository::entity_write_observer<Entity>::observes,
                  "An entity with a write observer stays on sqlgen until its observer moves");
    using namespace ores::logging;
    BOOST_LOG_SEV(lg, debug) << operation_desc << ".";
    if (query.rows.empty())
        return;
    try {
        database::transact(ctx, [&](pq::transaction& tx) {
            const auto& statement = detail::prepared(detail::insert_sql<Entity>(query.replace));
            pq::transaction::batch b;
            for (const auto& row : query.rows)
                b.add(statement, detail::row_params(row));
            tx.run(b);
        });
    } catch (const pq::store_error& e) {
        detail::refused(e, lg);
    }
    BOOST_LOG_SEV(lg, debug) << "Finished " << operation_desc << ".";
}

template <typename Entity>
void execute_write_query(context ctx,
                         const Entity& entity,
                         logging::logger_t& lg,
                         const std::string& operation_desc) {
    execute_write_op(ctx, insert(entity), lg, operation_desc);
}

template <typename Entity>
void execute_write_query(context ctx,
                         const std::vector<Entity>& entities,
                         logging::logger_t& lg,
                         const std::string& operation_desc) {
    execute_write_op(ctx, insert(entities), lg, operation_desc);
}

template <typename Entity>
void execute_delete_query(context ctx,
                          const delete_query<Entity>& query,
                          logging::logger_t& lg,
                          const std::string& operation_desc) {
    using namespace ores::logging;
    BOOST_LOG_SEV(lg, debug) << operation_desc << ".";
    condition statement{"delete from " + detail::table<Entity>(), {}};
    if (query.filter) {
        statement.sql += " where " + query.filter->sql;
        statement.params = query.filter->params;
        statement.variadic = query.filter->variadic;
    }
    try {
        database::transact(ctx, [&](pq::transaction& tx) {
            detail::run(tx, detail::numbered(statement.sql), statement.params, statement.variadic);
        });
    } catch (const pq::store_error& e) {
        detail::refused(e, lg);
    }
    BOOST_LOG_SEV(lg, debug) << "Finished " << operation_desc << ".";
}

}

#endif
