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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_DATABASE_REPOSITORY_STATED_ORDER_HPP
#define ORES_DATABASE_REPOSITORY_STATED_ORDER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/make_logger.hpp"
#include <algorithm>
#include <initializer_list>
#include <sqlgen/postgres.hpp>
#include <string>
#include <utility>
#include <vector>

namespace ores::database::repository {

/**
 * @brief The order a page is read in: the ordered columns, then the key.
 *
 * The ordered columns take the stated direction. Every key column that is not
 * among them follows in ascending order, so rows that tie on the ordered
 * columns still come back in the same order on every read, and paging through
 * them neither repeats nor skips a row.
 */
inline sqlgen::dynamic::OrderBy make_order(std::initializer_list<std::string> ordered,
                                           bool descending,
                                           std::initializer_list<std::string> key) {
    sqlgen::dynamic::OrderBy r;
    for (const auto& name : ordered)
        r.columns.push_back({.column = {.name = name}, .desc = descending});
    for (const auto& name : key)
        if (std::ranges::find(ordered, name) == ordered.end())
            r.columns.push_back({.column = {.name = name}, .desc = false});
    return r;
}

/**
 * @brief Executes a read query in an order chosen at run time.
 *
 * A sqlgen query fixes its order when it is compiled, and a stated order is
 * only known when the request arrives. The query is turned into the statement
 * it stands for, the statement takes the order, and the session runs it, so
 * the conditions, the page and the mapping stay the query's own.
 */
template <typename EntityType,
          typename DomainType,
          typename Type,
          typename WhereType,
          typename OrderByType,
          typename LimitType,
          typename OffsetType,
          typename MapperFunc>
std::vector<DomainType> execute_ordered_read_query(
    context ctx,
    const sqlgen::Read<Type, WhereType, OrderByType, LimitType, OffsetType>& query,
    sqlgen::dynamic::OrderBy order,
    MapperFunc&& mapper,
    logging::logger_t& lg,
    const std::string& operation_desc) {

    using namespace ores::logging;

    BOOST_LOG_SEV(lg, debug) << operation_desc << ".";

    auto select = sqlgen::transpilation::
        read_to_select_from<EntityType, WhereType, OrderByType, LimitType, OffsetType>(
            query.where_, query.limit_, query.offset_);
    select.order_by = std::move(order);

    const auto r = sqlgen::session(ctx.connection_pool()).and_then([&](const auto& s) {
        return s->template read<Type>(select);
    });
    ensure_success(r, lg);

    BOOST_LOG_SEV(lg, debug) << operation_desc << ". Total: " << r->size();
    return std::forward<MapperFunc>(mapper)(*r);
}

}

#endif
