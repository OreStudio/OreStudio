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
#ifndef ORES_DATABASE_REPOSITORY_DOCUMENT_OPERATIONS_HPP
#define ORES_DATABASE_REPOSITORY_DOCUMENT_OPERATIONS_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/domain/party_scope.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <format>
#include <stdexcept>
#include <string_view>
#include <vector>

/**
 * @file document_operations.hpp
 * @brief Helpers for a component that stores a document across its tables.
 */
namespace ores::database::repository {

/**
 * @brief Stamps the session's party on every row of a value, when the session
 * has one; otherwise the rows keep the party the caller gave them.
 */
template <typename T>
void stamp_party(const database::context& ctx, T& v) {
    if (const auto party = ctx.party_id())
        domain::assign_party(v, *party);
}

/**
 * @brief The latest rows the session sees that satisfy @p keep.
 */
template <typename Repository, typename Keep>
auto read_where(const database::context& ctx, Repository repo, Keep keep) {
    auto rows = repo.read_latest(ctx);
    std::erase_if(rows, [&](const auto& r) { return !keep(r); });
    return rows;
}

/**
 * @brief The latest row with @p id, or an error naming what was missing.
 */
template <typename Repository>
auto read_one(const database::context& ctx,
              Repository repo,
              std::string_view what,
              const boost::uuids::uuid& id) {
    auto rows = repo.read_latest(ctx, boost::uuids::to_string(id));
    if (rows.empty())
        throw std::invalid_argument(std::format(
            "No {} with id {} is visible to the session.", what, boost::uuids::to_string(id)));
    return rows.front();
}

}

#endif
