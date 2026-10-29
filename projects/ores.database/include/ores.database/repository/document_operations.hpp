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
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <format>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

/**
 * @file document_operations.hpp
 * @brief Helpers for a component that stores a document across its tables.
 */
namespace ores::database::repository {

/**
 * @brief The claim a row states: the version it was read at, or none when it is new.
 *
 * A row that moved on since the read is refused by the store's version
 * management, so a stale write is a conflict rather than an overwrite.
 */
inline ores::utility::domain::precondition amendment_claim_of(int version) {
    using ores::utility::domain::precondition_kind;
    if (version == 0)
        return {precondition_kind::must_not_exist, std::nullopt};
    return {precondition_kind::must_match_version, static_cast<std::uint32_t>(version)};
}

/**
 * @brief States why the rows are being written on every row of the amendment.
 *
 * The rows take the write's reason, because the reason the document was created
 * with does not describe this version. An empty reason leaves the store's own.
 */
template <typename Row>
void state_amendment_reason(Row& row, std::string_view reason, std::string_view commentary) {
    if constexpr (requires { row.change_reason_code; }) {
        if (!reason.empty())
            row.change_reason_code = std::string(reason);
        if constexpr (requires { row.change_commentary; })
            row.change_commentary = std::string(commentary);
    }
}

/**
 * @brief Writes the rows a caller sent, and nothing else.
 *
 * A document's save names the families it changed. Writing only those leaves the
 * families the caller did not read at their versions, so their history stays a
 * record of what changed. Each row claims the version it states, so a row that
 * moved on is a conflict the store raises, never a silent overwrite.
 */
template <typename Repository, typename Row>
void write_amendment(database::context ctx,
                     Repository repository,
                     std::vector<Row> rows,
                     std::string_view reason,
                     std::string_view commentary) {
    if (rows.empty())
        return;
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(rows.size());
    for (auto& row : rows) {
        claims.push_back(amendment_claim_of(static_cast<int>(row.version)));
        state_amendment_reason(row, reason, commentary);
    }
    repository.write(ctx, rows, claims);
}

/** The latest rows the session sees that satisfy @p keep.
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

/**
 * @brief The ids of rows, as the repositories' remove takes them.
 */
template <typename Rows>
std::vector<std::string> ids_of(const Rows& rows) {
    std::vector<std::string> out;
    out.reserve(rows.size());
    for (const auto& r : rows)
        out.push_back(boost::uuids::to_string(r.id));
    return out;
}

}

#endif
