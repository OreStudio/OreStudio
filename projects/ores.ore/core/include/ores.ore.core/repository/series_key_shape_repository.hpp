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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_repository.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ORE_CORE_REPOSITORY_SERIES_KEY_SHAPE_REPOSITORY_HPP
#define ORES_ORE_CORE_REPOSITORY_SERIES_KEY_SHAPE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.api/domain/series_key_shape.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::ore::repository {

/**
 * @brief Reads and writes series key shapes to data storage.
 */
class ORES_ORE_CORE_EXPORT series_key_shape_repository {
private:
    inline static std::string_view logger_name = "ores.ore.repository.series_key_shape_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes series key shapes to database.
     *
     * The plain form replaces the row the caller last read: it states the
     * version the row carries now, so the store can tell a replace from a
     * create. A row that moved on since that read is a conflict, never a silent
     * overwrite.
     */
    /**@{*/
    void write(context ctx, const domain::series_key_shape& v);
    void write(context ctx, const std::vector<domain::series_key_shape>& v);
    /**@}*/

    /**
     * @brief Writes a series key shape, honouring the claim it states.
     *
     * The claim is the version the caller read (@c must_match_version), that no
     * current row exists (@c must_not_exist), or neither (@c any, which
     * replaces the row as it stands). The store decides in the write's own
     * transaction, so a create that collides with a live row and a write over a
     * row that moved on are refused by the store rather than by a check a
     * caller might have forgotten.
     */
    void write(context ctx,
               const domain::series_key_shape& v,
               const ores::utility::domain::precondition& claim);

    /**
     * @brief Writes a set of series key shapes, each honouring its own
     * claim, as one statement.
     */
    void write(context ctx,
               const std::vector<domain::series_key_shape>& v,
               const std::vector<ores::utility::domain::precondition>& claims);

    /**
     * @brief Reads latest series key shapes, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::series_key_shape> read_latest(context ctx);
    std::vector<domain::series_key_shape> read_latest(context ctx, const std::string& series_type);
    std::vector<domain::series_key_shape> read_latest(context ctx,
                                                      const std::vector<std::string>& series_types);
    /**@}*/


    /**
     * @brief Reads all series key shapes, possibly filtered by primary key.
     */
    std::vector<domain::series_key_shape> read_all(context ctx, const std::string& series_type);

    /**
     * @brief Reads a single series key shape as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param version The version to fetch
     */
    std::optional<domain::series_key_shape>
    read_at_version(context ctx, const std::string& series_type, std::uint32_t version);

    /**
     * @brief Reads latest series key shapes with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::series_key_shape>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active series key shapes.
     * @param ctx Repository context with database connection
     * @return Total number of active series key shapes
     */
    std::uint32_t get_total_shape_count(context ctx);

    /**
     * @brief Deletes a series key shape by closing its temporal validity.
     */
    void remove(context ctx, const std::string& series_type);

    /**
     * @brief What a removal did, so a caller reports a conflict as an outcome
     * rather than catching an exception.
     *
     * @c missing means there was no current row to remove, and @c unsupported
     * means the store cannot answer the version at all -- a current-state
     * table has no version column, so a versioned removal has no meaning
     * there.
     */
    enum class remove_status { removed, conflicting, missing, unsupported };

    /**
     * @brief Removes a series key shape, refusing a row that moved on.
     *
     * A stated version is the version the caller read. The removal is refused
     * with @c conflicting when the current row carries another, so a caller
     * that decided on stale state cannot remove a change it never saw. A null
     * version removes whatever is current, which is what a caller that stated
     * no version asked for.
     */
    remove_status
    remove(context ctx, const std::string& series_type, std::optional<std::uint32_t> version);

    /**
     * @brief Deletes series key shapes by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& series_types);


private:
    /**
     * @brief The claim a replace makes: the version the row carries now, or
     * that no row exists yet.
     */
    ores::utility::domain::precondition replace_claim(context ctx,
                                                      const domain::series_key_shape& v);

    /**
     * @brief The object with the claim's version stamped onto it.
     *
     * A claim the store cannot check is refused here rather than ignored.
     */
    domain::series_key_shape apply_claim(context ctx,
                                         const domain::series_key_shape& v,
                                         const ores::utility::domain::precondition& claim);
};

}

#endif
