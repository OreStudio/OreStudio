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
#ifndef ORES_TRADING_CORE_REPOSITORY_STRUCTURE_IDENTIFIER_REPOSITORY_HPP
#define ORES_TRADING_CORE_REPOSITORY_STRUCTURE_IDENTIFIER_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/structure_identifier.hpp"
#include "ores.trading.api/messaging/structure_identifier_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::repository {

/**
 * @brief Reads and writes structure identifiers to data storage.
 */
class ORES_TRADING_CORE_EXPORT structure_identifier_repository {
private:
    inline static std::string_view logger_name =
        "ores.trading.repository.structure_identifier_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the insert statement the repository writes.
     */
    std::string sql();

    /**
     * @brief Writes structure identifiers to database.
     *
     * The plain form replaces the row the caller last read: it states the
     * version the row carries now, so the store can tell a replace from a
     * create. A row that moved on since that read is a conflict, never a silent
     * overwrite.
     */
    /**@{*/
    void write(context ctx, const domain::structure_identifier& v);
    void write(context ctx, const std::vector<domain::structure_identifier>& v);
    /**@}*/

    /**
     * @brief Writes a structure identifier, honouring the claim it states.
     *
     * The claim is the version the caller read (@c must_match_version), that no
     * current row exists (@c must_not_exist), or neither (@c any, which
     * replaces the row as it stands). The store decides in the write's own
     * transaction, so a create that collides with a live row and a write over a
     * row that moved on are refused by the store rather than by a check a
     * caller might have forgotten.
     */
    void write(context ctx,
               const domain::structure_identifier& v,
               const ores::utility::domain::precondition& claim);

    /**
     * @brief Writes a set of structure identifiers, each honouring its own
     * claim, as one statement.
     */
    void write(context ctx,
               const std::vector<domain::structure_identifier>& v,
               const std::vector<ores::utility::domain::precondition>& claims);

    /**
     * @brief Reads latest structure identifiers, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::structure_identifier> read_latest(context ctx);
    std::vector<domain::structure_identifier>
    read_latest(context ctx, const std::string& structure_id, const std::string& id_type);
    std::vector<domain::structure_identifier>
    read_latest(context ctx,
                const std::vector<std::string>& structure_ids,
                const std::vector<std::string>& id_types);
    /**@}*/


    /**
     * @brief Reads all structure identifiers, possibly filtered by primary key.
     */
    std::vector<domain::structure_identifier>
    read_all(context ctx, const std::string& structure_id, const std::string& id_type);

    /**
     * @brief Reads a single structure identifier as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param version The version to fetch
     */
    std::optional<domain::structure_identifier> read_at_version(context ctx,
                                                                const std::string& structure_id,
                                                                const std::string& id_type,
                                                                std::uint32_t version);

    /**
     * @brief Reads latest structure identifiers filtered by structure_id, with pagination.
     * @param ctx Repository context with database connection
     * @param structure_id The structure_id to filter by
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @param order The stated order; an empty field is the default order
     */
    std::vector<domain::structure_identifier> read_latest_by_structure_id(
        context ctx,
        const std::string& structure_id,
        std::uint32_t offset,
        std::uint32_t limit,
        const ores::utility::domain::order& order = {},
        const std::optional<messaging::structure_identifiers_filter>& filter = std::nullopt);

    /**
     * @brief Gets the total count of active structure identifiers filtered by structure_id.
     */
    std::uint32_t get_total_identifier_count_by_structure_id(
        context ctx,
        const std::string& structure_id,
        const std::optional<messaging::structure_identifiers_filter>& filter = std::nullopt);


    /**
     * @brief Whether a list of structure identifiers can be ordered by a field.
     *
     * The model's :sortable: columns, and nothing else.
     */
    static bool is_sortable(std::string_view field);

    /**
     * @brief Reads latest structure identifiers with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @param order The stated order; an empty field is the default order
     * @param filter The filter record; the members it sets must all hold
     * @throws std::invalid_argument if the field is not sortable
     */
    std::vector<domain::structure_identifier>
    read_latest(context ctx,
                std::uint32_t offset,
                std::uint32_t limit,
                const ores::utility::domain::order& order = {},
                const std::optional<messaging::structure_identifiers_filter>& filter = std::nullopt,
                const std::optional<std::string>& as_of = std::nullopt);

    /**
     * @brief Gets the total count of active structure identifiers.
     * @param ctx Repository context with database connection
     * @return Total number of active structure identifiers
     */
    std::uint32_t get_total_identifier_count(
        context ctx,
        const std::optional<messaging::structure_identifiers_filter>& filter = std::nullopt,
        const std::optional<std::string>& as_of = std::nullopt);

    /**
     * @brief Deletes a structure identifier by closing its temporal validity.
     */
    void remove(context ctx, const std::string& structure_id, const std::string& id_type);

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
     * @brief Removes a structure identifier, refusing a row that moved on.
     *
     * A stated version is the version the caller read. The removal is refused
     * with @c conflicting when the current row carries another, so a caller
     * that decided on stale state cannot remove a change it never saw. A null
     * version removes whatever is current, which is what a caller that stated
     * no version asked for.
     */
    remove_status remove(context ctx,
                         const std::string& structure_id,
                         const std::string& id_type,
                         std::optional<std::uint32_t> version);

    /**
     * @brief Deletes structure identifiers by closing their temporal validity.
     */
    void remove(context ctx,
                const std::vector<std::string>& structure_ids,
                const std::vector<std::string>& id_types);


private:
    /**
     * @brief The claim a replace makes: the version the row carries now, or
     * that no row exists yet.
     */
    ores::utility::domain::precondition replace_claim(context ctx,
                                                      const domain::structure_identifier& v);

    /**
     * @brief The object with the claim's version stamped onto it.
     *
     * A claim the store cannot check is refused here rather than ignored.
     */
    domain::structure_identifier apply_claim(context ctx,
                                             const domain::structure_identifier& v,
                                             const ores::utility::domain::precondition& claim);
};

}

#endif
