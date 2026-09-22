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
#ifndef ORES_REFDATA_CORE_REPOSITORY_PARTY_IDENTIFIER_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_PARTY_IDENTIFIER_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_identifier.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes party identifiers to data storage.
 */
class ORES_REFDATA_CORE_EXPORT party_identifier_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.party_identifier_repository";

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
     * @brief Writes party identifiers to database.
     *
     * The plain form replaces the row the caller last read: it states the
     * version the row carries now, so the store can tell a replace from a
     * create. A row that moved on since that read is a conflict, never a silent
     * overwrite.
     */
    /**@{*/
    void write(context ctx, const domain::party_identifier& v);
    void write(context ctx, const std::vector<domain::party_identifier>& v);
    /**@}*/

    /**
     * @brief Writes a party identifier, honouring the claim it states.
     *
     * The claim is the version the caller read (@c must_match_version), that no
     * current row exists (@c must_not_exist), or neither (@c any, which
     * replaces the row as it stands). The store decides in the write's own
     * transaction, so a create that collides with a live row and a write over a
     * row that moved on are refused by the store rather than by a check a
     * caller might have forgotten.
     */
    void write(context ctx,
               const domain::party_identifier& v,
               const ores::utility::domain::precondition& claim);

    /**
     * @brief Writes a set of party identifiers, each honouring its own
     * claim, as one statement.
     */
    void write(context ctx,
               const std::vector<domain::party_identifier>& v,
               const std::vector<ores::utility::domain::precondition>& claims);

    /**
     * @brief Reads latest party identifiers, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::party_identifier> read_latest(context ctx);
    std::vector<domain::party_identifier> read_latest(context ctx, const std::string& id);
    std::vector<domain::party_identifier> read_latest(context ctx,
                                                      const std::vector<std::string>& ids);
    /**@}*/

    /**
     * @brief Reads latest party identifiers filtered by party_id and id_scheme.
     */
    std::vector<domain::party_identifier>
    read_latest_by_code(context ctx, const std::string& party_id, const std::string& id_scheme);

    /**
     * @brief Reads latest party identifiers filtered by id_value.
     */
    std::vector<domain::party_identifier> read_latest_by_id_value(context ctx,
                                                                  const std::string& id_value);

    /**
     * @brief Reads the newest party identifiers filtered by id_value, current or not.
     *
     * History is addressed by the key the model declares and must stay readable
     * after a delete, which closes the transaction-time window rather than
     * removing the row. A latest read cannot resolve a closed row, so this one
     * ignores the window and takes the newest match.
     */
    std::vector<domain::party_identifier> read_any_by_id_value(context ctx,
                                                               const std::string& id_value);


    /**
     * @brief Reads all party identifiers, possibly filtered by primary key.
     */
    std::vector<domain::party_identifier> read_all(context ctx, const std::string& id);

    /**
     * @brief Reads a single party identifier as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param version The version to fetch
     */
    std::optional<domain::party_identifier>
    read_at_version(context ctx, const std::string& id, std::uint32_t version);


    /**
     * @brief Reads latest party identifiers filtered by party_id, with pagination.
     * @param ctx Repository context with database connection
     * @param party_id The party_id to filter by
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::party_identifier> read_latest_by_party_id(context ctx,
                                                                  const std::string& party_id,
                                                                  std::uint32_t offset,
                                                                  std::uint32_t limit);

    /**
     * @brief Gets the total count of active party identifiers filtered by party_id.
     */
    std::uint32_t get_total_party_identifier_count_by_party_id(context ctx,
                                                               const std::string& party_id);


    /**
     * @brief Reads party identifiers filtered by party_id that were live at
     * any point during [valid_from_bound, valid_to_bound) — i.e. the set of
     * party identifiers that compose a parent entity's state as of one of
     * its own historical versions. See the "Temporal composite entity
     * versioning" architecture doc.
     * @param ctx Repository context with database connection
     * @param party_id The party_id to filter by
     * @param valid_from_bound The parent version's own valid_from
     * @param valid_to_bound The parent version's own valid_to
     */
    std::vector<domain::party_identifier>
    read_by_party_id_as_of(context ctx,
                           const std::string& party_id,
                           std::chrono::system_clock::time_point valid_from_bound,
                           std::chrono::system_clock::time_point valid_to_bound);

    /**
     * @brief Reads latest party identifiers with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::party_identifier>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party identifiers.
     * @param ctx Repository context with database connection
     * @return Total number of active party identifiers
     */
    std::uint32_t get_total_party_identifier_count(context ctx);

    /**
     * @brief Deletes a party identifier by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

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
     * @brief Removes a party identifier, refusing a row that moved on.
     *
     * A stated version is the version the caller read. The removal is refused
     * with @c conflicting when the current row carries another, so a caller
     * that decided on stale state cannot remove a change it never saw. A null
     * version removes whatever is current, which is what a caller that stated
     * no version asked for.
     */
    remove_status remove(context ctx, const std::string& id, std::optional<std::uint32_t> version);

    /**
     * @brief Deletes party identifiers by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);


private:
    /**
     * @brief The claim a replace makes: the version the row carries now, or
     * that no row exists yet.
     */
    ores::utility::domain::precondition replace_claim(context ctx,
                                                      const domain::party_identifier& v);

    /**
     * @brief The object with the claim's version stamped onto it.
     *
     * A claim the store cannot check is refused here rather than ignored.
     */
    domain::party_identifier apply_claim(context ctx,
                                         const domain::party_identifier& v,
                                         const ores::utility::domain::precondition& claim);
};

}

#endif
