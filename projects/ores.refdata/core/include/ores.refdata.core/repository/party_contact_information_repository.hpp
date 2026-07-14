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
#ifndef ORES_REFDATA_CORE_REPOSITORY_PARTY_CONTACT_INFORMATION_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_PARTY_CONTACT_INFORMATION_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_contact_information.hpp"
#include "ores.refdata.core/export.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes party contact informations to data storage.
 */
class ORES_REFDATA_CORE_EXPORT party_contact_information_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.party_contact_information_repository";

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
     * @brief Writes party contact informations to database.
     */
    /**@{*/
    void write(context ctx, const domain::party_contact_information& v);
    void write(context ctx, const std::vector<domain::party_contact_information>& v);
    /**@}*/

    /**
     * @brief Reads latest party contact informations, possibly filtered by id.
     */
    /**@{*/
    std::vector<domain::party_contact_information> read_latest(context ctx);
    std::vector<domain::party_contact_information> read_latest(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Reads all party contact informations, possibly filtered by id.
     */
    std::vector<domain::party_contact_information> read_all(context ctx, const std::string& id);

    /**
     * @brief Reads a single party contact information as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param id The id to look up
     * @param version The version to fetch
     */
    std::optional<domain::party_contact_information>
    read_at_version(context ctx, const std::string& id, std::uint32_t version);

    /**
     * @brief Reads latest party contact informations filtered by party_id, with pagination.
     * @param ctx Repository context with database connection
     * @param party_id The party_id to filter by
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::party_contact_information> read_latest_by_party_id(
        context ctx, const std::string& party_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party contact informations filtered by party_id.
     */
    std::uint32_t
    get_total_party_contact_information_count_by_party_id(context ctx, const std::string& party_id);

    /**
     * @brief Reads party contact informations filtered by party_id that were live at
     * any point during [valid_from_bound, valid_to_bound) — i.e. the set of
     * party contact informations that compose a parent entity's state as of one of
     * its own historical versions. See the "Temporal composite entity
     * versioning" architecture doc.
     * @param ctx Repository context with database connection
     * @param party_id The party_id to filter by
     * @param valid_from_bound The parent version's own valid_from
     * @param valid_to_bound The parent version's own valid_to
     */
    std::vector<domain::party_contact_information>
    read_by_party_id_as_of(context ctx,
                           const std::string& party_id,
                           std::chrono::system_clock::time_point valid_from_bound,
                           std::chrono::system_clock::time_point valid_to_bound);
    /**
     * @brief Reads latest party contact informations with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::party_contact_information>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party contact informations.
     * @param ctx Repository context with database connection
     * @return Total number of active party contact informations
     */
    std::uint32_t get_total_party_contact_information_count(context ctx);

    /**
     * @brief Deletes a party contact information by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes party contact informations by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);
};

}

#endif
