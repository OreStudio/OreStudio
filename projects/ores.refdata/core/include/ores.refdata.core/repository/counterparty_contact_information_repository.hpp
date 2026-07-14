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
#ifndef ORES_REFDATA_CORE_REPOSITORY_COUNTERPARTY_CONTACT_INFORMATION_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_COUNTERPARTY_CONTACT_INFORMATION_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/counterparty_contact_information.hpp"
#include "ores.refdata.core/export.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes counterparty contact informations to data storage.
 */
class ORES_REFDATA_CORE_EXPORT counterparty_contact_information_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.counterparty_contact_information_repository";

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
     * @brief Writes counterparty contact informations to database.
     */
    /**@{*/
    void write(context ctx, const domain::counterparty_contact_information& v);
    void write(context ctx, const std::vector<domain::counterparty_contact_information>& v);
    /**@}*/

    /**
     * @brief Reads latest counterparty contact informations, possibly filtered by id.
     */
    /**@{*/
    std::vector<domain::counterparty_contact_information> read_latest(context ctx);
    std::vector<domain::counterparty_contact_information> read_latest(context ctx,
                                                                      const std::string& id);
    /**@}*/

    /**
     * @brief Reads all counterparty contact informations, possibly filtered by id.
     */
    std::vector<domain::counterparty_contact_information> read_all(context ctx,
                                                                   const std::string& id);

    /**
     * @brief Reads a single counterparty contact information as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param id The id to look up
     * @param version The version to fetch
     */
    std::optional<domain::counterparty_contact_information>
    read_at_version(context ctx, const std::string& id, std::uint32_t version);

    /**
     * @brief Reads latest counterparty contact informations filtered by counterparty_id, with
     * pagination.
     * @param ctx Repository context with database connection
     * @param counterparty_id The counterparty_id to filter by
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::counterparty_contact_information> read_latest_by_counterparty_id(
        context ctx, const std::string& counterparty_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty contact informations filtered by
     * counterparty_id.
     */
    std::uint32_t get_total_counterparty_contact_information_count_by_counterparty_id(
        context ctx, const std::string& counterparty_id);

    /**
     * @brief Reads counterparty contact informations filtered by counterparty_id that were live at
     * any point during [valid_from_bound, valid_to_bound) — i.e. the set of
     * counterparty contact informations that compose a parent entity's state as of one of
     * its own historical versions. See the "Temporal composite entity
     * versioning" architecture doc.
     * @param ctx Repository context with database connection
     * @param counterparty_id The counterparty_id to filter by
     * @param valid_from_bound The parent version's own valid_from
     * @param valid_to_bound The parent version's own valid_to
     */
    std::vector<domain::counterparty_contact_information>
    read_by_counterparty_id_as_of(context ctx,
                                  const std::string& counterparty_id,
                                  std::chrono::system_clock::time_point valid_from_bound,
                                  std::chrono::system_clock::time_point valid_to_bound);
    /**
     * @brief Reads latest counterparty contact informations with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::counterparty_contact_information>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty contact informations.
     * @param ctx Repository context with database connection
     * @return Total number of active counterparty contact informations
     */
    std::uint32_t get_total_counterparty_contact_information_count(context ctx);

    /**
     * @brief Deletes a counterparty contact information by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes counterparty contact informations by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);
};

}

#endif
