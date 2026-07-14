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
#ifndef ORES_REFDATA_CORE_SERVICE_PARTY_IDENTIFIER_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PARTY_IDENTIFIER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_identifier.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/party_identifier_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party identifiers.
 *
 * Provides a higher-level interface for party identifier operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT party_identifier_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.party_identifier_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_identifier_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_identifier_service(context ctx);

    /**
     * @brief Lists party identifiers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of party identifiers for the requested page.
     */
    std::vector<domain::party_identifier> list_party_identifiers(std::uint32_t offset,
                                                                 std::uint32_t limit);

    /**
     * @brief Gets the total count of active party identifiers.
     *
     * @return Total number of active party identifiers.
     */
    std::uint32_t count_party_identifiers();

    /**
     * @brief Lists party identifiers filtered by party_id, with pagination.
     *
     * @param party_id The party_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching party identifiers for the requested page.
     */
    std::vector<domain::party_identifier> list_party_identifiers_by_party_id(
        const std::string& party_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party identifiers filtered by party_id.
     *
     * @param party_id The party_id to filter by.
     * @return Total number of matching party identifiers.
     */
    std::uint32_t count_party_identifiers_by_party_id(const std::string& party_id);

    /**
     * @brief Lists party identifiers filtered by party_id that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param party_id The party_id to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching party identifiers.
     */
    std::vector<domain::party_identifier>
    list_party_identifiers_by_party_id_as_of(const std::string& party_id,
                                             std::chrono::system_clock::time_point valid_from_bound,
                                             std::chrono::system_clock::time_point valid_to_bound);
    /**
     * @brief Retrieves a single party identifier as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param id The id of the party identifier.
     * @param version The version to fetch.
     * @return The party identifier at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::party_identifier> get_party_identifier_at_version(const std::string& id,
                                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single party identifier by its id.
     *
     * @param id The id of the party identifier.
     * @return The party identifier if found, std::nullopt otherwise.
     */
    std::optional<domain::party_identifier> get_party_identifier(const std::string& id);

    /**
     * @brief Saves a party identifier (creates or updates).
     *
     * @param party_identifier The party identifier to save.
     * @throws std::exception on failure.
     */
    void save_party_identifier(const domain::party_identifier& party_identifier);

    /**
     * @brief Saves a batch of party identifiers.
     *
     * @param party_identifiers The party identifiers to save.
     * @throws std::exception on failure.
     */
    void save_party_identifiers(const std::vector<domain::party_identifier>& party_identifiers);

    /**
     * @brief Deletes a party identifier by its id.
     *
     * @param id The id of the party identifier to delete.
     * @throws std::exception on failure.
     */
    void delete_party_identifier(const std::string& id);

    /**
     * @brief Deletes party identifiers by their ids.
     */
    void delete_party_identifiers(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a party identifier.
     */
    std::vector<domain::party_identifier> get_party_identifier_history(const std::string& id);

private:
    context ctx_;
    repository::party_identifier_repository repo_;
};

}

#endif
