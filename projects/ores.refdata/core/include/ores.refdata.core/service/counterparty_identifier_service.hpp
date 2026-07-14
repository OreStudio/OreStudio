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
#ifndef ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_IDENTIFIER_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_IDENTIFIER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/counterparty_identifier.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/counterparty_identifier_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing counterparty identifiers.
 *
 * Provides a higher-level interface for counterparty identifier operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT counterparty_identifier_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.counterparty_identifier_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a counterparty_identifier_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit counterparty_identifier_service(context ctx);

    /**
     * @brief Lists counterparty identifiers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of counterparty identifiers for the requested page.
     */
    std::vector<domain::counterparty_identifier> list_counterparty_identifiers(std::uint32_t offset,
                                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty identifiers.
     *
     * @return Total number of active counterparty identifiers.
     */
    std::uint32_t count_counterparty_identifiers();

    /**
     * @brief Lists counterparty identifiers filtered by counterparty_id, with pagination.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching counterparty identifiers for the requested page.
     */
    std::vector<domain::counterparty_identifier> list_counterparty_identifiers_by_counterparty_id(
        const std::string& counterparty_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty identifiers filtered by counterparty_id.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @return Total number of matching counterparty identifiers.
     */
    std::uint32_t
    count_counterparty_identifiers_by_counterparty_id(const std::string& counterparty_id);

    /**
     * @brief Lists counterparty identifiers filtered by counterparty_id that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param counterparty_id The counterparty_id to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching counterparty identifiers.
     */
    std::vector<domain::counterparty_identifier>
    list_counterparty_identifiers_by_counterparty_id_as_of(
        const std::string& counterparty_id,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);
    /**
     * @brief Retrieves a single counterparty identifier as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param id The id of the counterparty identifier.
     * @param version The version to fetch.
     * @return The counterparty identifier at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty_identifier>
    get_counterparty_identifier_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single counterparty identifier by its id.
     *
     * @param id The id of the counterparty identifier.
     * @return The counterparty identifier if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty_identifier>
    get_counterparty_identifier(const std::string& id);

    /**
     * @brief Saves a counterparty identifier (creates or updates).
     *
     * @param counterparty_identifier The counterparty identifier to save.
     * @throws std::exception on failure.
     */
    void
    save_counterparty_identifier(const domain::counterparty_identifier& counterparty_identifier);

    /**
     * @brief Saves a batch of counterparty identifiers.
     *
     * @param counterparty_identifiers The counterparty identifiers to save.
     * @throws std::exception on failure.
     */
    void save_counterparty_identifiers(
        const std::vector<domain::counterparty_identifier>& counterparty_identifiers);

    /**
     * @brief Deletes a counterparty identifier by its id.
     *
     * @param id The id of the counterparty identifier to delete.
     * @throws std::exception on failure.
     */
    void delete_counterparty_identifier(const std::string& id);

    /**
     * @brief Deletes counterparty identifiers by their ids.
     */
    void delete_counterparty_identifiers(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a counterparty identifier.
     */
    std::vector<domain::counterparty_identifier>
    get_counterparty_identifier_history(const std::string& id);

private:
    context ctx_;
    repository::counterparty_identifier_repository repo_;
};

}

#endif
