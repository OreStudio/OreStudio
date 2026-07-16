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
#ifndef ORES_REFDATA_CORE_SERVICE_PARTY_STATUS_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PARTY_STATUS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_status.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/party_status_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party statuses.
 *
 * Provides a higher-level interface for party status operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT party_status_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.party_status_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_status_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_status_service(context ctx);

    /**
     * @brief Lists party statuses with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of party statuses for the requested page.
     */
    std::vector<domain::party_status> list_statuses(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party statuses.
     *
     * @return Total number of active party statuses.
     */
    std::uint32_t count_statuses();

    /**
     * @brief Retrieves a single party status as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the party status.
     * @param version The version to fetch.
     * @return The party status at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::party_status> get_status_at_version(const std::string& code,
                                                              std::uint32_t version);

    /**
     * @brief Retrieves a single party status by its code.
     *
     * @param code The code of the party status.
     * @return The party status if found, std::nullopt otherwise.
     */
    std::optional<domain::party_status> get_status(const std::string& code);

    /**
     * @brief Saves a party status (creates or updates).
     *
     * @param status The party status to save.
     * @throws std::exception on failure.
     */
    void save_status(const domain::party_status& status);

    /**
     * @brief Saves a batch of party statuses.
     *
     * @param statuses The party statuses to save.
     * @throws std::exception on failure.
     */
    void save_statuses(const std::vector<domain::party_status>& statuses);

    /**
     * @brief Deletes a party status by its code.
     *
     * @param code The code of the party status to delete.
     * @throws std::exception on failure.
     */
    void delete_status(const std::string& code);

    /**
     * @brief Deletes party statuses by their codes.
     */
    void delete_statuses(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a party status.
     */
    std::vector<domain::party_status> get_status_history(const std::string& code);

private:
    context ctx_;
    repository::party_status_repository repo_;
};

}

#endif
