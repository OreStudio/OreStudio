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
#ifndef ORES_IAM_CORE_SERVICE_TENANT_STATUS_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_TENANT_STATUS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/tenant_status.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/tenant_status_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing tenant statuses.
 *
 * Provides a higher-level interface for tenant status operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT tenant_status_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.tenant_status_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenant_status_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenant_status_service(context ctx);

    /**
     * @brief Lists tenant statuses with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenant statuses for the requested page.
     */
    std::vector<domain::tenant_status> list_statuses(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenant statuses.
     *
     * @return Total number of active tenant statuses.
     */
    std::uint32_t count_statuses();


    /**
     * @brief Retrieves a single tenant status as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The tenant status at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenant_status> get_status_at_version(const std::string& status,
                                                               std::uint32_t version);

    /**
     * @brief Retrieves a single tenant status by its primary key.
     *
     * @return The tenant status if found, std::nullopt otherwise.
     */
    std::optional<domain::tenant_status> find_status(const std::string& status);

    /**
     * @brief Saves a tenant status (creates or updates).
     *
     * @param status The tenant status to save.
     * @throws std::exception on failure.
     */
    void save_status(const domain::tenant_status& status);

    /**
     * @brief Saves a batch of tenant statuses.
     *
     * @param statuses The tenant statuses to save.
     * @throws std::exception on failure.
     */
    void save_statuses(const std::vector<domain::tenant_status>& statuses);

    /**
     * @brief Deletes a tenant status by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_status(const std::string& status);

    /**
     * @brief Deletes tenant statuses by their primary keys.
     */
    void delete_statuses(const std::vector<std::string>& statuss);

    /**
     * @brief Retrieves all historical versions of a tenant status.
     */
    std::vector<domain::tenant_status> get_status_history(const std::string& status);

private:
    context ctx_;
    repository::tenant_status_repository repo_;
};

}

#endif
