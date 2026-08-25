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
#ifndef ORES_COMPUTE_CORE_SERVICE_HOST_SERVICE_HPP
#define ORES_COMPUTE_CORE_SERVICE_HOST_SERVICE_HPP

#include "ores.compute.api/domain/host.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/host_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::service {

/**
 * @brief Service for managing compute hosts.
 *
 * Provides a higher-level interface for compute host operations,
 * wrapping the underlying repository.
 */
class ORES_COMPUTE_CORE_EXPORT host_service {
private:
    inline static std::string_view logger_name = "ores.compute.service.host_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a host_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit host_service(context ctx);

    /**
     * @brief Lists compute hosts with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of compute hosts for the requested page.
     */
    std::vector<domain::host> list_hosts(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active compute hosts.
     *
     * @return Total number of active compute hosts.
     */
    std::uint32_t count_hosts();


    /**
     * @brief Retrieves a single compute host as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The compute host at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::host> get_host_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single compute host by its primary key.
     *
     * @return The compute host if found, std::nullopt otherwise.
     */
    std::optional<domain::host> get_host(const std::string& id);

    /**
     * @brief Saves a compute host (creates or updates).
     *
     * @param host The compute host to save.
     * @throws std::exception on failure.
     */
    void save_host(const domain::host& host);

    /**
     * @brief Saves a batch of compute hosts.
     *
     * @param hosts The compute hosts to save.
     * @throws std::exception on failure.
     */
    void save_hosts(const std::vector<domain::host>& hosts);

    /**
     * @brief Deletes a compute host by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_host(const std::string& id);

    /**
     * @brief Deletes compute hosts by their primary keys.
     */
    void delete_hosts(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a compute host.
     */
    std::vector<domain::host> get_host_history(const std::string& id);

private:
    context ctx_;
    repository::host_repository repo_;
};

}

#endif
