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
#ifndef ORES_IAM_CORE_SERVICE_TENANT_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_TENANT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/tenant.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing tenants.
 *
 * Provides a higher-level interface for tenant operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT tenant_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.tenant_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenant_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenant_service(context ctx);

    /**
     * @brief Lists tenants with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenants for the requested page.
     */
    std::vector<domain::tenant> list_tenants(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenants.
     *
     * @return Total number of active tenants.
     */
    std::uint32_t count_tenants();


    /**
     * @brief Retrieves a single tenant as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The tenant at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenant> get_tenant_at_version(const std::string& id,
                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single tenant by its primary key.
     *
     * @return The tenant if found, std::nullopt otherwise.
     */
    std::optional<domain::tenant> get_tenant(const std::string& id);

    /**
     * @brief Saves a tenant (creates or updates).
     *
     * @param tenant The tenant to save.
     * @throws std::exception on failure.
     */
    void save_tenant(const domain::tenant& tenant);

    /**
     * @brief Saves a batch of tenants.
     *
     * @param tenants The tenants to save.
     * @throws std::exception on failure.
     */
    void save_tenants(const std::vector<domain::tenant>& tenants);

    /**
     * @brief Deletes a tenant by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_tenant(const std::string& id);

    /**
     * @brief Deletes tenants by their primary keys.
     */
    void delete_tenants(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a tenant.
     */
    std::vector<domain::tenant> get_tenant_history(const std::string& id);

private:
    context ctx_;
    repository::tenant_repository repo_;
};

}

#endif
