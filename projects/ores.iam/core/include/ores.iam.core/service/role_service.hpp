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
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_CORE_SERVICE_ROLE_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_ROLE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/role.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/role_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing roles.
 *
 * Provides a higher-level interface for role operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT role_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.role_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a role_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit role_service(context ctx);

    /**
     * @brief Lists roles with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of roles for the requested page.
     */
    std::vector<domain::role> list_roles(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active roles.
     *
     * @return Total number of active roles.
     */
    std::uint32_t count_roles();


    /**
     * @brief Retrieves a single role as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The role at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::role> get_role_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single role by its primary key.
     *
     * @return The role if found, std::nullopt otherwise.
     */
    std::optional<domain::role> get_role(const std::string& id);

    /**
     * @brief Saves a role (creates or updates).
     *
     * @param role The role to save.
     * @throws std::exception on failure.
     */
    void save_role(const domain::role& role);

    /**
     * @brief Saves a batch of roles.
     *
     * @param roles The roles to save.
     * @throws std::exception on failure.
     */
    void save_roles(const std::vector<domain::role>& roles);

    /**
     * @brief Deletes a role by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_role(const std::string& id);

    /**
     * @brief Deletes roles by their primary keys.
     */
    void delete_roles(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a role.
     */
    std::vector<domain::role> get_role_history(const std::string& id);

private:
    context ctx_;
    repository::role_repository repo_;
};

}

#endif
