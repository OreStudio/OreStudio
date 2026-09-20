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
#ifndef ORES_IAM_CORE_SERVICE_PERMISSION_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_PERMISSION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/permission.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/permission_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing permissions.
 *
 * Provides a higher-level interface for permission operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT permission_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.permission_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a permission_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit permission_service(context ctx);

    /**
     * @brief Lists permissions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of permissions for the requested page.
     */
    std::vector<domain::permission> list_permissions(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active permissions.
     *
     * @return Total number of active permissions.
     */
    std::uint32_t count_permissions();


    /**
     * @brief Retrieves a single permission by its primary key.
     *
     * @return The permission if found, std::nullopt otherwise.
     */
    std::optional<domain::permission> get_permission(const std::string& id);

    /**
     * @brief Retrieves a single permission by its code.
     *
     * @return The permission if found, std::nullopt otherwise.
     */
    std::optional<domain::permission> find_permission_by_code(const std::string& code);

    /**
     * @brief Saves a permission (creates or updates).
     *
     * @param permission The permission to save.
     * @throws std::exception on failure.
     */
    void save_permission(const domain::permission& permission);

    /**
     * @brief Saves a batch of permissions.
     *
     * @param permissions The permissions to save.
     * @throws std::exception on failure.
     */
    void save_permissions(const std::vector<domain::permission>& permissions);

    /**
     * @brief Deletes a permission by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_permission(const std::string& id);

    /**
     * @brief Deletes permissions by their primary keys.
     */
    void delete_permissions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a permission.
     */
    std::vector<domain::permission> get_permission_history(const std::string& id);

private:
    context ctx_;
    repository::permission_repository repo_;
};

}

#endif
