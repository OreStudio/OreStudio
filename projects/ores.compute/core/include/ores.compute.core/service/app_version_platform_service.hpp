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
#ifndef ORES_COMPUTE_SERVICE_APP_VERSION_PLATFORM_SERVICE_HPP
#define ORES_COMPUTE_SERVICE_APP_VERSION_PLATFORM_SERVICE_HPP

#include "ores.compute.api/domain/app_version_platform.hpp"
#include "ores.compute.core/repository/app_version_platform_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

namespace ores::compute::service {

/**
 * @brief Service for managing app version platforms.
 *
 * This service provides functionality for:
 * - Managing app version platforms (CRUD operations)
 */
class app_version_platform_service {
private:
    inline static std::string_view logger_name =
        "ores.compute.service.app_version_platform_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a app_version_platform_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit app_version_platform_service(context ctx);

    /**
     * @brief Lists all app version platforms.
     */
    std::vector<domain::app_version_platform> list_app_version_platforms();

    /**
     * @brief Lists app version platforms for a specific app version.
     *
     * @param app_version_id The app version to filter by
     */
    std::vector<domain::app_version_platform>
    list_app_version_platforms_by_app_version(const boost::uuids::uuid& app_version_id);

    /**
     * @brief Lists app version platforms for a specific app version, with pagination.
     */
    std::vector<domain::app_version_platform> list_app_version_platforms_by_app_version(
        const boost::uuids::uuid& app_version_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active app version platforms filtered by app_version_id.
     */
    std::uint32_t
    get_total_app_version_platform_count_by_app_version(const boost::uuids::uuid& app_version_id);

    /**
     * @brief Saves a app version platform (creates or updates).
     *
     * @param app_version_platform The app version platform to save
     */
    void save_app_version_platform(const domain::app_version_platform& app_version_platform);

    /**
     * @brief Removes a app version platform.
     *
     * @param app_version_id The app version
     * @param platform_id The platform
     */
    void remove_app_version_platform(const boost::uuids::uuid& app_version_id,
                                     const boost::uuids::uuid& platform_id);

    /**
     * @brief Replaces the app version platforms for a app version.
     *
     * Soft-closes the currently active rows for the given
     * app version and inserts the rows in @p app_version_platforms,
     * so the active set exactly matches the caller's list.
     *
     * @param app_version_id The app version
     * @param app_version_platforms The rows that make up the new active set
     */
    void replace_app_version_platforms_by_app_version(
        const boost::uuids::uuid& app_version_id,
        const std::vector<domain::app_version_platform>& app_version_platforms,
        const std::string& modified_by,
        const std::string& performed_by,
        const std::string& change_reason_code,
        const std::string& change_commentary);

private:
    repository::app_version_platform_repository repo_;
};

}

#endif
