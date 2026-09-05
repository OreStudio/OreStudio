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
#ifndef ORES_COMPUTE_CORE_SERVICE_APP_VERSION_SERVICE_HPP
#define ORES_COMPUTE_CORE_SERVICE_APP_VERSION_SERVICE_HPP

#include "ores.compute.api/domain/app_version.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/app_version_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::service {

/**
 * @brief Service for managing app versions.
 *
 * Provides a higher-level interface for app version operations,
 * wrapping the underlying repository.
 */
class ORES_COMPUTE_CORE_EXPORT app_version_service {
private:
    inline static std::string_view logger_name = "ores.compute.service.app_version_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a app_version_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit app_version_service(context ctx);

    /**
     * @brief Lists app versions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of app versions for the requested page.
     */
    std::vector<domain::app_version> list_app_versions(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active app versions.
     *
     * @return Total number of active app versions.
     */
    std::uint32_t count_app_versions();


    /**
     * @brief Retrieves a single app version as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The app version at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::app_version> get_app_version_at_version(const std::string& id,
                                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single app version by its primary key.
     *
     * @return The app version if found, std::nullopt otherwise.
     */
    std::optional<domain::app_version> get_app_version(const std::string& id);

    /**
     * @brief Saves a app version (creates or updates).
     *
     * @param app_version The app version to save.
     * @throws std::exception on failure.
     */
    void save_app_version(const domain::app_version& app_version);

    /**
     * @brief Saves a batch of app versions.
     *
     * @param app_versions The app versions to save.
     * @throws std::exception on failure.
     */
    void save_app_versions(const std::vector<domain::app_version>& app_versions);

    /**
     * @brief Deletes a app version by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_app_version(const std::string& id);

    /**
     * @brief Deletes app versions by their primary keys.
     */
    void delete_app_versions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a app version.
     */
    std::vector<domain::app_version> get_app_version_history(const std::string& id);

private:
    context ctx_;
    repository::app_version_repository repo_;
};

}

#endif
