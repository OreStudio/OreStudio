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
#ifndef ORES_COMPUTE_CORE_REPOSITORY_APP_VERSION_PLATFORM_REPOSITORY_HPP
#define ORES_COMPUTE_CORE_REPOSITORY_APP_VERSION_PLATFORM_REPOSITORY_HPP

#include "ores.compute.api/domain/app_version_platform.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::compute::repository {

/**
 * @brief Reads and writes app version platforms to data storage.
 */
class ORES_COMPUTE_CORE_EXPORT app_version_platform_repository {
private:
    inline static std::string_view logger_name =
        "ores.compute.repository.app_version_platform_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit app_version_platform_repository(context ctx);

    std::string sql();

    void write(const domain::app_version_platform& app_version_platform);
    void write(const std::vector<domain::app_version_platform>& app_version_platforms);

    std::vector<domain::app_version_platform> read_latest();
    std::vector<domain::app_version_platform>
    read_latest_by_app_version(const boost::uuids::uuid& app_version_id);
    /**
     * @brief Reads latest app version platforms filtered by app_version_id, with pagination.
     */
    std::vector<domain::app_version_platform> read_latest_by_app_version(
        const boost::uuids::uuid& app_version_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active app version platforms filtered by app_version_id.
     */
    std::uint32_t
    get_total_app_version_platform_count_by_app_version(const boost::uuids::uuid& app_version_id);
    std::vector<domain::app_version_platform>
    read_latest_by_platform(const boost::uuids::uuid& platform_id);

    void remove(const boost::uuids::uuid& app_version_id, const boost::uuids::uuid& platform_id);
    void remove_by_app_version(const boost::uuids::uuid& app_version_id);
    /**
     * @brief Replaces the active app version platforms for a app version.
     *
     * Soft-closes the currently active rows for the given
     * app version and inserts the rows in @p app_version_platforms,
     * so the active set exactly matches the caller's list.
     */
    void
    replace_by_app_version(const boost::uuids::uuid& app_version_id,
                           const std::vector<domain::app_version_platform>& app_version_platforms,
                           const std::string& modified_by,
                           const std::string& performed_by,
                           const std::string& change_reason_code,
                           const std::string& change_commentary);

private:
    context ctx_;
};

}

#endif
