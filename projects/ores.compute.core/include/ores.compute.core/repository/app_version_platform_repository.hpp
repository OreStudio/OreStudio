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
#ifndef ORES_COMPUTE_REPOSITORY_APP_VERSION_PLATFORM_REPOSITORY_HPP
#define ORES_COMPUTE_REPOSITORY_APP_VERSION_PLATFORM_REPOSITORY_HPP

#include <string>
#include <vector>
#include "ores.database/domain/context.hpp"
#include "ores.compute.api/domain/app_version_platform.hpp"

namespace ores::compute::repository {

/**
 * @brief Reads and writes the (app_version, platform) → package_uri junction
 * table.
 *
 * The orchestrator uses @c list_for_version at workunit dispatch time to find
 * which packaged bundles are available for an app version and pick the URI
 * matching the target host's triplet. @c replace_for_version is called from
 * the save_app_version handler to sync the junction rows for an app version
 * in one pass.
 */
class app_version_platform_repository {
public:
    /**
     * @brief Return all active junction rows for a given app version, with
     * the platform code denormalised from ores_compute_platforms_tbl.
     */
    std::vector<domain::app_version_platform>
    list_for_version(database::context ctx, const std::string& app_version_id);

    /**
     * @brief Replace all active junction rows for the given app version with
     * the supplied set. Existing active rows that are not in @p rows are
     * soft-deleted; rows in @p rows are upserted via the bitemporal insert
     * trigger which closes any pre-existing active row for the same
     * (app_version, platform) pair.
     */
    void replace_for_version(database::context ctx,
        const std::string& app_version_id,
        const std::vector<domain::app_version_platform>& rows,
        const std::string& modified_by,
        const std::string& performed_by,
        const std::string& change_reason_code,
        const std::string& change_commentary);
};

}

#endif
