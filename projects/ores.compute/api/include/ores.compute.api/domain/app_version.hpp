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
#ifndef ORES_COMPUTE_DOMAIN_APP_VERSION_HPP
#define ORES_COMPUTE_DOMAIN_APP_VERSION_HPP

#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::compute::domain {

/**
 * @brief A versioned wrapper+engine bundle for a compute grid application.
 *
 * Combines a specific wrapper version with a specific engine version. The BOINC
 * equivalent of 'app_version'. Per-platform package artefacts are tracked via
 * ores_compute_app_version_platforms_tbl — each (app_version, platform) row
 * owns the URI of its own packaged bundle.
 */
struct app_version final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key for the app version.
     */
    boost::uuids::uuid id;

    /**
     * @brief FK reference to ores_compute_apps_tbl.
     */
    boost::uuids::uuid app_id;

    /**
     * @brief Version of the OreStudio wrapper binary, e.g. 'v1.2.0'.
     */
    std::string wrapper_version;

    /**
     * @brief Version of the underlying engine, e.g. 'ORE-Studio-7.1'.
     */
    std::string engine_version;

    /**
     * @brief Minimum RAM in MB required by the node to run this app version.
     */
    int min_ram_mb;

    /**
     * @brief Username of the person who last modified this app version.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
