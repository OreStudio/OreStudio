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
#ifndef ORES_WORKSPACE_DOMAIN_WORKSPACE_HPP
#define ORES_WORKSPACE_DOMAIN_WORKSPACE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>

namespace ores::workspace::domain {

/**
 * @brief Named, isolated data context for workspace-level operations.
 *
 * Workspaces provide Docker-layer-style inheritance: data not present in a
 * workspace is resolved from the parent chain up to the Live workspace
 * (ores_utility_live_workspace_id_fn()).
 */
struct workspace final {
    /**
     * @brief Tenant that owns this workspace.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Party within the tenant that owns this workspace.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief UUID primary key.
     *
     * Live workspace uses the sentinel aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique workspace name.
     */
    std::string name;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    /**
     * @brief Optional path to source data.
     */
    std::string source_path;

    /**
     * @brief Optional parent workspace UUID for inheritance chain.
     */
    std::optional<boost::uuids::uuid> parent_workspace_id;

    /**
     * @brief Optional portfolio UUID to scope this workspace.
     */
    std::optional<boost::uuids::uuid> scope_portfolio_id;

    /**
     * @brief UUID of the IAM account that owns this workspace.
     */
    boost::uuids::uuid owner_id;

    /**
     * @brief Lifecycle status: active or archived.
     */
    std::string status_code;

    /**
     * @brief Username of the person who last modified this workspace.
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
