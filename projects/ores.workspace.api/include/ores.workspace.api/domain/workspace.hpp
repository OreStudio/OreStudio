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
#ifndef ORES_WORKSPACE_API_DOMAIN_WORKSPACE_HPP
#define ORES_WORKSPACE_API_DOMAIN_WORKSPACE_HPP

#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::workspace::domain {

/**
 * @brief A named, isolated data context for workspace-level operations.
 *
 * Workspaces provide Docker-layer-style inheritance: data not present in a
 * workspace is resolved from the parent chain up to workspace 0 (Live).
 */
struct workspace final {
    /**
     * @brief Auto-generated integer primary key.
     */
    int id = 0;

    /**
     * @brief Unique name of the workspace.
     */
    std::string name;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    /**
     * @brief Path to the source data for this workspace.
     */
    std::string source_path;

    /**
     * @brief Optional parent workspace for inheritance chain.
     */
    std::optional<int> parent_workspace_id;

    /**
     * @brief Optional portfolio UUID to scope this workspace to.
     *
     * Soft FK to the portfolios table in refdata; no hard DB constraint.
     */
    std::optional<boost::uuids::uuid> scope_portfolio_id;

    /**
     * @brief Username that created this workspace.
     */
    std::string created_by;

    /**
     * @brief Lifecycle status: "active" or "archived".
     */
    std::string status;
};

}

#endif
