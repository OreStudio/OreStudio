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
#ifndef ORES_WORKSPACE_CORE_SERVICE_WORKSPACE_SERVICE_HPP
#define ORES_WORKSPACE_CORE_SERVICE_WORKSPACE_SERVICE_HPP

#include <optional>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.workspace.api/domain/workspace.hpp"
#include "ores.workspace.core/repository/workspace_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.workspace.core/export.hpp"

namespace ores::workspace::service {

/**
 * @brief Service for managing workspaces.
 *
 * Provides a higher-level interface wrapping the repository with validation
 * and business rules.
 */
class ORES_WORKSPACE_CORE_EXPORT workspace_service {
private:
    inline static std::string_view logger_name =
        "ores.workspace.core.service.workspace_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit workspace_service(context ctx);

    /**
     * @brief Returns all active workspaces.
     */
    std::vector<domain::workspace> list_workspaces();

    /**
     * @brief Returns a workspace by UUID string, or nullopt if not found.
     */
    std::optional<domain::workspace> get_workspace(const std::string& id);

    /**
     * @brief Creates a new workspace and returns its UUID string.
     *
     * @throws std::invalid_argument if name is empty or status_code is invalid.
     */
    std::string create_workspace(const domain::workspace& ws);

    /**
     * @brief Archives a workspace by UUID string.
     */
    void archive_workspace(const std::string& id,
        const std::string& modified_by,
        const std::string& change_reason_code,
        const std::string& change_commentary);

    /**
     * @brief Soft-deletes a workspace and its associated data.
     *
     * Clears the trade scope then closes the bitemporal record.
     * @throws std::invalid_argument if the workspace is the Live workspace.
     */
    void remove_workspace(const std::string& id);

    /**
     * @brief Returns the UUID ancestor resolution chain for a workspace.
     */
    std::vector<std::string> resolve(const std::string& workspace_id);

    /**
     * @brief Replaces the trade scope for a workspace.
     */
    void set_trade_scope(const std::string& workspace_id,
        const std::vector<boost::uuids::uuid>& trade_ids);

    /**
     * @brief Clears all trade scope entries for a workspace.
     */
    void clear_trade_scope(const std::string& workspace_id);

private:
    context ctx_;
    repository::workspace_repository repo_{};
};

}

#endif
