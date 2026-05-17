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
#ifndef ORES_WORKSPACE_CORE_REPOSITORY_WORKSPACE_REPOSITORY_HPP
#define ORES_WORKSPACE_CORE_REPOSITORY_WORKSPACE_REPOSITORY_HPP

#include <optional>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.workspace.api/domain/workspace.hpp"
#include "ores.workspace.core/export.hpp"

namespace ores::workspace::repository {

/**
 * @brief Reads and writes workspaces to data storage.
 *
 * Uses raw SQL helpers because the workspace table is not bitemporal.
 */
class ORES_WORKSPACE_CORE_EXPORT workspace_repository {
private:
    inline static std::string_view logger_name =
        "ores.workspace.core.repository.workspace_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit workspace_repository(context ctx);

    /**
     * @brief Returns all active workspaces ordered by id.
     */
    std::vector<domain::workspace> list_active();

    /**
     * @brief Finds a workspace by its integer id.
     *
     * @return The workspace if found, std::nullopt otherwise.
     */
    std::optional<domain::workspace> find_by_id(int id);

    /**
     * @brief Inserts a new workspace and returns its generated id.
     */
    int create(const domain::workspace& ws);

    /**
     * @brief Sets status = 'archived' for the given workspace id.
     */
    void archive(int id);

    /**
     * @brief Returns the ancestor chain for a workspace (id, parent, ..., root).
     */
    std::vector<int> resolution_order(int workspace_id);

    /**
     * @brief Replaces the trade scope for a workspace.
     *
     * Deletes all existing rows for workspace_id then inserts the new set.
     */
    void set_trade_scope(int workspace_id,
        const std::vector<boost::uuids::uuid>& trade_ids);

    /**
     * @brief Removes all trade scope entries for a workspace.
     */
    void clear_trade_scope(int workspace_id);

private:
    static domain::workspace map_row(
        const std::vector<std::optional<std::string>>& row);

    context ctx_;
};

}

#endif
