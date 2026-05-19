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
 * Workspace table is bitemporal: all queries filter on
 * valid_to = ores_utility_infinity_timestamp_fn().
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
     * @brief Returns all active workspaces ordered by name.
     */
    std::vector<domain::workspace> list_active();

    /**
     * @brief Finds a workspace by its UUID.
     */
    std::optional<domain::workspace> find_by_id(const std::string& id);

    /**
     * @brief Inserts a new workspace and returns its UUID string.
     *
     * The caller must set ws.id to a new gen_random_uuid(); the DB trigger
     * manages version, valid_from, and valid_to.
     */
    std::string create(const domain::workspace& ws);

    /**
     * @brief Archives the workspace identified by id.
     *
     * Uses the bitemporal delete rule (closes valid_to = now()).
     */
    void archive(const std::string& id, const std::string& modified_by,
        const std::string& change_reason_code,
        const std::string& change_commentary);

    /**
     * @brief Soft-deletes the workspace: clears trade scope then closes the
     * bitemporal record (valid_to = now() via DELETE rule).
     */
    void remove(const std::string& id);

    /**
     * @brief Returns the UUID ancestor chain for a workspace.
     */
    std::vector<std::string> resolution_order(const std::string& workspace_id);

    /**
     * @brief Replaces the trade scope for a workspace.
     */
    void set_trade_scope(const std::string& workspace_id,
        const std::vector<boost::uuids::uuid>& trade_ids);

    /**
     * @brief Removes all trade scope entries for a workspace.
     */
    void clear_trade_scope(const std::string& workspace_id);

private:
    context ctx_;
};

}

#endif
