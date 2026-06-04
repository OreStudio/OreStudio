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
#ifndef ORES_WORKSPACE_REPOSITORY_WORKSPACE_REPOSITORY_HPP
#define ORES_WORKSPACE_REPOSITORY_WORKSPACE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.workspace.api/domain/workspace.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::workspace::repository {

/**
 * @brief Reads and writes workspaces to data storage.
 */
class workspace_repository {
private:
    inline static std::string_view logger_name = "ores.workspace.repository.workspace_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    std::string sql();

    void write(context ctx, const domain::workspace& v);
    void write(context ctx, const std::vector<domain::workspace>& v);

    std::vector<domain::workspace> read_latest(context ctx);
    std::vector<domain::workspace> read_latest(context ctx, const std::string& id);
    std::vector<domain::workspace> read_all(context ctx, const std::string& id);

    void remove(context ctx, const std::string& id);

    std::vector<domain::workspace> read_history(context ctx, const std::string& id);

    /**
     * @brief Returns all active workspaces ordered by name.
     */
    std::vector<domain::workspace> list_active(context ctx);

    /**
     * @brief Returns a workspace by UUID string, or nullopt if not found.
     */
    std::optional<domain::workspace> find_by_id(context ctx, const std::string& id);

    /**
     * @brief Archives the workspace: closes current record and inserts archived.
     */
    void archive(context ctx,
                 const std::string& id,
                 const std::string& modified_by,
                 const std::string& change_reason_code,
                 const std::string& change_commentary);

    /**
     * @brief Returns the UUID ancestor resolution chain for a workspace.
     */
    std::vector<std::string> resolution_order(context ctx, const std::string& workspace_id);

    /**
     * @brief Replaces the trade scope for a workspace.
     */
    void set_trade_scope(context ctx,
                         const std::string& workspace_id,
                         const std::vector<boost::uuids::uuid>& trade_ids);

    /**
     * @brief Removes all trade scope entries for a workspace.
     */
    void clear_trade_scope(context ctx, const std::string& workspace_id);
};

}

#endif
