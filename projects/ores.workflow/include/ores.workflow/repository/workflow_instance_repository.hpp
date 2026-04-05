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
#ifndef ORES_WORKFLOW_REPOSITORY_WORKFLOW_INSTANCE_REPOSITORY_HPP
#define ORES_WORKFLOW_REPOSITORY_WORKFLOW_INSTANCE_REPOSITORY_HPP

#include <string>
#include <vector>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.workflow/domain/workflow_instance.hpp"

namespace ores::workflow::repository {

/**
 * @brief Repository for workflow instances (non-temporal, append-mostly).
 */
class workflow_instance_repository {
private:
    inline static std::string_view logger_name =
        "ores.workflow.repository.workflow_instance_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    std::vector<domain::workflow_instance> read(context ctx);

    /**
     * @brief Finds a workflow instance by primary key (no tenant filter).
     *
     * Used by the engine which operates cross-tenant as the service account.
     * Returns nullopt if no record with @p id exists.
     */
    std::optional<domain::workflow_instance>
    find_by_id(context ctx, const boost::uuids::uuid& id);

    /**
     * @brief Returns all workflow instances with state_id == @p state_id.
     *
     * Used at startup to recover in-progress workflows across all tenants.
     */
    std::vector<domain::workflow_instance>
    find_by_state(context ctx, const boost::uuids::uuid& state_id);

    /**
     * @brief Inserts a new workflow instance record.
     */
    void create(context ctx, const domain::workflow_instance& v);

    /**
     * @brief Updates the FSM state (and optional result/error) of a workflow instance.
     *
     * Sets @p state_id, @p result_json, @p error, and stamps completed_at to now.
     */
    void update_state(context ctx, const boost::uuids::uuid& id,
        const boost::uuids::uuid& state_id,
        const std::string& result_json,
        const std::string& error);

    /**
     * @brief Advances the current step index and records the event timestamp.
     *
     * Called by the engine after successfully dispatching the next step command.
     * Sets current_step_index = @p next_index and last_event_at = now().
     */
    void update_step_progress(context ctx, const boost::uuids::uuid& id,
        int next_index);
};

}

#endif
