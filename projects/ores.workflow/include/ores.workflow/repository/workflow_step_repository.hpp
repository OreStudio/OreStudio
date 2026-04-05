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
#ifndef ORES_WORKFLOW_REPOSITORY_WORKFLOW_STEP_REPOSITORY_HPP
#define ORES_WORKFLOW_REPOSITORY_WORKFLOW_STEP_REPOSITORY_HPP

#include <string>
#include <vector>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.workflow/domain/workflow_step.hpp"

namespace ores::workflow::repository {

/**
 * @brief Repository for workflow steps (non-temporal, append-mostly).
 */
class workflow_step_repository {
private:
    inline static std::string_view logger_name =
        "ores.workflow.repository.workflow_step_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    std::vector<domain::workflow_step> read(context ctx);

    /**
     * @brief Finds a workflow step by primary key (no tenant filter).
     *
     * Used by the engine which processes step-completed events cross-tenant.
     * Returns nullopt if no record with @p id exists.
     */
    std::optional<domain::workflow_step>
    find_by_id(context ctx, const boost::uuids::uuid& id);

    /**
     * @brief Returns all steps belonging to a workflow instance.
     *
     * Results are returned in step_index ascending order.
     */
    std::vector<domain::workflow_step>
    find_by_workflow_id(context ctx, const boost::uuids::uuid& workflow_id);

    /**
     * @brief Inserts a new workflow step record.
     */
    void create(context ctx, const domain::workflow_step& v);

    /**
     * @brief Updates the FSM state (and optional response/error) of a workflow step.
     *
     * Sets @p state_id, @p response_json, @p error, and stamps completed_at to now.
     */
    void update_state(context ctx, const boost::uuids::uuid& id,
        const boost::uuids::uuid& state_id,
        const std::string& response_json,
        const std::string& error);

    /**
     * @brief Records that the step command was successfully published.
     *
     * Sets command_published_at = now(). Called after nats.publish() succeeds
     * so a restart can detect unpublished commands and re-dispatch them.
     */
    void mark_command_published(context ctx, const boost::uuids::uuid& id);
};

}

#endif
