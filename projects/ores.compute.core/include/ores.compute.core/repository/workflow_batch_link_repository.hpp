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
#ifndef ORES_COMPUTE_REPOSITORY_WORKFLOW_BATCH_LINK_REPOSITORY_HPP
#define ORES_COMPUTE_REPOSITORY_WORKFLOW_BATCH_LINK_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.compute.core/repository/workflow_batch_link_entity.hpp"

namespace ores::compute::repository {

/**
 * @brief Stores and retrieves workflow-batch async bridge records.
 *
 * Used by report_submit_handler (create) and batch_workflow_bridge
 * (find_all / remove).
 */
class workflow_batch_link_repository {
private:
    inline static std::string_view logger_name =
        "ores.compute.repository.workflow_batch_link_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Persist a new batch-to-workflow-step link.
     */
    void create(context ctx, const workflow_batch_link_entity& link);

    /**
     * @brief Return all pending links (across all tenants).
     *
     * Called by batch_workflow_bridge using the service-level context (no
     * tenant isolation needed — this is an internal service table).
     */
    std::vector<workflow_batch_link_entity> find_all(context ctx);

    /**
     * @brief Delete the link once the step_completed_event has been published.
     */
    void remove(context ctx, const std::string& batch_id);
};

}

#endif
