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
#ifndef ORES_WORKFLOW_DOMAIN_WORKFLOW_STEP_HPP
#define ORES_WORKFLOW_DOMAIN_WORKFLOW_STEP_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::workflow::domain {

/**
 * @brief A single step within a workflow instance.
 *
 * Records the execution of one step in a saga workflow, including the step
 * index, name, status, request/response payloads, and timing. Steps reference
 * their parent workflow instance via workflow_id.
 */
struct workflow_step final {
    /**
     * @brief UUID primary key for the workflow step.
     */
    boost::uuids::uuid id;

    /**
     * @brief FK reference to the parent workflow instance.
     */
    boost::uuids::uuid workflow_id;

    /**
     * @brief Zero-based ordinal position of this step within the workflow.
     */
    int step_index;

    /**
     * @brief Human-readable step name, e.g. 'create_account'.
     */
    std::string name;

    /**
     * @brief Step status: pending, in_progress, completed, failed, compensated.
     */
    std::string status;

    /**
     * @brief Serialised JSON payload sent to the downstream service for this step.
     */
    std::string request_json;

    /**
     * @brief Serialised JSON response received from the downstream service.
     */
    std::string response_json;

    /**
     * @brief Human-readable error message if the step failed.
     */
    std::string error;

    /**
     * @brief Timestamp when the step began executing.
     */
    std::optional<std::chrono::system_clock::time_point> started_at;

    /**
     * @brief Timestamp when the step reached a terminal state.
     */
    std::optional<std::chrono::system_clock::time_point> completed_at;

    /**
     * @brief Timestamp when this record was created.
     */
    std::chrono::system_clock::time_point created_at;
};

}

#endif
