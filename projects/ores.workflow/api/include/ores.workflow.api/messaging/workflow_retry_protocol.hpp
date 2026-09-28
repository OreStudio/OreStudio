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
#ifndef ORES_WORKFLOW_API_MESSAGING_WORKFLOW_RETRY_PROTOCOL_HPP
#define ORES_WORKFLOW_API_MESSAGING_WORKFLOW_RETRY_PROTOCOL_HPP

#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <string>
#include <string_view>

namespace ores::workflow::messaging {

/**
 * @brief Asks the engine to resume a stopped run from the step that failed.
 *
 * A retry is the second half of the failure policy the workflow definition
 * declares: a failure stops the run and keeps every completed step, and a
 * retry re-dispatches the step that failed so the run can finish.
 */
struct retry_workflow_instance_request {
    using response_type = struct retry_workflow_instance_response;
    static constexpr std::string_view nats_subject = "workflow.v1.instances.retry";

    /**
     * @brief The run to resume.
     */
    std::string workflow_instance_id;

    /**
     * @brief The step to resume from, by the name the definition gave it.
     *
     * Empty means the step that failed, which is what a page that has just
     * rendered a stopped run asks for: it holds the whole step list from the
     * progress read and names one only when a person chooses to resume from
     * somewhere other than where the run stopped.
     */
    std::string step_name;
};

struct retry_workflow_instance_response {
    bool success = false;

    /**
     * @brief Why the run was not resumed, or empty when it was.
     *
     * A refusal names what it refused: a run that is not stopped, a step the
     * run does not hold, or a step whose predecessors have not all completed.
     */
    std::string message;

    /**
     * @brief The run the answer is about, echoed from the request.
     */
    std::string workflow_instance_id;

    /**
     * @brief The index of the step the engine re-dispatched, or -1 on refusal.
     */
    int step_index = -1;

    /**
     * @brief The name of the step the engine re-dispatched, or empty on refusal.
     */
    std::string step_name;
};

}

#endif
