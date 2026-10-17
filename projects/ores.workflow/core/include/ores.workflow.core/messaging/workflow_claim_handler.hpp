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
#ifndef ORES_WORKFLOW_CORE_MESSAGING_WORKFLOW_CLAIM_HANDLER_HPP
#define ORES_WORKFLOW_CORE_MESSAGING_WORKFLOW_CLAIM_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.workflow.core/export.hpp"
#include "ores.workflow.core/repository/step_claim_repository.hpp"
#include "ores.workflow.core/repository/workflow_step_repository.hpp"
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <unordered_map>

namespace ores::workflow::messaging {

/**
 * @brief Answers one step command delivery's question: may I do this step's work?
 *
 * A step command is delivered at least once, so two deliveries for one step can
 * arrive at once. Both reach the service that serves the step's subject, and
 * both ask here before doing anything. Exactly one is told it has the step, and
 * the other is told to come back later, so the work happens once.
 *
 * The answer comes from two places and in this order. The step's own state is
 * read first, because it is what refuses a replay: a delivery that arrives after
 * the work is done is answered with the state it already reached, which is a
 * no-op rather than a second attempt. Only a step still in_progress reaches the
 * claim, and there the lease decides between the deliveries that are in flight
 * together.
 */
class ORES_WORKFLOW_CORE_EXPORT workflow_claim_handler {
private:
    inline static std::string_view logger_name = "ores.workflow.messaging.workflow_claim_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    workflow_claim_handler(ores::nats::service::client& nats,
                           ores::database::context ctx,
                           const service::fsm_state_map& step_states);

    /**
     * @brief Answers a claim_step_request, on workflow.v1.ops.claim_step.
     *
     * Decodes the request, reads the step it names inside the tenant it names,
     * and replies with a claim_step_response. The delivery is answered with an
     * ack_guard on every path, so a request this handler cannot decide is
     * redelivered rather than dropped.
     */
    void claim_step(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;

    // Reverse map: state UUID -> state name, built from the fsm_state_map.
    std::unordered_map<boost::uuids::uuid, std::string, std::hash<boost::uuids::uuid>>
        step_state_names_;

    repository::step_claim_repository claim_repo_;
    repository::workflow_step_repository step_repo_;
};

}

#endif
