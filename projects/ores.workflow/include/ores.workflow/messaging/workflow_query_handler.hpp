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
#ifndef ORES_WORKFLOW_MESSAGING_WORKFLOW_QUERY_HANDLER_HPP
#define ORES_WORKFLOW_MESSAGING_WORKFLOW_QUERY_HANDLER_HPP

#include <string>
#include <unordered_map>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.workflow/service/fsm_state_map.hpp"
#include "ores.workflow/repository/workflow_instance_repository.hpp"
#include "ores.workflow/repository/workflow_step_repository.hpp"

namespace ores::workflow::messaging {

/**
 * @brief NATS request/reply handler for workflow query endpoints.
 *
 * Handles:
 *  - workflow.v1.instances.list  — list workflow instances for the tenant
 *  - workflow.v1.instances.steps — list steps for a specific instance
 *
 * Both handlers validate the Bearer JWT, scope the database query to the
 * authenticated tenant via RLS, and return JSON responses.
 */
class workflow_query_handler {
private:
    inline static std::string_view logger_name =
        "ores.workflow.messaging.workflow_query_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    workflow_query_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer,
        const service::fsm_state_map& instance_states,
        const service::fsm_state_map& step_states);

    /**
     * @brief Lists workflow instances for the authenticated tenant.
     *
     * Decodes a list_workflow_instances_request from @p msg, validates the JWT,
     * queries the database filtered by tenant (via RLS context), and replies
     * with a list_workflow_instances_response.
     */
    void list_instances(ores::nats::message msg);

    /**
     * @brief Returns the steps for a specific workflow instance.
     *
     * Decodes a get_workflow_steps_request, validates the JWT, verifies the
     * instance belongs to the authenticated tenant, and replies with the step
     * list.
     */
    void get_steps(ores::nats::message msg);

private:
    /**
     * @brief Maps a state UUID to its human-readable name.
     *
     * Returns "unknown" if the UUID is not in the reverse map (e.g. the FSM
     * state was added after this service started).
     */
    std::string state_name(const boost::uuids::uuid& id) const;

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;

    // Reverse maps: state UUID → state name, built from the fsm_state_maps.
    std::unordered_map<boost::uuids::uuid, std::string,
        std::hash<boost::uuids::uuid>> instance_state_names_;
    std::unordered_map<boost::uuids::uuid, std::string,
        std::hash<boost::uuids::uuid>> step_state_names_;

    repository::workflow_instance_repository instance_repo_;
    repository::workflow_step_repository step_repo_;
};

}  // namespace ores::workflow::messaging

#endif
