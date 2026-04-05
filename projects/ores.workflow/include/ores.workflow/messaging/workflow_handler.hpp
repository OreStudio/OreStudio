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
#ifndef ORES_WORKFLOW_MESSAGING_WORKFLOW_HANDLER_HPP
#define ORES_WORKFLOW_MESSAGING_WORKFLOW_HANDLER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"

namespace ores::workflow::messaging {

/**
 * @brief NATS message handler for workflow orchestration endpoints.
 *
 * Handles inbound workflow requests, creates workflow instance records,
 * drives the appropriate executor, and replies with the outcome.
 *
 * The handler uses @p signer to validate the caller's JWT and extract
 * the per-request database context (tenant, actor, roles).  It uses
 * @p outbound_nats (already authenticated as the workflow service) with
 * delegation set to the caller's JWT to forward identity to downstream
 * services.
 */
class workflow_handler {
private:
    inline static std::string_view logger_name =
        "ores.workflow.messaging.workflow_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    workflow_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer,
        ores::nats::service::nats_client outbound_nats);

    /**
     * @brief Handles workflow.v1.parties.provision requests.
     */
    void provision_parties(ores::nats::message msg);

    /**
     * @brief Handles workflow.v1.reports.run fire-and-forget messages.
     *
     * Receives a run_report_message published by the reporting service after
     * a report instance is created. Creates a workflow_instance record and
     * drives run_report_workflow through the ORE execution lifecycle.
     * No reply is sent (fire-and-forget).
     */
    void run_report(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
    ores::nats::service::nats_client outbound_nats_;
};

}

#endif
