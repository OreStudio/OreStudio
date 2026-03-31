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
#include "ores.workflow/messaging/workflow_handler.hpp"

#include <chrono>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.workflow/domain/workflow_instance.hpp"
#include "ores.workflow/messaging/workflow_protocol.hpp"
#include "ores.workflow/repository/workflow_instance_repository.hpp"
#include "ores.workflow/service/provision_parties_workflow.hpp"

namespace ores::workflow::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

workflow_handler::workflow_handler(ores::nats::service::client& nats,
    ores::database::context ctx,
    ores::security::jwt::jwt_authenticator signer,
    ores::nats::service::nats_client outbound_nats)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , signer_(std::move(signer))
    , outbound_nats_(std::move(outbound_nats)) {}

void workflow_handler::provision_parties(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "Handling provision_parties request.";

    // Validate JWT and build per-request context
    auto ctx_expected = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx_expected) {
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& req_ctx = *ctx_expected;

    if (!has_permission(req_ctx, "workflow::parties:provision")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }

    // Decode request
    auto req = decode<provision_parties_request>(msg);
    if (!req) {
        reply(nats_, msg, provision_parties_response{
            .success = false, .message = "Invalid request payload."});
        return;
    }

    if (req->parties.empty()) {
        reply(nats_, msg, provision_parties_response{
            .success = false, .message = "No parties specified."});
        return;
    }

    // Build the outbound nats_client with the caller's JWT delegated so
    // downstream services can build the correct DB context.
    const auto bearer = ores::nats::service::extract_bearer(msg);
    auto delegated_nats = outbound_nats_.with_delegation(bearer);

    // Serialize the request for audit storage
    const auto request_json = rfl::json::write(*req);

    // Create workflow instance
    domain::workflow_instance instance;
    instance.id = boost::uuids::random_generator()();
    instance.tenant_id = req_ctx.tenant_id().to_uuid();
    instance.type = "provision_parties_workflow";
    instance.status = "in_progress";
    instance.request_json = request_json;
    instance.created_by = delegated_actor(req_ctx);
    instance.created_at = std::chrono::system_clock::now();

    repository::workflow_instance_repository instance_repo;
    instance_repo.create(req_ctx, instance);

    // Run the saga executor
    service::provision_parties_workflow executor(instance.id, std::move(*req));

    bool ok = false;
    try {
        ok = executor.execute(req_ctx, delegated_nats);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Unhandled exception in workflow execution: " << e.what();
        instance_repo.update_status(req_ctx, instance.id, "failed", "", e.what());
        reply(nats_, msg, provision_parties_response{
            .success = false, .message = e.what()});
        return;
    }

    if (!ok) {
        BOOST_LOG_SEV(lg(), warn) << "provision_parties_workflow failed: "
                                     << executor.failure_reason()
                                     << ". Starting compensation.";

        instance_repo.update_status(req_ctx, instance.id,
            "compensating", "", executor.failure_reason());

        try {
            executor.compensate(req_ctx, delegated_nats);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Unhandled exception in workflow compensation: " << e.what();
        }

        instance_repo.update_status(req_ctx, instance.id,
            "compensated", "", executor.failure_reason());

        reply(nats_, msg, provision_parties_response{
            .success = false, .message = executor.failure_reason()});
        return;
    }

    const auto result_json = rfl::json::write(executor.result());
    instance_repo.update_status(req_ctx, instance.id,
        "completed", result_json, "");

    BOOST_LOG_SEV(lg(), debug) << "provision_parties_workflow completed.";
    reply(nats_, msg, executor.result());
}

}
