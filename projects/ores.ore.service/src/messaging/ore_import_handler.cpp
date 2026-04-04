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
#include "ores.ore.service/messaging/ore_import_handler.hpp"

#include <algorithm>
#include <chrono>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.service/error_code.hpp"
#include "ores.nats/domain/correlation.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.workflow/domain/workflow_instance.hpp"
#include "ores.workflow/repository/workflow_instance_repository.hpp"
#include "ores.workflow/service/fsm_state_map.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"
#include "ores.ore.service/service/ore_import_workflow.hpp"

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

ore_import_handler::ore_import_handler(ores::nats::service::client& nats,
    ores::database::context ctx,
    ores::security::jwt::jwt_authenticator signer,
    ores::nats::service::nats_client outbound_nats,
    std::string http_base_url,
    std::string work_dir)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , signer_(std::move(signer))
    , outbound_nats_(std::move(outbound_nats))
    , http_base_url_(std::move(http_base_url))
    , work_dir_(std::move(work_dir)) {}

void ore_import_handler::ore_import(ores::nats::message msg) {
    using ores::ore::messaging::ore_import_request;
    using ores::ore::messaging::ore_import_response;

    // Extract (or generate) correlation ID — propagated from the Qt client.
    const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);
    BOOST_LOG_SEV(lg(), info) << "ore.import received | corr=" << correlation_id;

    // Authenticate JWT and build per-request DB context.
    auto ctx_expected = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx_expected) {
        BOOST_LOG_SEV(lg(), warn) << "ore.import auth failed | corr=" << correlation_id;
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& req_ctx = *ctx_expected;

    // Decode request payload.
    auto req = decode<ore_import_request>(msg);
    if (!req) {
        BOOST_LOG_SEV(lg(), warn) << "ore.import bad payload | corr=" << correlation_id;
        reply(nats_, msg, ore_import_response{
            .success = false,
            .message = "Invalid request payload.",
            .correlation_id = correlation_id});
        return;
    }

    if (req->request_id.empty()) {
        reply(nats_, msg, ore_import_response{
            .success = false,
            .message = "request_id is required.",
            .correlation_id = correlation_id});
        return;
    }

    // Validate request_id — must contain only alphanumeric characters and hyphens
    // to prevent path traversal when constructing the work directory path.
    const bool safe = std::all_of(req->request_id.begin(), req->request_id.end(),
        [](unsigned char c) { return std::isalnum(c) || c == '-'; });
    if (!safe) {
        reply(nats_, msg, ore_import_response{
            .success = false,
            .message = "request_id contains invalid characters.",
            .correlation_id = correlation_id});
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import starting | corr=" << correlation_id
                              << " request_id=" << req->request_id;

    // Build outbound NATS client with caller's JWT delegated.
    const auto bearer = ores::nats::service::extract_bearer(msg);
    auto delegated_nats = outbound_nats_
        .with_delegation(bearer)
        .with_correlation_id(correlation_id);

    // Load FSM states for workflow instances.
    const auto instance_states = ores::workflow::service::load_fsm_states(
        delegated_nats, "workflow_instance");

    const auto request_json = rfl::json::write(*req);

    // Create workflow instance record.
    ores::workflow::domain::workflow_instance instance;
    instance.id = boost::uuids::random_generator()();
    instance.tenant_id = req_ctx.tenant_id().to_uuid();
    instance.type = "ore_import_workflow";
    instance.state_id = instance_states.require("in_progress");
    instance.request_json = request_json;
    instance.correlation_id = correlation_id;
    instance.created_by = delegated_actor(req_ctx);
    instance.created_at = std::chrono::system_clock::now();

    ores::workflow::repository::workflow_instance_repository instance_repo;
    instance_repo.create(req_ctx, instance);

    BOOST_LOG_SEV(lg(), debug) << "ore.import workflow_instance created | corr=" << correlation_id
                               << " workflow_id=" << boost::uuids::to_string(instance.id);

    // Run the saga executor.
    ores::ore::service::service::ore_import_workflow executor(
        instance.id, std::move(*req), correlation_id, http_base_url_, work_dir_);

    bool ok = false;
    try {
        ok = executor.execute(req_ctx, delegated_nats);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "ore.import unhandled exception | corr="
                                   << correlation_id << " error=" << e.what();
        instance_repo.update_state(req_ctx, instance.id,
            instance_states.require("failed"), "", e.what());
        reply(nats_, msg, ore_import_response{
            .success = false,
            .message = e.what(),
            .correlation_id = correlation_id});
        return;
    }

    if (!ok) {
        BOOST_LOG_SEV(lg(), warn) << "ore.import workflow failed | corr=" << correlation_id
                                  << " reason=" << executor.failure_reason()
                                  << ". Starting compensation.";

        instance_repo.update_state(req_ctx, instance.id,
            instance_states.require("compensating"), "", executor.failure_reason());

        try {
            executor.compensate(req_ctx, delegated_nats);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "ore.import compensation exception | corr="
                                       << correlation_id << " error=" << e.what();
        }

        instance_repo.update_state(req_ctx, instance.id,
            instance_states.require("compensated"), "", executor.failure_reason());

        reply(nats_, msg, ore_import_response{
            .success = false,
            .message = executor.failure_reason(),
            .correlation_id = correlation_id});
        return;
    }

    const auto result_json = rfl::json::write(executor.result());
    instance_repo.update_state(req_ctx, instance.id,
        instance_states.require("completed"), result_json, "");

    BOOST_LOG_SEV(lg(), info) << "ore.import complete | corr=" << correlation_id
                              << " item_errors=" << executor.result().item_errors.size();
    reply(nats_, msg, executor.result());
}

}
