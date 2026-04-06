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
#include <span>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.nats/domain/correlation.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"
#include "ores.ore.api/messaging/ore_import_engine_protocol.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

ore_import_handler::ore_import_handler(ores::nats::service::client& nats,
    ores::database::context ctx,
    ores::security::jwt::jwt_authenticator signer)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , signer_(std::move(signer)) {}

void ore_import_handler::ore_import(ores::nats::message msg) {
    using ores::ore::messaging::ore_import_request;
    using ores::ore::messaging::ore_import_response;
    using ores::ore::messaging::ore_import_execute_request;
    using ores::workflow::messaging::start_workflow_message;

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

    // Validate request_id — must contain only alphanumeric characters and
    // hyphens to prevent path traversal when constructing the work directory.
    const bool safe = std::all_of(req->request_id.begin(), req->request_id.end(),
        [](unsigned char c) { return std::isalnum(c) || c == '-'; });
    if (!safe) {
        reply(nats_, msg, ore_import_response{
            .success = false,
            .message = "request_id contains invalid characters.",
            .correlation_id = correlation_id});
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ore.import dispatching workflow | corr=" << correlation_id
                              << " request_id=" << req->request_id;

    // Pre-generate the workflow instance UUID so we can return it immediately.
    const auto instance_uuid = boost::uuids::random_generator()();
    const auto instance_id_str = boost::uuids::to_string(instance_uuid);

    // Extract bearer token to delegate the caller's identity inside the saga.
    const auto bearer = ores::nats::service::extract_bearer(msg);

    // Build the internal execute request (stored as workflow_instance.request_json).
    ore_import_execute_request execute_req;
    execute_req.request_id         = req->request_id;
    execute_req.import_choices_json = req->import_choices_json;
    execute_req.correlation_id     = correlation_id;
    execute_req.bearer_token       = bearer;

    // Dispatch start_workflow_message — fire and forget.
    start_workflow_message swm;
    swm.type           = "ore_import_workflow";
    swm.tenant_id      = boost::uuids::to_string(req_ctx.tenant_id().to_uuid());
    swm.request_json   = rfl::json::write(execute_req);
    swm.correlation_id = correlation_id;
    swm.instance_id    = instance_id_str;

    const auto swm_json = rfl::json::write(swm);
    const auto data = std::as_bytes(std::span{swm_json.data(), swm_json.size()});
    nats_.publish(start_workflow_message::nats_subject, data);

    BOOST_LOG_SEV(lg(), info) << "ore.import workflow dispatched | corr=" << correlation_id
                              << " instance_id=" << instance_id_str;

    reply(nats_, msg, ore_import_response{
        .success              = true,
        .message              = "ORE import submitted.",
        .correlation_id       = correlation_id,
        .workflow_instance_id = instance_id_str});
}

}
