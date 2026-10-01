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
#include "ores.workflow.core/messaging/workflow_handler.hpp"
#include "ores.nats/domain/correlation.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include "ores.workflow.core/service/workflow_engine.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <rfl/json.hpp>

namespace ores::workflow::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

workflow_handler::workflow_handler(ores::nats::service::client& nats,
                                   ores::database::context ctx,
                                   ores::security::jwt::jwt_authenticator signer,
                                   std::shared_ptr<service::workflow_engine> engine)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , signer_(std::move(signer))
    , engine_(std::move(engine)) {}

void workflow_handler::retry_instance(ores::nats::message msg) {
    const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);
    BOOST_LOG_SEV(lg(), info) << "retry_instance correlation_id=" << correlation_id;

    auto ctx_expected = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx_expected) {
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& req_ctx = *ctx_expected;

    if (!has_permission(req_ctx, "workflow::workflow_instances:write")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }

    auto req = decode<retry_workflow_instance_request>(msg);
    if (!req) {
        reply(nats_,
              msg,
              retry_workflow_instance_response{.success = false,
                                               .message = "Invalid request payload."});
        return;
    }

    if (req->workflow_instance_id.empty()) {
        reply(nats_,
              msg,
              retry_workflow_instance_response{.success = false,
                                               .message = "workflow_instance_id is required."});
        return;
    }

    boost::uuids::uuid instance_id;
    try {
        instance_id = boost::lexical_cast<boost::uuids::uuid>(req->workflow_instance_id);
    } catch (...) {
        reply(nats_,
              msg,
              retry_workflow_instance_response{.success = false,
                                               .message = "Invalid workflow_instance_id."});
        return;
    }

    retry_workflow_instance_response resp;
    resp.workflow_instance_id = req->workflow_instance_id;
    try {
        const auto outcome =
            engine_->retry_instance(instance_id, req->step_name, req_ctx.tenant_id());
        resp.success = outcome.resumed;
        resp.message = outcome.reason;
        resp.step_index = outcome.step_index;
        resp.step_name = outcome.step_name;
        BOOST_LOG_SEV(lg(), info) << "retry_instance " << (outcome.resumed ? "resumed" : "refused")
                                  << " workflow=" << req->workflow_instance_id
                                  << (outcome.resumed ? " step=" + outcome.step_name :
                                                        " reason=" + outcome.reason);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << msg.subject << " failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }
    reply(nats_, msg, resp);
}

}
