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

#include <span>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.service/error_code.hpp"
#include "ores.nats/domain/correlation.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"

namespace ores::workflow::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

workflow_handler::workflow_handler(ores::nats::service::client& nats,
    ores::database::context ctx,
    ores::security::jwt::jwt_authenticator signer)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , signer_(std::move(signer)) {}

void workflow_handler::provision_parties(ores::nats::message msg) {
    const auto correlation_id =
        ores::nats::extract_or_generate_correlation_id(msg);
    BOOST_LOG_SEV(lg(), info) << "provision_parties correlation_id=" << correlation_id;

    // Validate JWT and build per-request context.
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

    const auto tenant_id_str =
        boost::uuids::to_string(req_ctx.tenant_id().to_uuid());

    provision_parties_response resp;
    resp.success = true;
    resp.correlation_id = correlation_id;

    boost::uuids::random_generator gen;
    for (const auto& input : req->parties) {
        const auto party_id = gen();
        const auto party_id_str = boost::uuids::to_string(party_id);

        // Build the per-party workflow request that the engine will store and
        // pass to the step builders.
        provision_party_workflow_request wf_req;
        wf_req.party_id            = party_id_str;
        wf_req.full_name           = input.full_name;
        wf_req.short_code          = input.short_code;
        wf_req.party_category      = input.party_category;
        wf_req.party_type          = input.party_type;
        wf_req.business_center_code = input.business_center_code;
        wf_req.parent_party_id     = input.parent_party_id;
        wf_req.status              = "Inactive"; // wizard fires on first login
        wf_req.principal           = input.principal;
        wf_req.password            = input.password;
        wf_req.totp_secret         = input.totp_secret;
        wf_req.email               = input.email;
        wf_req.account_type        = input.account_type;

        start_workflow_message start_msg;
        start_msg.type         = "provision_parties_workflow";
        start_msg.tenant_id    = tenant_id_str;
        start_msg.request_json = rfl::json::write(wf_req);
        start_msg.correlation_id = correlation_id;

        const auto json = rfl::json::write(start_msg);
        const auto data = std::as_bytes(std::span{json.data(), json.size()});
        nats_.publish(start_workflow_message::nats_subject, data);

        resp.party_ids.push_back(party_id_str);
    }

    BOOST_LOG_SEV(lg(), debug) << "provision_parties dispatched "
        << req->parties.size() << " workflow(s).";
    reply(nats_, msg, resp);
}

}
