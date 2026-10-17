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
#include "ores.workflow.core/messaging/workflow_claim_handler.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workflow.api/messaging/workflow_claim_messages.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <exception>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::workflow::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

namespace {

using state_names_t =
    std::unordered_map<boost::uuids::uuid, std::string, std::hash<boost::uuids::uuid>>;

/**
 * @brief Decides one claim request, answering whatever it can.
 *
 * Every answer is a claim_step_response, including the refusals: a delivery
 * that cannot be told what to do would have to guess, and both guesses -- do
 * the work anyway, or drop the command -- are worse than saying which it is.
 */
claim_step_response decide(ores::database::context ctx,
                           const state_names_t& state_names,
                           repository::step_claim_repository& claim_repo,
                           repository::workflow_step_repository& step_repo,
                           const claim_step_request& req) {

    boost::uuids::uuid step_uuid;
    try {
        step_uuid = boost::lexical_cast<boost::uuids::uuid>(req.step_id);
    } catch (const std::exception&) {
        return {.success = false, .message = "Not a step id: " + req.step_id};
    }

    // The caller names the tenant the step belongs to and the read is scoped to
    // it, exactly as the step-result query is. The service's own context would
    // reach every tenant's steps, and a claim is only ever granted for a step of
    // the tenant the command was dispatched to.
    if (req.owner.empty())
        return {.success = false, .message = "The request names no holder for the claim."};

    const auto tenant = ores::utility::uuid::tenant_id::from_string(req.tenant_id);
    if (!tenant)
        return {.success = false, .message = "Not a tenant id: " + req.tenant_id};
    const auto req_ctx = ctx.with_tenant(*tenant, ctx.actor());

    const auto steps = step_repo.read_latest(req_ctx, boost::uuids::to_string(step_uuid));
    if (steps.empty() || steps.front().tenant_id != req_ctx.tenant_id()) {
        // Nothing can ever serve this command, so the delivery is told so
        // rather than left holding a step that does not exist.
        return {.success = true,
                .claimed = false,
                .state = "unknown",
                .message = "No such workflow step."};
    }
    const auto* step = &steps.front();

    // The step's state is the authority on whether this work is still owed. A
    // step that already completed or failed refuses a replay here, whatever the
    // claim table says, so a redelivery after the outcome was recorded does
    // nothing rather than running the work a second time.
    const auto state = state_names.find(step->state_id);
    const std::string state_name = state == state_names.end() ? "unknown" : state->second;
    if (state_name != "in_progress") {
        return {.success = true,
                .claimed = false,
                .state = state_name,
                .message = "The step has already left in_progress."};
    }

    // The holder is the caller's own identity, recorded verbatim: the caller is
    // the process that runs the step's work, and only it knows which process
    // that is.
    const auto attempts = claim_repo.claim(
        ctx, step_uuid, req_ctx.tenant_id().to_uuid(), req.owner, step_claim_lease);
    if (!attempts) {
        // Someone else is working the step right now. The delivery comes back
        // when the lease ends, and by then the step will have left in_progress
        // or the claim will be free.
        return {.success = true,
                .claimed = false,
                .state = "in_progress",
                .message = "Another delivery holds a live claim on this step."};
    }

    return {.success = true, .claimed = true, .state = "in_progress", .attempts = *attempts};
}

} // namespace

workflow_claim_handler::workflow_claim_handler(ores::nats::service::client& nats,
                                               ores::database::context ctx,
                                               const service::fsm_state_map& step_states)
    : nats_(nats)
    , ctx_(std::move(ctx)) {

    for (const auto& [name, uuid] : step_states.states)
        step_state_names_[uuid] = name;
}

void workflow_claim_handler::claim_step(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "claim_step request received";

    // The delivery is answered on every path, so a claim this handler cannot
    // decide is redelivered rather than dropped.
    ores::nats::ack_guard ack(msg);

    const auto req = decode<claim_step_request>(msg);
    if (!req || req->step_id.empty() || req->tenant_id.empty()) {
        reply(nats_,
              msg,
              claim_step_response{.success = false,
                                  .message = "step_id and tenant_id are both required."});
        ack.accept();
        return;
    }

    // A database call can throw. An exception leaving this handler reaches the
    // transport, which contains it but sends no reply, so the caller would wait
    // out its deadline for an answer this handler could have given. An answer
    // that says the claim could not be decided makes the caller return the
    // command to the queue, which is what failing closed means here: the step's
    // work is never done unclaimed.
    claim_step_response resp;
    try {
        resp = decide(ctx_, step_state_names_, claim_repo_, step_repo_, *req);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << msg.subject << " failed: " << e.what();
        resp = claim_step_response{.success = false, .message = e.what()};
    }

    if (resp.success && resp.claimed)
        BOOST_LOG_SEV(lg(), debug)
            << "Claimed step " << req->step_id << " on attempt " << resp.attempts;
    else
        BOOST_LOG_SEV(lg(), debug) << "Did not claim step " << req->step_id << ": " << resp.message;

    reply(nats_, msg, resp);
    ack.accept();
}

} // namespace ores::workflow::messaging
