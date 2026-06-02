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
#ifndef ORES_DQ_CORE_MESSAGING_FSM_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_FSM_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.dq.api/messaging/fsm_protocol.hpp"
#include "ores.dq.core/service/fsm_service.hpp"

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

namespace {
inline auto& fsm_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.dq.messaging.fsm_handler");
    return instance;
}
} // namespace

class fsm_handler {
public:
    fsm_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(fsm_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_fsm_states_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fsm_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            reply(nats_, msg, get_fsm_states_response{
                .success = false, .message = "Failed to decode request"});
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        service::fsm_service svc(*ctx_expected);
        try {
            get_fsm_states_response resp;
            if (req->machine_name.empty())
                resp.states = svc.list_all_states();
            else
                resp.states = svc.list_states_for_machine(req->machine_name);
            resp.success = true;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(fsm_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_fsm_states_response{
                .success = false, .message = e.what()});
        }
        BOOST_LOG_SEV(fsm_handler_lg(), debug) << "Completed " << msg.subject;
    }

    void list_transitions(ores::nats::message msg) {
        BOOST_LOG_SEV(fsm_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_fsm_transitions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fsm_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            reply(nats_, msg, get_fsm_transitions_response{
                .success = false, .message = "Failed to decode request"});
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        service::fsm_service svc(*ctx_expected);
        try {
            get_fsm_transitions_response resp;
            if (req->machine_name.empty())
                resp.transitions = svc.list_all_transitions();
            else
                resp.transitions = svc.list_transitions_for_machine(req->machine_name);
            resp.success = true;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(fsm_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_fsm_transitions_response{
                .success = false, .message = e.what()});
        }
        BOOST_LOG_SEV(fsm_handler_lg(), debug) << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
