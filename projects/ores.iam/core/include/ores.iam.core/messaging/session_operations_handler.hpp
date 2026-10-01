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
#ifndef ORES_IAM_MESSAGING_SESSION_HANDLER_HPP
#define ORES_IAM_MESSAGING_SESSION_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/messaging/session_operations_protocol.hpp"
#include "ores.iam.api/messaging/session_samples_protocol.hpp"
#include "ores.iam.core/service/session_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"

namespace ores::iam::messaging {

namespace {

inline auto& session_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.iam.messaging.session_operations_handler");
    return instance;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class session_operations_handler {
public:
    session_operations_handler(ores::nats::service::client& nats,
                               ores::database::context ctx,
                               ores::security::jwt::jwt_authenticator signer)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer)) {}

    /**
     * @brief Serves iam.v1.sessions.active.
     *
     * The rows are the sessions whose end time is empty, which is what makes a
     * session active. The caller must hold the permission a session read needs,
     * as the derived CRUD reads of this resource do.
     */
    void active(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(session_handler_lg(), msg);
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, signer_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "iam::sessions:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        try {
            service::session_service svc(req_ctx);
            auto rows = svc.active_sessions();
            BOOST_LOG_SEV(session_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, get_active_sessions_response{.sessions = std::move(rows), .success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(session_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_active_sessions_response{.success = false, .message = e.what()});
        }
    }

    void samples(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(session_handler_lg(), msg);
        BOOST_LOG_SEV(session_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, get_session_samples_response{.success = true});
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
