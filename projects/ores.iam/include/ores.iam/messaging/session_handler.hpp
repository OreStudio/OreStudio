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

#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.iam/messaging/session_protocol.hpp"
#include "ores.iam/messaging/session_samples_protocol.hpp"

namespace ores::iam::messaging {

namespace {

inline auto& session_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.iam.messaging.session_handler");
    return instance;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;

class session_handler {
public:
    session_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer)
        : nats_(nats), ctx_(std::move(ctx)), signer_(std::move(signer)) {}

    void list(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(session_handler_lg(), debug)
            << "Handling " << msg.subject;
        BOOST_LOG_SEV(session_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, list_sessions_response{});
    }

    void active(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(session_handler_lg(), debug)
            << "Handling " << msg.subject;
        BOOST_LOG_SEV(session_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, get_active_sessions_response{});
    }

    void samples(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(session_handler_lg(), debug)
            << "Handling " << msg.subject;
        BOOST_LOG_SEV(session_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, get_session_samples_response{});
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
