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
#ifndef ORES_DQ_CORE_MESSAGING_BADGE_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_BADGE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/messaging/badge_protocol.hpp"
#include "ores.dq.core/service/badge_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <stdexcept>

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

namespace {
inline auto& badge_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.badge_handler");
    return instance;
}
} // namespace

class badge_handler {
public:
    badge_handler(ores::nats::service::client& nats,
                  ores::database::context ctx,
                  std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    // =========================================================================
    // Badge Mapping (read-only)
    // =========================================================================

    void list_mappings(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_badge_mappings_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        service::badge_service svc(*ctx_expected);
        try {
            const auto items = svc.list_mappings();
            get_badge_mappings_response resp;
            resp.mappings = items;
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_badge_mappings_response{});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
