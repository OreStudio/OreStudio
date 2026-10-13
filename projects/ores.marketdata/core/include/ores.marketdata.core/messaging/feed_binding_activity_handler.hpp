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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_FEED_BINDING_ACTIVITY_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_FEED_BINDING_ACTIVITY_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/service/feed_binding_activity_reader.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::marketdata::messaging {

namespace {

auto& feed_binding_activity_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.feed_binding_activity_handler");
    return instance;
}

}

using ores::service::messaging::reply;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for the read of what each feed binding has written.
 *
 * The request carries nothing, so there is nothing to decode. The adapter proves the
 * request, checks that the caller may read feed bindings and replies with what
 * service::feed_binding_activity_reader answers.
 */
class feed_binding_activity_handler {
public:
    feed_binding_activity_handler(ores::nats::service::client& nats,
                                  ores::database::context ctx,
                                  std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(feed_binding_activity_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::feed_bindings:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        list_feed_binding_activity_response resp;
        try {
            resp = service::feed_binding_activity_reader::read(req_ctx);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(feed_binding_activity_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(feed_binding_activity_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
