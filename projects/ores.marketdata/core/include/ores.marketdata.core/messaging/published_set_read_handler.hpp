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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_PUBLISHED_SET_READ_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_PUBLISHED_SET_READ_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.marketdata.core/service/published_set_reader.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::messaging {

namespace {
inline auto& published_set_read_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.published_set_read_handler");
    return instance;
}
}

/**
 * @brief NATS message handler for the newest published set of a batch of series.
 *
 * The handler owns the session, the permission and the wire. The read itself is
 * service::published_set_reader, which a test drives without NATS.
 */
class published_set_read_handler {
public:
    published_set_read_handler(ores::nats::service::client& nats,
                               const ores::database::context& ctx,
                               std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    void latest(ores::nats::message msg) {
        using namespace ores::logging;
        using ores::service::messaging::error_reply;
        auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx) {
            error_reply(nats_, msg, req_ctx.error());
            return;
        }
        if (!ores::service::messaging::has_permission(*req_ctx,
                                                      std::string(curve_publishes_read))) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = ores::service::messaging::decode<get_latest_published_sets_request>(msg);
        if (!req) {
            ORES_LOG_SEV(published_set_read_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        get_latest_published_sets_response resp;
        try {
            resp = service::published_set_reader::latest(*req_ctx, *req);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(published_set_read_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp = {};
            resp.result.outcome = ores::utility::domain::outcome::failed;
            resp.result.code = "internal_error";
            resp.result.message = e.what();
        }
        ores::service::messaging::reply(nats_, msg, resp);
    }

private:
    static constexpr std::string_view curve_publishes_read = "marketdata::curve_publishes:read";

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
