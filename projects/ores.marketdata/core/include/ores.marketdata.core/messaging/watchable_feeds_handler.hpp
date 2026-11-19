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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_WATCHABLE_FEEDS_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_WATCHABLE_FEEDS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/tick_entitlement.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <optional>

namespace ores::marketdata::messaging {

namespace {
inline auto& watchable_feeds_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.watchable_feeds_handler");
    return instance;
}
}

/**
 * @brief NATS message handler for the feeds a party may watch live.
 *
 * The broker does not check subjects, so a watcher asks this before it subscribes. The rule is
 * repository::watchable_feeds, which a test drives without NATS.
 */
class watchable_feeds_handler {
public:
    watchable_feeds_handler(ores::nats::service::client& nats,
                            const ores::database::context& ctx,
                            std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    void list(ores::nats::message msg) {
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        BOOST_LOG_SEV(watchable_feeds_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        // A watch shows the ticks the history read returns, so it takes the same permission.
        if (!ores::service::messaging::has_permission(req_ctx,
                                                      "marketdata::market_observations:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        const auto req = decode<list_watchable_feeds_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(watchable_feeds_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        list_watchable_feeds_response resp;
        if (!req_ctx.tenant_id().is_system() && !req_ctx.party_id()) {
            resp.result = ores::database::domain::refuse(
                ores::database::domain::outcome_code::party_required, {.entity = "feeds"});
        } else {
            try {
                resp.feeds = repository::watchable_feeds(req_ctx, req->for_display);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(watchable_feeds_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.result.outcome = ores::utility::domain::outcome::failed;
                resp.result.code = "internal_error";
                resp.result.message = e.what();
            }
        }
        BOOST_LOG_SEV(watchable_feeds_handler_lg(), debug)
            << "Completed " << msg.subject << " tenant=" << req_ctx.tenant_id().to_string()
            << " for_display=" << req->for_display << " feeds=" << resp.feeds.size();
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
