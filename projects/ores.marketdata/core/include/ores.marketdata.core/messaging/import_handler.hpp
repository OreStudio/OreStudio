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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_IMPORT_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_IMPORT_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/import_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::marketdata::messaging {

namespace {
inline auto& import_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.marketdata.messaging.import_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class ORES_MARKETDATA_CORE_EXPORT import_handler {
public:
    import_handler(ores::nats::service::client& nats,
                   ores::database::context ctx,
                   std::optional<ores::security::jwt::jwt_authenticator> verifier,
                   ores::nats::service::nats_client& auth_nats)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , auth_nats_(auth_nats) {}

    void import(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(import_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "marketdata::import:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<import_market_data_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(import_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        // Currency-pair reference data (for FX quote convention checking)
        // is tenant-scoped — the service's own long-lived token has no
        // tenant context, so it must run delegated as the caller, exactly
        // like feed_controller's market-data lookups.
        auto delegated_nats = auth_nats_.with_delegation(ores::nats::service::extract_bearer(msg));
        service::import_service svc(ctx, delegated_nats);
        try {
            auto resp = svc.import(*req);
            BOOST_LOG_SEV(import_handler_lg(), debug)
                << "Completed " << msg.subject << ": series=" << resp.series_count
                << " obs=" << resp.observation_count << " fixings=" << resp.fixing_count;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(import_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, import_market_data_response{.success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    ores::nats::service::nats_client& auth_nats_;
};

} // namespace ores::marketdata::messaging
#endif
