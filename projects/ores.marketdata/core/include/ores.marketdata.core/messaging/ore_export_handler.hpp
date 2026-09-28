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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_ORE_EXPORT_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_ORE_EXPORT_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/ore_export_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/service/ore_export_service.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::marketdata::messaging {

namespace {
inline auto& ore_export_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.marketdata.messaging.ore_export_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class ORES_MARKETDATA_CORE_EXPORT ore_export_handler {
public:
    ore_export_handler(ores::nats::service::client& nats,
                       ores::database::context ctx,
                       std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void write_all(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(ore_export_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        // The export reads every series, observation and fixing the tenant
        // has, so it asks for the read permission over all of them rather than
        // for a separate "export" code. Nothing is written, and a caller
        // allowed to read the rows is allowed to read them out.
        if (!has_permission(ctx, "marketdata::observations:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<export_market_data_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(ore_export_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            const service::ore_export_service svc(ctx);
            const auto written = svc.write_all();
            export_market_data_response resp;
            resp.success = true;
            resp.market_data_content = written.market_data;
            resp.fixings_content = written.fixings;
            resp.series_count = written.series_count;
            resp.observation_count = written.observation_count;
            resp.fixing_count = written.fixing_count;
            BOOST_LOG_SEV(ore_export_handler_lg(), debug)
                << "Completed " << msg.subject << ": series=" << resp.series_count
                << " obs=" << resp.observation_count << " fixings=" << resp.fixing_count;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(ore_export_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, export_market_data_response{.success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::marketdata::messaging
#endif
