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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_FIXING_SERIES_SUMMARY_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_FIXING_SERIES_SUMMARY_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/fixing_read_operations_protocol.hpp"
#include "ores.marketdata.service/app/fixing_series_summaries.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <exception>
#include <optional>
#include <string>
#include <utility>

namespace ores::marketdata::messaging {

namespace {
inline auto& fixing_series_summary_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.fixing_series_summary_handler");
    return instance;
}
}

/**
 * @brief NATS message handler for the fixing series a screen lists.
 *
 * The screen is system-level: it states the observations the platform holds,
 * whichever source made them, so the read runs in the system tenant and asks
 * only to read the fixings.
 */
class fixing_series_summary_handler {
public:
    fixing_series_summary_handler(ores::nats::service::client& nats,
                                  const ores::database::context& ctx,
                                  std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    void list(ores::nats::message msg) {
        using namespace ores::logging;
        using ores::service::messaging::error_reply;
        auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx) {
            error_reply(nats_, msg, req_ctx.error());
            return;
        }
        if (!ores::service::messaging::has_permission(*req_ctx,
                                                      "marketdata::market_fixings:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = ores::service::messaging::decode<list_fixing_series_summaries_request>(msg);
        if (!req) {
            ORES_LOG_SEV(fixing_series_summary_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        list_fixing_series_summaries_response resp;
        try {
            const auto sys_ctx = ores::database::service::tenant_context::with_system_tenant(ctx_);
            resp = service::app::list_fixing_series_summaries(sys_ctx, *req);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(fixing_series_summary_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp = {};
            resp.result.outcome = ores::utility::domain::outcome::failed;
            resp.result.code = "internal_error";
            resp.result.message = e.what();
        }
        ores::service::messaging::reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    const ores::database::context& ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
