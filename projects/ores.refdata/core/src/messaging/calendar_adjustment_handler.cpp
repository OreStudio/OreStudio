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
#include "ores.refdata.core/messaging/calendar_adjustment_handler.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/messaging/calendar_adjustment_protocol.hpp"
#include "ores.refdata.core/service/calendar_adjustment_export_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"

namespace ores::refdata::messaging {

using namespace ores::logging;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::reply;

namespace {
inline auto& lg() {
    static auto instance = make_logger("ores.refdata.messaging.calendar_adjustment_handler");
    return instance;
}
} // namespace

calendar_adjustment_handler::calendar_adjustment_handler(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , verifier_(std::move(verifier)) {}

void calendar_adjustment_handler::get(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "Handling " << msg.subject;
    auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
    if (!req_ctx_expected) {
        error_reply(nats_, msg, req_ctx_expected.error());
        return;
    }
    const auto& req_ctx = *req_ctx_expected;
    if (auto req = decode<get_calendar_adjustments_request>(msg)) {
        try {
            service::calendar_adjustment_export_service svc(req_ctx);
            auto adjustments = svc.assemble(req->calendar_codes);
            BOOST_LOG_SEV(lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  get_calendar_adjustments_response{.adjustments = std::move(adjustments),
                                                    .success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_calendar_adjustments_response{.success = false, .message = e.what()});
        }
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode: " << msg.subject;
        error_reply(nats_, msg, ores::service::error_code::bad_request);
    }
}

}
