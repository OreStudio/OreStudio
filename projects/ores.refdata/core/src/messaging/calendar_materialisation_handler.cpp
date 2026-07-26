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
#include "ores.refdata.core/messaging/calendar_materialisation_handler.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/messaging/calendar_materialisation_protocol.hpp"
#include "ores.refdata.core/service/calendar_materialisation_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"

namespace ores::refdata::messaging {

using namespace ores::logging;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;

namespace {
inline auto& lg() {
    static auto instance =
        make_logger("ores.refdata.messaging.calendar_materialisation_handler");
    return instance;
}
} // namespace

calendar_materialisation_handler::calendar_materialisation_handler(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , verifier_(std::move(verifier)) {}

void calendar_materialisation_handler::regenerate(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "Handling " << msg.subject;
    auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
    if (!req_ctx_expected) {
        error_reply(nats_, msg, req_ctx_expected.error());
        return;
    }
    const auto& req_ctx = *req_ctx_expected;
    if (!has_permission(req_ctx, "refdata::calendars:write")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    if (auto req = decode<regenerate_calendar_dates_request>(msg)) {
        try {
            service::calendar_materialisation_service svc(req_ctx);
            const auto end_year = req->end_year
                ? std::optional<std::chrono::year>(std::chrono::year{*req->end_year})
                : std::nullopt;
            const auto rows = req->calendar_code
                ? svc.regenerate(*req->calendar_code, end_year)
                : svc.regenerate_all(end_year);
            BOOST_LOG_SEV(lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  regenerate_calendar_dates_response{
                      .success = true, .rows_written = static_cast<std::uint64_t>(rows)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  regenerate_calendar_dates_response{.success = false, .message = e.what()});
        }
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode: " << msg.subject;
        error_reply(nats_, msg, ores::service::error_code::bad_request);
    }
}

}
