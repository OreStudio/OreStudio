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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_MARKET_SERIES_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_MARKET_SERIES_HANDLER_HPP

#include <algorithm>
#include <optional>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.marketdata.core/service/market_series_service.hpp"

namespace ores::marketdata::messaging {

namespace {
inline auto& market_series_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.marketdata.messaging.market_series_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class market_series_handler {
public:
    market_series_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(market_series_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        auto req = decode<get_market_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        service::market_series_service svc(ctx);
        get_market_series_response resp;
        try {
            auto all = svc.list();
            // Apply series_type filter if specified.
            if (!req->series_type.empty()) {
                all.erase(std::remove_if(all.begin(), all.end(),
                    [&](const auto& s) {
                        return s.series_type != req->series_type;
                    }), all.end());
            }
            resp.total_available_count = static_cast<int>(all.size());
            // Apply pagination.
            const int offset = std::max(0, req->offset);
            const int limit  = req->limit > 0 ? req->limit : 1000;
            const auto begin = std::min(offset, static_cast<int>(all.size()));
            const auto end   = std::min(begin + limit, static_cast<int>(all.size()));
            resp.series.assign(all.begin() + begin, all.begin() + end);
            BOOST_LOG_SEV(market_series_handler_lg(), debug)
                << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(market_series_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "marketdata::series:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<save_market_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        service::market_series_service svc(ctx);
        try {
            svc.save(req->series);
            BOOST_LOG_SEV(market_series_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, save_market_series_response{
                .success = true,
                .saved_count = static_cast<int>(req->series.size())});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_market_series_response{
                .success = false, .message = e.what()});
        }
    }

    void del(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(market_series_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "marketdata::series:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_market_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        service::market_series_service svc(ctx);
        try {
            svc.remove(boost::lexical_cast<boost::uuids::uuid>(req->id));
            BOOST_LOG_SEV(market_series_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, delete_market_series_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_market_series_response{
                .success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::marketdata::messaging
#endif
