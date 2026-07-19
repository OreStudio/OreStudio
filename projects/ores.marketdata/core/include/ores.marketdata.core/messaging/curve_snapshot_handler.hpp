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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_CURVE_SNAPSHOT_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_CURVE_SNAPSHOT_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/curve_snapshot_protocol.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <chrono>
#include <optional>

namespace ores::marketdata::messaging {

namespace {
inline auto& curve_snapshot_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.curve_snapshot_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

/**
 * @brief NATS message handler for curve snapshot / curve-evolution queries -- read-only, thin
 * wrappers around market_observations_repository::read_as_of()/read_as_of_buckets(), with
 * series_id resolved server-side from (series_type, metric, qualifier) so callers don't need
 * to know internal series ids.
 */
class curve_snapshot_handler {
public:
    curve_snapshot_handler(ores::nats::service::client& nats,
                           ores::database::context ctx,
                           std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void get_snapshot(ores::nats::message msg) {
        BOOST_LOG_SEV(curve_snapshot_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        get_curve_snapshot_response resp;
        if (auto req = decode<get_curve_snapshot_request>(msg)) {
            try {
                repository::market_series_repository series_repo;
                auto series = series_repo.read_latest_by_type(
                    req_ctx, req->series_type, req->metric, req->qualifier);
                if (!series.empty()) {
                    repository::market_observations_repository obs_repo;
                    resp.observations =
                        obs_repo.read_as_of(req_ctx, series.front().id, std::chrono::system_clock::now());
                }
                // No series yet (feed hasn't published) is not an error -- empty snapshot.
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(curve_snapshot_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        } else {
            BOOST_LOG_SEV(curve_snapshot_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        BOOST_LOG_SEV(curve_snapshot_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void get_snapshot_buckets(ores::nats::message msg) {
        BOOST_LOG_SEV(curve_snapshot_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        get_curve_snapshot_buckets_response resp;
        if (auto req = decode<get_curve_snapshot_buckets_request>(msg)) {
            try {
                repository::market_series_repository series_repo;
                auto series = series_repo.read_latest_by_type(
                    req_ctx, req->series_type, req->metric, req->qualifier);
                if (!series.empty()) {
                    repository::market_observations_repository obs_repo;
                    resp.buckets = obs_repo.read_as_of_buckets(
                        req_ctx,
                        series.front().id,
                        std::chrono::system_clock::now(),
                        std::chrono::seconds(req->bucket_seconds),
                        req->bucket_count);
                } else {
                    resp.buckets.resize(req->bucket_count);
                }
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(curve_snapshot_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        } else {
            BOOST_LOG_SEV(curve_snapshot_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        BOOST_LOG_SEV(curve_snapshot_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::marketdata::messaging

#endif
