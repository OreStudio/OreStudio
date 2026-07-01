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

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/market_series_export_protocol.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.marketdata.core/service/market_series_service.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.storage/net/storage_transfer.hpp"
#include <optional>
#include <rfl/msgpack.hpp>

namespace ores::marketdata::messaging {

namespace {
inline auto& market_series_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.market_series_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for market series operations.
 */
class market_series_handler {
public:
    market_series_handler(ores::nats::service::client& nats,
                          ores::database::context ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::market_series_service svc(req_ctx);
        get_market_series_response resp;
        if (auto req = decode<get_market_series_request>(msg)) {
            try {
                resp.market_series = svc.list_market_series(req->offset, req->limit);
                resp.total_available_count = static_cast<int>(svc.count_market_series());
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(market_series_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        } else {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::market_series_service svc(req_ctx);
        if (auto req = decode<save_market_series_request>(msg)) {
            try {
                svc.save_market_series(req->data);
                BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_, msg, save_market_series_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(market_series_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(
                    nats_, msg, save_market_series_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::market_series_service svc(req_ctx);
        if (auto req = decode<get_market_series_history_request>(msg)) {
            try {
                auto hist = svc.get_market_series_history(req->id);
                BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_,
                      msg,
                      get_market_series_history_response{.history = std::move(hist),
                                                         .success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(market_series_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      get_market_series_history_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::market_series_service svc(req_ctx);
        if (auto req = decode<delete_market_series_request>(msg)) {
            try {
                svc.delete_market_series(req->ids);
                BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_, msg, delete_market_series_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(market_series_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      delete_market_series_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void export_to_storage(ores::nats::message msg, const std::string& http_base_url) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(market_series_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        export_market_data_to_storage_response resp;
        try {
            auto req = decode<export_market_data_to_storage_request>(msg);
            if (!req) {
                resp.message = "Failed to decode request.";
                reply(nats_, msg, resp);
                return;
            }

            service::market_series_service svc(ctx);
            const auto total = svc.count_market_series();
            const auto series = svc.list_market_series(0, total ? total : 1);

            const auto blob = rfl::msgpack::write(series);
            ores::storage::net::storage_transfer transfer(http_base_url);
            transfer.upload_blob(req->storage_bucket, req->storage_key, blob);

            resp.success = true;
            resp.series_count = static_cast<int>(series.size());
            resp.storage_key = req->storage_key;
            resp.message = "Exported " + std::to_string(series.size()) + " series to storage.";

            BOOST_LOG_SEV(market_series_handler_lg(), info)
                << "export_to_storage: exported " << series.size() << " series, " << blob.size()
                << " bytes to " << req->storage_bucket << "/" << req->storage_key;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::marketdata::messaging

#endif
