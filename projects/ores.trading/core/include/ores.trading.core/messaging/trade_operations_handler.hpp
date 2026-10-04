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
#ifndef ORES_TRADING_CORE_MESSAGING_TRADE_OPERATIONS_HANDLER_HPP
#define ORES_TRADING_CORE_MESSAGING_TRADE_OPERATIONS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.core/service/trade_export_service.hpp"
#include "ores.trading.core/service/trade_operations_service.hpp"
#include <cstdint>
#include <optional>
#include <rfl/msgpack.hpp>
#include <string>

namespace ores::trading::messaging {

namespace {
inline auto& trade_operations_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.trading.messaging.trade_operations_handler");
    return instance;
}
} // namespace

/**
 * @brief Hand-written NATS handler for the trade operations.
 *
 * Lives outside the generated handlers because booking a trade writes the
 * anchor and its components together, and an export reads them together,
 * which no entity verb does. The service decides the outcome; the handler
 * proves the request, checks the permission, and replies.
 */
class trade_operations_handler {
public:
    trade_operations_handler(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             std::string http_base_url = {})
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , http_base_url_(std::move(http_base_url)) {}

    /**
     * @brief Serves trading.v1.trades.book.
     */
    void book_trade(ores::nats::message msg) {
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::has_permission;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        BOOST_LOG_SEV(trade_operations_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::trades:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<book_trade_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(trade_operations_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::trade_operations_service svc(req_ctx);
        try {
            reply(nats_, msg, svc.book_trade(*req));
        } catch (const std::exception& e) {
            // The service reports a conflict in the response; an exception is
            // the store refusing the write, such as a check the trade fails.
            BOOST_LOG_SEV(trade_operations_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            book_trade_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.trades.portfolio.export.
     */
    void export_portfolio(ores::nats::message msg) {
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        BOOST_LOG_SEV(trade_operations_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        export_portfolio_response resp;
        try {
            if (auto req = decode<export_portfolio_request>(msg)) {
                service::trade_export_service svc(*ctx_expected);
                resp.items = svc.export_node(req->node_id,
                                             static_cast<std::uint32_t>(req->offset),
                                             static_cast<std::uint32_t>(req->limit));
                resp.success = true;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(trade_operations_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    /**
     * @brief Serves trading.v1.trades.export-to-storage.
     */
    void export_trades_to_storage(ores::nats::message msg) {
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        BOOST_LOG_SEV(trade_operations_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        export_trades_to_storage_response resp;
        try {
            auto req = decode<export_trades_to_storage_request>(msg);
            if (!req || req->book_ids.empty()) {
                resp.message = "Invalid request or empty book_ids.";
                reply(nats_, msg, resp);
                return;
            }

            service::trade_export_service svc(*ctx_expected);
            std::vector<trade_export_item> all_items;
            constexpr std::uint32_t page_size = 1000;
            for (std::uint32_t offset = 0;; offset += page_size) {
                auto page = svc.export_books(req->book_ids, offset, page_size);
                const auto n = page.size();
                all_items.insert(all_items.end(),
                                 std::make_move_iterator(page.begin()),
                                 std::make_move_iterator(page.end()));
                if (n < page_size)
                    break;
            }

            const auto blob = rfl::msgpack::write(all_items);
            ores::storage::net::storage_transfer transfer(http_base_url_, extract_bearer(msg));
            transfer.upload_blob(req->storage_bucket, req->storage_key, blob);

            resp.success = true;
            resp.trade_count = static_cast<int>(all_items.size());
            resp.storage_key = req->storage_key;
            resp.message = "Exported " + std::to_string(all_items.size()) + " trades to storage.";
            BOOST_LOG_SEV(trade_operations_handler_lg(), info)
                << "export_trades_to_storage: exported " << all_items.size() << " trades, "
                << blob.size() << " bytes (pre-compression) to " << req->storage_bucket << "/"
                << req->storage_key;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(trade_operations_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    // The end user's token, preferring the delegated header, so the storage
    // service sees the caller's identity rather than the trading service's.
    static std::string extract_bearer(const ores::nats::message& msg) {
        using namespace ores::nats::headers;
        for (auto hdr : {delegated_authorization, authorization}) {
            const auto it = msg.headers.find(std::string(hdr));
            if (it != msg.headers.end() && it->second.starts_with(bearer_prefix))
                return std::string(it->second.substr(bearer_prefix.size()));
        }
        return {};
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    std::string http_base_url_;
};

} // namespace ores::trading::messaging

#endif
