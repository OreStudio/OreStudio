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
#ifndef ORES_TRADING_MESSAGING_TRADE_HANDLER_HPP
#define ORES_TRADING_MESSAGING_TRADE_HANDLER_HPP

#include <optional>
#include <rfl/msgpack.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.storage/net/storage_transfer.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.core/service/activity_type_service.hpp"
#include "ores.trading.core/service/trade_service.hpp"
#include "ores.trading.core/service/instrument_service.hpp"
#include "ores.trading.core/service/fx_instrument_service.hpp"
#include "ores.trading.core/service/bond_instrument_service.hpp"
#include "ores.trading.core/service/credit_instrument_service.hpp"
#include "ores.trading.core/service/equity_instrument_service.hpp"
#include "ores.trading.core/service/commodity_instrument_service.hpp"
#include "ores.trading.core/service/composite_instrument_service.hpp"
#include "ores.trading.core/service/scripted_instrument_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::messaging {

namespace {
inline auto& trade_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.trading.messaging.trade_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

class trade_handler {
public:
    trade_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier,
        std::string http_base_url = {})
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)),
          http_base_url_(std::move(http_base_url)) {}

private:
    /**
     * @brief Populate @p item.instrument from the trade's product family.
     *
     * Single dispatch point for the eight instrument families. Adding a new
     * family means adding one case here (and a new product_type enumerator).
     */
    template<typename Ctx>
    static void populate_instrument_for_trade(
        const Ctx& ctx,
        const ores::trading::domain::trade& t,
        trade_export_item& item) {
        if (!t.instrument_id || !t.product_type) return;
        const auto id = boost::uuids::to_string(*t.instrument_id);
        using ores::trading::domain::product_type;
        switch (*t.product_type) {
        case product_type::swap: {
            service::instrument_service isvc(ctx);
            if (auto r = isvc.find_instrument(id)) {
                swap_export_result ex;
                ex.instrument = std::move(*r);
                ex.legs = isvc.get_legs(id);
                item.instrument = std::move(ex);
            }
            break;
        }
        case product_type::fx: {
            service::fx_instrument_service isvc(ctx);
            if (auto r = isvc.find_fx_instrument(id))
                item.instrument = std::move(*r);
            break;
        }
        case product_type::bond: {
            service::bond_instrument_service isvc(ctx);
            if (auto r = isvc.find_bond_instrument(id))
                item.instrument = std::move(*r);
            break;
        }
        case product_type::credit: {
            service::credit_instrument_service isvc(ctx);
            if (auto r = isvc.find_credit_instrument(id))
                item.instrument = std::move(*r);
            break;
        }
        case product_type::equity: {
            service::equity_instrument_service isvc(ctx);
            if (auto r = isvc.find_equity_instrument(id))
                item.instrument = std::move(*r);
            break;
        }
        case product_type::commodity: {
            service::commodity_instrument_service isvc(ctx);
            if (auto r = isvc.find_commodity_instrument(id))
                item.instrument = std::move(*r);
            break;
        }
        case product_type::composite: {
            service::composite_instrument_service isvc(ctx);
            if (auto r = isvc.find_composite_instrument(id)) {
                composite_export_result ex;
                ex.instrument = std::move(*r);
                ex.legs = isvc.get_legs(id);
                item.instrument = std::move(ex);
            }
            break;
        }
        case product_type::scripted: {
            service::scripted_instrument_service isvc(ctx);
            if (auto r = isvc.find_scripted_instrument(id))
                item.instrument = std::move(*r);
            break;
        }
        }
    }

public:

    void list_activity_types(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        // Activity types are system-level configuration; look them up under
        // the system tenant so all tenants see the same standard set.
        const auto sys_ctx = req_ctx.with_tenant(
            ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        service::activity_type_service svc(sys_ctx);
        get_activity_types_response resp;
        try {
            resp.activity_types = svc.list_types();
        } catch (...) {}
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::trade_service svc(ctx);
        get_trades_response resp;
        try {
            if (auto req = decode<get_trades_request>(msg)) {
                const auto offset =
                    static_cast<std::uint32_t>(req->offset);
                const auto limit =
                    static_cast<std::uint32_t>(req->limit);
                if (!req->book_id.empty()) {
                    boost::uuids::string_generator gen;
                    const auto book_uuid = gen(req->book_id);
                    resp.trades = svc.list_trades_filtered(
                        offset, limit,
                        std::optional<boost::uuids::uuid>(book_uuid),
                        std::nullopt);
                    resp.total_available_count =
                        static_cast<int>(svc.count_trades_filtered(
                            std::optional<boost::uuids::uuid>(book_uuid),
                            std::nullopt));
                } else {
                    resp.trades = svc.list_trades(offset, limit);
                    resp.total_available_count =
                        static_cast<int>(svc.count_trades());
                }
            }
        } catch (...) {}
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::trades:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::trade_service svc(ctx);
        if (auto req = decode<save_trade_request>(msg)) {
            try {
                svc.save_trades(req->trades);
                BOOST_LOG_SEV(trade_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, save_trade_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(trade_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, save_trade_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(trade_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::trades:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::trade_service svc(ctx);
        if (auto req = decode<delete_trade_request>(msg)) {
            try {
                for (const auto& id : req->ids)
                    svc.remove_trade(id);
                BOOST_LOG_SEV(trade_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, delete_trade_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(trade_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, delete_trade_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(trade_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::trade_service svc(ctx);
        if (auto req = decode<get_trade_history_request>(msg)) {
            try {
                auto versions = svc.get_trade_history(req->id);
                BOOST_LOG_SEV(trade_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, get_trade_history_response{
                    .success = true,
                    .versions = std::move(versions)});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(trade_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, get_trade_history_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(trade_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    void export_portfolio(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        export_portfolio_response resp;
        try {
            if (auto req = decode<export_portfolio_request>(msg)) {
                service::trade_service svc(ctx);
                const auto offset =
                    static_cast<std::uint32_t>(req->offset);
                const auto limit =
                    static_cast<std::uint32_t>(req->limit);

                std::vector<domain::trade> trades;
                if (!req->book_id.empty()) {
                    boost::uuids::string_generator gen;
                    trades = svc.list_trades_filtered(offset, limit,
                        std::optional<boost::uuids::uuid>(gen(req->book_id)),
                        std::nullopt);
                } else if (!req->portfolio_id.empty()) {
                    boost::uuids::string_generator gen;
                    trades = svc.list_trades_filtered(offset, limit,
                        std::nullopt,
                        std::optional<boost::uuids::uuid>(gen(req->portfolio_id)));
                }

                resp.items.reserve(trades.size());
                for (auto& t : trades) {
                    trade_export_item item;
                    item.trade = t;
                    populate_instrument_for_trade(ctx, t, item);
                    resp.items.push_back(std::move(item));
                }
                resp.success = true;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(trade_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void export_trades_to_storage(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        export_trades_to_storage_response resp;
        try {
            auto req = decode<export_trades_to_storage_request>(msg);
            if (!req || req->book_ids.empty()) {
                resp.message = "Invalid request or empty book_ids.";
                reply(nats_, msg, resp);
                return;
            }

            // Fetch trades for all requested books.
            service::trade_service svc(ctx);
            std::vector<trade_export_item> all_items;
            for (const auto& bid : req->book_ids) {
                try {
                    boost::uuids::string_generator gen;
                    auto trades = svc.list_trades_filtered(0, 100000,
                        std::optional<boost::uuids::uuid>(gen(bid)),
                        std::nullopt);
                    for (auto& t : trades) {
                        trade_export_item item;
                        item.trade = t;
                        populate_instrument_for_trade(ctx, t, item);
                        all_items.push_back(std::move(item));
                    }
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(trade_handler_lg(), warn)
                        << "export_trades_to_storage: book " << bid
                        << " failed: " << e.what();
                }
            }

            // Serialise to MsgPack and upload to storage.
            const auto blob = rfl::msgpack::write(all_items);
            ores::storage::net::storage_transfer transfer(http_base_url_);
            transfer.upload_blob(req->storage_bucket, req->storage_key, blob);

            resp.success = true;
            resp.trade_count = static_cast<int>(all_items.size());
            resp.storage_key = req->storage_key;
            resp.message = "Exported " + std::to_string(all_items.size())
                + " trades to storage.";

            BOOST_LOG_SEV(trade_handler_lg(), info)
                << "export_trades_to_storage: exported "
                << all_items.size() << " trades, "
                << blob.size() << " bytes (pre-compression) to "
                << req->storage_bucket << "/" << req->storage_key;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(trade_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    std::string http_base_url_;
};

} // namespace ores::trading::messaging

#endif
