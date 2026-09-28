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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_nats_handler.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_CORE_MESSAGING_TRADE_HANDLER_HPP
#define ORES_TRADING_CORE_MESSAGING_TRADE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/callable_swap_call_date_repository.hpp"
#include "ores.trading.core/repository/commodity_basket_constituent_repository.hpp"
#include "ores.trading.core/repository/composite_leg_repository.hpp"
#include "ores.trading.core/repository/equity_position_option_underlying_repository.hpp"
#include "ores.trading.core/repository/swap_leg_repository.hpp"
#include "ores.trading.core/service/balance_guaranteed_swap_instrument_service.hpp"
#include "ores.trading.core/service/bond_instrument_reader.hpp"
#include "ores.trading.core/service/callable_swap_instrument_service.hpp"
#include "ores.trading.core/service/cap_floor_instrument_service.hpp"
#include "ores.trading.core/service/commodity_instrument_service.hpp"
#include "ores.trading.core/service/composite_instrument_service.hpp"
#include "ores.trading.core/service/credit_instrument_service.hpp"
#include "ores.trading.core/service/equity_accumulator_instrument_service.hpp"
#include "ores.trading.core/service/equity_asian_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_barrier_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_digital_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_forward_instrument_service.hpp"
#include "ores.trading.core/service/equity_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_position_instrument_service.hpp"
#include "ores.trading.core/service/equity_swap_instrument_service.hpp"
#include "ores.trading.core/service/equity_variance_swap_instrument_service.hpp"
#include "ores.trading.core/service/fra_instrument_service.hpp"
#include "ores.trading.core/service/fx_accumulator_instrument_service.hpp"
#include "ores.trading.core/service/fx_asian_forward_instrument_service.hpp"
#include "ores.trading.core/service/fx_barrier_option_instrument_service.hpp"
#include "ores.trading.core/service/fx_digital_option_instrument_service.hpp"
#include "ores.trading.core/service/fx_forward_instrument_service.hpp"
#include "ores.trading.core/service/fx_vanilla_option_instrument_service.hpp"
#include "ores.trading.core/service/fx_variance_swap_instrument_service.hpp"
#include "ores.trading.core/service/inflation_swap_instrument_service.hpp"
#include "ores.trading.core/service/knock_out_swap_instrument_service.hpp"
#include "ores.trading.core/service/rpa_instrument_service.hpp"
#include "ores.trading.core/service/scripted_instrument_service.hpp"
#include "ores.trading.core/service/swaption_instrument_service.hpp"
#include "ores.trading.core/service/trade_envelope_reader.hpp"
#include "ores.trading.core/service/trade_service.hpp"
#include "ores.trading.core/service/vanilla_swap_instrument_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/string_generator.hpp>
#include <chrono>
#include <optional>
#include <rfl/msgpack.hpp>
#include <unordered_map>
#include <unordered_set>

namespace ores::trading::messaging {

namespace {
inline auto& trade_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.trading.messaging.trade_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for trade operations.
 */
class trade_handler {
public:
    trade_handler(ores::nats::service::client& nats,
                  ores::database::context ctx,
                  std::optional<ores::security::jwt::jwt_authenticator> verifier,
                  std::string http_base_url = {})
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , http_base_url_(std::move(http_base_url)) {}

    /**
     * @brief Serves trading.v1.trades.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_trades(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<list_trades_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(trade_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::trade_service svc(req_ctx);
        try {
            auto response = svc.list_trades(*req);
            BOOST_LOG_SEV(trade_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            list_trades_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.trades.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_trade(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<get_trade_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(trade_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::trade_service svc(req_ctx);
        try {
            auto response = svc.get_trade(*req);
            BOOST_LOG_SEV(trade_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_trade_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.trades.get_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_many_trades(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<get_many_trades_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(trade_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::trade_service svc(req_ctx);
        try {
            auto response = svc.get_many_trades(*req);
            BOOST_LOG_SEV(trade_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_many_trades_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.trades.put.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_trade(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
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
        auto req = decode<put_trade_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(trade_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::trade_service svc(req_ctx);
        try {
            auto response = svc.put_trade(*req);
            BOOST_LOG_SEV(trade_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            put_trade_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.trades.put_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_many_trades(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
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
        auto req = decode<put_many_trades_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(trade_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::trade_service svc(req_ctx);
        try {
            auto response = svc.put_many_trades(*req);
            BOOST_LOG_SEV(trade_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            put_many_trades_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.trades.delete.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_trade(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::trades:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_trade_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(trade_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::trade_service svc(req_ctx);
        try {
            auto response = svc.delete_trade(*req);
            BOOST_LOG_SEV(trade_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            delete_trade_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.trades.delete_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_many_trades(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::trades:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_trades_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(trade_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::trade_service svc(req_ctx);
        try {
            auto response = svc.delete_many_trades(*req);
            BOOST_LOG_SEV(trade_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            delete_many_trades_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.trades_versions.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_trade_versions(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<list_trade_versions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(trade_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::trade_service svc(req_ctx);
        try {
            auto response = svc.list_trade_versions(*req);
            BOOST_LOG_SEV(trade_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            list_trade_versions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.trades_versions.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_trade_version(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<get_trade_version_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(trade_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::trade_service svc(req_ctx);
        try {
            auto response = svc.get_trade_version(*req);
            BOOST_LOG_SEV(trade_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_trade_version_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    void instrument(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::trade_service svc(ctx);
        get_trade_instrument_response resp;
        try {
            if (auto req = decode<get_trade_instrument_request>(msg)) {
                auto trade_opt = svc.get_trade(boost::uuids::string_generator()(req->trade_id));
                if (!trade_opt) {
                    resp.success = false;
                    resp.message = "Trade not found: " + req->trade_id;
                } else {
                    std::vector<trade_export_item> items{{.trade = std::move(*trade_opt)}};
                    populate_instruments_for_trades(ctx, items);
                    resp.trade = std::move(items[0].trade);
                    resp.instrument = decode_instrument(items[0].instrument);
                    resp.success = true;
                }
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void export_portfolio(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        export_portfolio_response resp;
        try {
            if (auto req = decode<export_portfolio_request>(msg)) {
                service::trade_service svc(ctx);
                const auto offset = static_cast<std::uint32_t>(req->offset);
                const auto limit = static_cast<std::uint32_t>(req->limit);

                auto trades = svc.list_trades(offset, limit, req->node_id);
                resp.items.reserve(trades.size());
                for (auto& t : trades)
                    resp.items.push_back({.trade = std::move(t)});
                populate_instruments_for_trades(ctx, resp.items);
                resp.success = true;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void export_trades_to_storage(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
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

            // Fetch trades for all requested books in pages and batch-populate instruments.
            service::trade_service svc(ctx);
            std::vector<trade_export_item> all_items;
            constexpr std::uint32_t page_size = 1000;
            for (const auto& bid : req->book_ids) {
                try {
                    std::uint32_t offset = 0;
                    while (true) {
                        auto trades = svc.list_trades(offset, page_size, bid);
                        const auto n = static_cast<std::uint32_t>(trades.size());
                        if (n == 0)
                            break;
                        std::vector<trade_export_item> page;
                        page.reserve(n);
                        for (auto& t : trades)
                            page.push_back({.trade = std::move(t)});
                        populate_instruments_for_trades(ctx, page);
                        all_items.insert(all_items.end(),
                                         std::make_move_iterator(page.begin()),
                                         std::make_move_iterator(page.end()));
                        offset += n;
                        if (n < page_size)
                            break;
                    }
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(trade_handler_lg(), warn)
                        << "export_trades_to_storage: book " << bid << " failed: " << e.what();
                }
            }

            // Serialise to MsgPack and upload to storage.
            const auto blob = rfl::msgpack::write(all_items);
            ores::storage::net::storage_transfer transfer(http_base_url_, extract_bearer(msg));
            transfer.upload_blob(req->storage_bucket, req->storage_key, blob);

            resp.success = true;
            resp.trade_count = static_cast<int>(all_items.size());
            resp.storage_key = req->storage_key;
            resp.message = "Exported " + std::to_string(all_items.size()) + " trades to storage.";

            BOOST_LOG_SEV(trade_handler_lg(), info)
                << "export_trades_to_storage: exported " << all_items.size() << " trades, "
                << blob.size() << " bytes (pre-compression) to " << req->storage_bucket << "/"
                << req->storage_key;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(trade_handler_lg(), error) << msg.subject << " failed: " << e.what();
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    template <typename Ctx>
    static void populate_instruments_for_trades(const Ctx& ctx,
                                                std::vector<trade_export_item>& items) {
        using ores::trading::domain::product_type;
        using ores::trading::domain::trade_instrument;
        using ores::trading::domain::swap_instrument_data;
        using ores::trading::domain::commodity_instrument_data;
        using ores::trading::domain::composite_instrument_data;

        // Phase 1: bucket instrument IDs by (product_type, trade_type)
        std::vector<std::string> bond_ids, credit_ids, commodity_ids, scripted_ids, composite_ids,
            fra_ids, vswap_ids, capfloor_ids, swaption_ids, bgs_ids, callable_ids, koswap_ids,
            infl_ids, rpa_ids, fxfwd_ids, fxopt_ids, fxbar_ids, fxdig_ids, fxasn_ids, fxacc_ids,
            fxvar_ids, eq_opt_ids, eq_fwd_ids, eq_swp_ids, eq_var_ids, eq_bar_ids, eq_asn_ids,
            eq_dig_ids, eq_acc_ids, eq_pos_ids;

        for (const auto& item : items) {
            const auto& t = item.trade;
            if (!t.classification.instrument_id ||
                t.classification.product_type == product_type::unknown)
                continue;
            const auto id = boost::uuids::to_string(*t.classification.instrument_id);
            const auto& ttc = t.classification.trade_type;
            switch (t.classification.product_type) {
                case product_type::bond:
                    bond_ids.push_back(id);
                    break;
                case product_type::credit:
                    credit_ids.push_back(id);
                    break;
                case product_type::commodity:
                    commodity_ids.push_back(id);
                    break;
                case product_type::scripted:
                    scripted_ids.push_back(id);
                    break;
                case product_type::composite:
                    composite_ids.push_back(id);
                    break;
                case product_type::swap:
                    if (ttc == "ForwardRateAgreement")
                        fra_ids.push_back(id);
                    else if (ttc == "Swap" || ttc == "CrossCurrencySwap" || ttc == "FlexiSwap")
                        vswap_ids.push_back(id);
                    else if (ttc == "CapFloor")
                        capfloor_ids.push_back(id);
                    else if (ttc == "Swaption")
                        swaption_ids.push_back(id);
                    else if (ttc == "BalanceGuaranteedSwap")
                        bgs_ids.push_back(id);
                    else if (ttc == "CallableSwap")
                        callable_ids.push_back(id);
                    else if (ttc == "KnockOutSwap")
                        koswap_ids.push_back(id);
                    else if (ttc == "InflationSwap")
                        infl_ids.push_back(id);
                    else if (ttc == "RiskParticipationAgreement")
                        rpa_ids.push_back(id);
                    break;
                case product_type::fx:
                    if (ttc == "FxForward" || ttc == "FxSwap")
                        fxfwd_ids.push_back(id);
                    else if (ttc == "FxOption")
                        fxopt_ids.push_back(id);
                    else if (ttc == "FxBarrierOption" || ttc == "FxGenericBarrierOption" ||
                             ttc == "FxDoubleBarrierOption" || ttc == "FxEuropeanBarrierOption" ||
                             ttc == "FxKIKOBarrierOption")
                        fxbar_ids.push_back(id);
                    else if (ttc == "FxDigitalOption" || ttc == "FxDigitalBarrierOption" ||
                             ttc == "FxTouchOption" || ttc == "FxDoubleTouchOption")
                        fxdig_ids.push_back(id);
                    else if (ttc == "FxAverageForward" || ttc == "FxTaRF")
                        fxasn_ids.push_back(id);
                    else if (ttc == "FxAccumulator")
                        fxacc_ids.push_back(id);
                    else if (ttc == "FxVarianceSwap")
                        fxvar_ids.push_back(id);
                    break;
                case product_type::equity:
                    if (ttc == "EquityOption" || ttc == "EquityCliquetOption" ||
                        ttc == "EquityOutperformanceOption")
                        eq_opt_ids.push_back(id);
                    else if (ttc == "EquityForward")
                        eq_fwd_ids.push_back(id);
                    else if (ttc == "EquitySwap" || ttc == "EquityWorstOfBasketSwap")
                        eq_swp_ids.push_back(id);
                    else if (ttc == "EquityVarianceSwap")
                        eq_var_ids.push_back(id);
                    else if (ttc == "EquityBarrierOption" || ttc == "EquityDoubleBarrierOption" ||
                             ttc == "EquityEuropeanBarrierOption")
                        eq_bar_ids.push_back(id);
                    else if (ttc == "EquityAsianOption")
                        eq_asn_ids.push_back(id);
                    else if (ttc == "EquityDigitalOption" || ttc == "EquityTouchOption")
                        eq_dig_ids.push_back(id);
                    else if (ttc == "EquityAccumulator" || ttc == "EquityTaRF")
                        eq_acc_ids.push_back(id);
                    else if (ttc == "EquityPosition")
                        eq_pos_ids.push_back(id);
                    break;
                case product_type::unknown:
                    break;
            }
        }

        // Phase 2: batch-fetch legs (one call covers all swap types)
        std::unordered_map<std::string, std::vector<ores::trading::domain::swap_leg>> legs_map;
        {
            std::vector<std::string> all_swap;
            for (auto* v : {&fra_ids,
                            &vswap_ids,
                            &capfloor_ids,
                            &swaption_ids,
                            &bgs_ids,
                            &callable_ids,
                            &koswap_ids,
                            &infl_ids,
                            &rpa_ids})
                all_swap.insert(all_swap.end(), v->begin(), v->end());
            if (!all_swap.empty()) {
                repository::swap_leg_repository leg_repo;
                for (auto& leg : leg_repo.read_by_instruments_batch(ctx, all_swap))
                    legs_map[boost::uuids::to_string(leg.identity.instrument_id)].push_back(
                        std::move(leg));
            }
        }
        std::unordered_map<std::string, std::vector<ores::trading::domain::composite_leg>>
            comp_legs_map;
        if (!composite_ids.empty()) {
            repository::composite_leg_repository comp_leg_repo;
            const std::unordered_set<std::string> wanted(composite_ids.begin(),
                                                         composite_ids.end());
            for (auto& leg : comp_leg_repo.read_latest(ctx)) {
                const auto key = boost::uuids::to_string(leg.identity.instrument_id);
                if (wanted.contains(key))
                    comp_legs_map[key].push_back(std::move(leg));
            }
        }

        // The callable swap's exercise schedule is a collection of its own,
        // so it is fetched beside the legs and only for the instruments
        // that state one.
        std::unordered_map<std::string, std::vector<ores::trading::domain::callable_swap_call_date>>
            call_dates_map;
        if (!callable_ids.empty()) {
            repository::callable_swap_call_date_repository call_date_repo;
            for (auto& call_date : call_date_repo.read_by_instruments_batch(ctx, callable_ids))
                call_dates_map[boost::uuids::to_string(call_date.instrument_id)].push_back(
                    std::move(call_date));
        }

        // Phase 3: batch-fetch instruments, build lookup map
        std::unordered_map<std::string, trade_instrument> imap;

        auto take_legs = [&](const std::string& id) {
            auto it = legs_map.find(id);
            return it != legs_map.end() ? std::move(it->second) :
                                          std::vector<ores::trading::domain::swap_leg>{};
        };

        auto take_call_dates = [&](const std::string& id) {
            auto it = call_dates_map.find(id);
            return it != call_dates_map.end() ?
                       std::move(it->second) :
                       std::vector<ores::trading::domain::callable_swap_call_date>{};
        };

        // A commodity basket's constituents are a collection of their own, so
        // they are fetched beside the instrument and only for the instruments
        // that state one.
        std::unordered_map<std::string,
                           std::vector<ores::trading::domain::commodity_basket_constituent>>
            constituents_map;
        if (!commodity_ids.empty()) {
            repository::commodity_basket_constituent_repository constituent_repo;
            for (auto& constituent : constituent_repo.read_by_instruments_batch(ctx, commodity_ids))
                constituents_map[boost::uuids::to_string(constituent.instrument_id)].push_back(
                    std::move(constituent));
        }

        auto take_constituents = [&](const std::string& id) {
            auto it = constituents_map.find(id);
            return it != constituents_map.end() ?
                       std::move(it->second) :
                       std::vector<ores::trading::domain::commodity_basket_constituent>{};
        };

        // Single-table types (credit, scripted).
        auto add_flat = [&](auto&& results) {
            for (auto& v : results)
                imap[boost::uuids::to_string(v.identity.instrument_id)] = std::move(v);
        };

        if (!bond_ids.empty()) {
            service::bond_instrument_reader reader(ctx);
            for (auto& [id, data] : reader.read_instruments(bond_ids))
                imap[id] = std::move(data);
        }
        if (!credit_ids.empty()) {
            service::credit_instrument_service svc(ctx);
            add_flat(svc.get_credit_instruments(credit_ids));
        }
        if (!commodity_ids.empty()) {
            service::commodity_instrument_service svc(ctx);
            for (auto& v : svc.get_commodity_instruments(commodity_ids)) {
                const auto id = boost::uuids::to_string(v.identity.instrument_id);
                commodity_instrument_data data;
                data.instrument = std::move(v);
                data.constituents = take_constituents(id);
                imap[id] = std::move(data);
            }
        }
        if (!scripted_ids.empty()) {
            service::scripted_instrument_service svc(ctx);
            add_flat(svc.get_scripted_instruments(scripted_ids));
        }
        if (!composite_ids.empty()) {
            service::composite_instrument_service svc(ctx);
            for (auto& v : svc.get_composite_instruments(composite_ids)) {
                const auto id = boost::uuids::to_string(v.identity.instrument_id);
                composite_instrument_data data;
                data.instrument = std::move(v);
                auto it = comp_legs_map.find(id);
                if (it != comp_legs_map.end())
                    data.legs = std::move(it->second);
                imap[id] = std::move(data);
            }
        }

        // Rates / swap types (9 sub-types, all share swap_legs table)
        auto add_swap = [&](auto&& results) {
            for (auto& v : results) {
                const auto id = boost::uuids::to_string(v.identity.instrument_id);
                swap_instrument_data data;
                data.instrument = std::move(v);
                data.legs = take_legs(id);
                data.call_dates = take_call_dates(id);
                imap[id] = std::move(data);
            }
        };
        if (!fra_ids.empty()) {
            service::fra_instrument_service svc(ctx);
            add_swap(svc.get_fra_instruments(fra_ids));
        }
        if (!vswap_ids.empty()) {
            service::vanilla_swap_instrument_service svc(ctx);
            add_swap(svc.get_vanilla_swap_instruments(vswap_ids));
        }
        if (!capfloor_ids.empty()) {
            service::cap_floor_instrument_service svc(ctx);
            add_swap(svc.get_cap_floor_instruments(capfloor_ids));
        }
        if (!swaption_ids.empty()) {
            service::swaption_instrument_service svc(ctx);
            add_swap(svc.get_swaption_instruments(swaption_ids));
        }
        if (!bgs_ids.empty()) {
            service::balance_guaranteed_swap_instrument_service svc(ctx);
            add_swap(svc.get_balance_guaranteed_swap_instruments(bgs_ids));
        }
        if (!callable_ids.empty()) {
            service::callable_swap_instrument_service svc(ctx);
            add_swap(svc.get_callable_swap_instruments(callable_ids));
        }
        if (!koswap_ids.empty()) {
            service::knock_out_swap_instrument_service svc(ctx);
            add_swap(svc.get_knock_out_swap_instruments(koswap_ids));
        }
        if (!infl_ids.empty()) {
            service::inflation_swap_instrument_service svc(ctx);
            add_swap(svc.get_inflation_swap_instruments(infl_ids));
        }
        if (!rpa_ids.empty()) {
            service::rpa_instrument_service svc(ctx);
            add_swap(svc.get_rpa_instruments(rpa_ids));
        }

        // FX types
        auto add_fx = [&](auto&& results) {
            for (auto& v : results)
                imap[boost::uuids::to_string(v.identity.instrument_id)] =
                    ores::trading::domain::fx_instrument_variant{std::move(v)};
        };
        if (!fxfwd_ids.empty()) {
            service::fx_forward_instrument_service svc(ctx);
            add_fx(svc.get_fx_forward_instruments(fxfwd_ids));
        }
        if (!fxopt_ids.empty()) {
            service::fx_vanilla_option_instrument_service svc(ctx);
            add_fx(svc.get_fx_vanilla_option_instruments(fxopt_ids));
        }
        if (!fxbar_ids.empty()) {
            service::fx_barrier_option_instrument_service svc(ctx);
            add_fx(svc.get_fx_barrier_option_instruments(fxbar_ids));
        }
        if (!fxdig_ids.empty()) {
            service::fx_digital_option_instrument_service svc(ctx);
            add_fx(svc.get_fx_digital_option_instruments(fxdig_ids));
        }
        if (!fxasn_ids.empty()) {
            service::fx_asian_forward_instrument_service svc(ctx);
            add_fx(svc.get_fx_asian_forward_instruments(fxasn_ids));
        }
        if (!fxacc_ids.empty()) {
            service::fx_accumulator_instrument_service svc(ctx);
            add_fx(svc.get_fx_accumulator_instruments(fxacc_ids));
        }
        if (!fxvar_ids.empty()) {
            service::fx_variance_swap_instrument_service svc(ctx);
            add_fx(svc.get_fx_variance_swap_instruments(fxvar_ids));
        }

        // Equity types. An equity option position states its entries as rows
        // of their own, so they are fetched beside the instrument and only
        // for the positions that state one.
        std::unordered_map<std::string,
                           std::vector<ores::trading::domain::equity_position_option_underlying>>
            equity_underlyings_map;
        if (!eq_pos_ids.empty()) {
            repository::equity_position_option_underlying_repository underlying_repo;
            for (auto& underlying : underlying_repo.read_by_instruments_batch(ctx, eq_pos_ids))
                equity_underlyings_map[boost::uuids::to_string(underlying.instrument_id)].push_back(
                    std::move(underlying));
        }

        auto take_equity_underlyings = [&](const std::string& id) {
            auto it = equity_underlyings_map.find(id);
            return it != equity_underlyings_map.end() ?
                       std::move(it->second) :
                       std::vector<ores::trading::domain::equity_position_option_underlying>{};
        };

        auto add_eq = [&](auto&& results) {
            for (auto& v : results) {
                const auto id = boost::uuids::to_string(v.identity.instrument_id);
                ores::trading::domain::equity_instrument_data data;
                data.instrument = ores::trading::domain::equity_instrument_variant{std::move(v)};
                data.underlyings = take_equity_underlyings(id);
                imap[id] = std::move(data);
            }
        };
        if (!eq_opt_ids.empty()) {
            service::equity_option_instrument_service svc(ctx);
            add_eq(svc.get_equity_option_instruments(eq_opt_ids));
        }
        if (!eq_fwd_ids.empty()) {
            service::equity_forward_instrument_service svc(ctx);
            add_eq(svc.get_equity_forward_instruments(eq_fwd_ids));
        }
        if (!eq_swp_ids.empty()) {
            service::equity_swap_instrument_service svc(ctx);
            add_eq(svc.get_equity_swap_instruments(eq_swp_ids));
        }
        if (!eq_var_ids.empty()) {
            service::equity_variance_swap_instrument_service svc(ctx);
            add_eq(svc.get_equity_variance_swap_instruments(eq_var_ids));
        }
        if (!eq_bar_ids.empty()) {
            service::equity_barrier_option_instrument_service svc(ctx);
            add_eq(svc.get_equity_barrier_option_instruments(eq_bar_ids));
        }
        if (!eq_asn_ids.empty()) {
            service::equity_asian_option_instrument_service svc(ctx);
            add_eq(svc.get_equity_asian_option_instruments(eq_asn_ids));
        }
        if (!eq_dig_ids.empty()) {
            service::equity_digital_option_instrument_service svc(ctx);
            add_eq(svc.get_equity_digital_option_instruments(eq_dig_ids));
        }
        if (!eq_acc_ids.empty()) {
            service::equity_accumulator_instrument_service svc(ctx);
            add_eq(svc.get_equity_accumulator_instruments(eq_acc_ids));
        }
        if (!eq_pos_ids.empty()) {
            service::equity_position_instrument_service svc(ctx);
            add_eq(svc.get_equity_position_instruments(eq_pos_ids));
        }

        // Phase 4: fill items from lookup map (copy — multiple items may share an instrument)
        for (auto& item : items) {
            const auto& t = item.trade;
            if (!t.classification.instrument_id ||
                t.classification.product_type == product_type::unknown)
                continue;
            const auto id = boost::uuids::to_string(*t.classification.instrument_id);
            if (auto it = imap.find(id); it != imap.end())
                item.instrument = encode_instrument(it->second);
        }

        // Phase 5: fill the trade-level envelope, which is keyed by the
        // trade rather than the instrument and so crosses product types.
        std::vector<std::string> trade_ids;
        trade_ids.reserve(items.size());
        for (const auto& item : items)
            trade_ids.push_back(boost::uuids::to_string(item.trade.identity.id));

        service::trade_envelope_reader envelope_reader(ctx);
        auto envelopes = envelope_reader.read_envelopes(trade_ids);
        for (auto& item : items) {
            const auto id = boost::uuids::to_string(item.trade.identity.id);
            if (auto it = envelopes.find(id); it != envelopes.end())
                item.envelope = std::move(it->second);
        }
    }

    // Extract the raw JWT from an incoming message, preferring the delegated
    // header so the original end-user context propagates to downstream calls.
    static std::string extract_bearer(const ores::nats::message& msg) {
        using namespace ores::nats::headers;
        for (auto hdr : {delegated_authorization, authorization}) {
            const auto it = msg.headers.find(std::string(hdr));
            if (it != msg.headers.end() && it->second.starts_with(bearer_prefix))
                return std::string(it->second.substr(bearer_prefix.size()));
        }
        return {};
    }

    std::string http_base_url_;
};

} // namespace ores::trading::messaging

#endif
