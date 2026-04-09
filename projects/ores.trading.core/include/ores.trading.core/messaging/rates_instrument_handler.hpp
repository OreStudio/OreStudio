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
#ifndef ORES_TRADING_MESSAGING_RATES_INSTRUMENT_HANDLER_HPP
#define ORES_TRADING_MESSAGING_RATES_INSTRUMENT_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.trading.core/service/fra_instrument_service.hpp"
#include "ores.trading.core/service/vanilla_swap_instrument_service.hpp"
#include "ores.trading.core/service/cap_floor_instrument_service.hpp"
#include "ores.trading.core/service/swaption_instrument_service.hpp"
#include "ores.trading.core/service/balance_guaranteed_swap_instrument_service.hpp"
#include "ores.trading.core/service/callable_swap_instrument_service.hpp"
#include "ores.trading.core/service/knock_out_swap_instrument_service.hpp"
#include "ores.trading.core/service/inflation_swap_instrument_service.hpp"
#include "ores.trading.core/service/rpa_instrument_service.hpp"

namespace ores::trading::messaging {

namespace {
inline auto& rates_instrument_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.trading.messaging.rates_instrument_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS handler for all rates/swap instrument families.
 *
 * Covers FRA, VanillaSwap, CapFloor, Swaption, BalanceGuaranteedSwap,
 * CallableSwap, KnockOutSwap, InflationSwap, and RPA instruments.
 */
class rates_instrument_handler {
public:
    rates_instrument_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    // ---- FRA ----

    void list_fra(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::fra_instrument_service svc(*ctx_expected);
        get_fra_instruments_response resp;
        try {
            if (auto req = decode<get_fra_instruments_request>(msg)) {
                resp.instruments = svc.list_fra_instruments(
                    static_cast<std::uint32_t>(req->offset),
                    static_cast<std::uint32_t>(req->limit));
                resp.total_available_count = static_cast<int>(svc.count_fra_instruments());
            }
        } catch (const std::exception& e) {
            resp.success = false; resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void save_fra(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::fra_instrument_service svc(ctx);
        if (auto req = decode<save_fra_instrument_request>(msg)) {
            try {
                svc.save_fra_instrument(req->data);
                reply(nats_, msg, save_fra_instrument_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_fra_instrument_response{.success = false, .message = e.what()});
            }
        }
    }

    void remove_fra(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::fra_instrument_service svc(ctx);
        if (auto req = decode<delete_fra_instrument_request>(msg)) {
            delete_fra_instrument_response resp;
            resp.success = true;
            for (const auto& id : req->ids) {
                try {
                    svc.remove_fra_instrument(id);
                    resp.results.push_back({id, {true, ""}});
                } catch (const std::exception& e) {
                    resp.results.push_back({id, {false, e.what()}});
                    resp.success = false;
                    resp.message = "One or more deletions failed";
                }
            }
            reply(nats_, msg, resp);
        }
    }

    void history_fra(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::fra_instrument_service svc(*ctx_expected);
        if (auto req = decode<get_fra_instrument_history_request>(msg)) {
            try {
                reply(nats_, msg, get_fra_instrument_history_response{
                    .success = true,
                    .history = svc.get_fra_instrument_history(req->id)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_fra_instrument_history_response{
                    .success = false, .message = e.what()});
            }
        }
    }

    // ---- Vanilla Swap ----

    void list_vanilla_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::vanilla_swap_instrument_service svc(*ctx_expected);
        get_vanilla_swap_instruments_response resp;
        try {
            if (auto req = decode<get_vanilla_swap_instruments_request>(msg)) {
                resp.instruments = svc.list_vanilla_swap_instruments(
                    static_cast<std::uint32_t>(req->offset),
                    static_cast<std::uint32_t>(req->limit));
                resp.total_available_count = static_cast<int>(svc.count_vanilla_swap_instruments());
            }
        } catch (const std::exception& e) {
            resp.success = false; resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void save_vanilla_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::vanilla_swap_instrument_service svc(ctx);
        if (auto req = decode<save_vanilla_swap_instrument_request>(msg)) {
            try {
                svc.save_vanilla_swap_instrument(req->data);
                reply(nats_, msg, save_vanilla_swap_instrument_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_vanilla_swap_instrument_response{.success = false, .message = e.what()});
            }
        }
    }

    void remove_vanilla_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::vanilla_swap_instrument_service svc(ctx);
        if (auto req = decode<delete_vanilla_swap_instrument_request>(msg)) {
            delete_vanilla_swap_instrument_response resp;
            resp.success = true;
            for (const auto& id : req->ids) {
                try {
                    svc.remove_vanilla_swap_instrument(id);
                    resp.results.push_back({id, {true, ""}});
                } catch (const std::exception& e) {
                    resp.results.push_back({id, {false, e.what()}});
                    resp.success = false;
                    resp.message = "One or more deletions failed";
                }
            }
            reply(nats_, msg, resp);
        }
    }

    void history_vanilla_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::vanilla_swap_instrument_service svc(*ctx_expected);
        if (auto req = decode<get_vanilla_swap_instrument_history_request>(msg)) {
            try {
                reply(nats_, msg, get_vanilla_swap_instrument_history_response{
                    .success = true,
                    .history = svc.get_vanilla_swap_instrument_history(req->id)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_vanilla_swap_instrument_history_response{
                    .success = false, .message = e.what()});
            }
        }
    }

    // ---- Cap/Floor ----

    void list_cap_floor(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::cap_floor_instrument_service svc(*ctx_expected);
        get_cap_floor_instruments_response resp;
        try {
            if (auto req = decode<get_cap_floor_instruments_request>(msg)) {
                resp.instruments = svc.list_cap_floor_instruments(
                    static_cast<std::uint32_t>(req->offset),
                    static_cast<std::uint32_t>(req->limit));
                resp.total_available_count = static_cast<int>(svc.count_cap_floor_instruments());
            }
        } catch (const std::exception& e) {
            resp.success = false; resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void save_cap_floor(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::cap_floor_instrument_service svc(ctx);
        if (auto req = decode<save_cap_floor_instrument_request>(msg)) {
            try {
                svc.save_cap_floor_instrument(req->data);
                reply(nats_, msg, save_cap_floor_instrument_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_cap_floor_instrument_response{.success = false, .message = e.what()});
            }
        }
    }

    void remove_cap_floor(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::cap_floor_instrument_service svc(ctx);
        if (auto req = decode<delete_cap_floor_instrument_request>(msg)) {
            delete_cap_floor_instrument_response resp;
            resp.success = true;
            for (const auto& id : req->ids) {
                try {
                    svc.remove_cap_floor_instrument(id);
                    resp.results.push_back({id, {true, ""}});
                } catch (const std::exception& e) {
                    resp.results.push_back({id, {false, e.what()}});
                    resp.success = false;
                    resp.message = "One or more deletions failed";
                }
            }
            reply(nats_, msg, resp);
        }
    }

    void history_cap_floor(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::cap_floor_instrument_service svc(*ctx_expected);
        if (auto req = decode<get_cap_floor_instrument_history_request>(msg)) {
            try {
                reply(nats_, msg, get_cap_floor_instrument_history_response{
                    .success = true,
                    .history = svc.get_cap_floor_instrument_history(req->id)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_cap_floor_instrument_history_response{
                    .success = false, .message = e.what()});
            }
        }
    }

    // ---- Swaption ----

    void list_swaption(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::swaption_instrument_service svc(*ctx_expected);
        get_swaption_instruments_response resp;
        try {
            if (auto req = decode<get_swaption_instruments_request>(msg)) {
                resp.instruments = svc.list_swaption_instruments(
                    static_cast<std::uint32_t>(req->offset),
                    static_cast<std::uint32_t>(req->limit));
                resp.total_available_count = static_cast<int>(svc.count_swaption_instruments());
            }
        } catch (const std::exception& e) {
            resp.success = false; resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void save_swaption(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::swaption_instrument_service svc(ctx);
        if (auto req = decode<save_swaption_instrument_request>(msg)) {
            try {
                svc.save_swaption_instrument(req->data);
                reply(nats_, msg, save_swaption_instrument_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_swaption_instrument_response{.success = false, .message = e.what()});
            }
        }
    }

    void remove_swaption(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::swaption_instrument_service svc(ctx);
        if (auto req = decode<delete_swaption_instrument_request>(msg)) {
            delete_swaption_instrument_response resp;
            resp.success = true;
            for (const auto& id : req->ids) {
                try {
                    svc.remove_swaption_instrument(id);
                    resp.results.push_back({id, {true, ""}});
                } catch (const std::exception& e) {
                    resp.results.push_back({id, {false, e.what()}});
                    resp.success = false;
                    resp.message = "One or more deletions failed";
                }
            }
            reply(nats_, msg, resp);
        }
    }

    void history_swaption(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::swaption_instrument_service svc(*ctx_expected);
        if (auto req = decode<get_swaption_instrument_history_request>(msg)) {
            try {
                reply(nats_, msg, get_swaption_instrument_history_response{
                    .success = true,
                    .history = svc.get_swaption_instrument_history(req->id)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_swaption_instrument_history_response{
                    .success = false, .message = e.what()});
            }
        }
    }

    // ---- Balance Guaranteed Swap ----

    void list_balance_guaranteed_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::balance_guaranteed_swap_instrument_service svc(*ctx_expected);
        get_balance_guaranteed_swap_instruments_response resp;
        try {
            if (auto req = decode<get_balance_guaranteed_swap_instruments_request>(msg)) {
                resp.instruments = svc.list_balance_guaranteed_swap_instruments(
                    static_cast<std::uint32_t>(req->offset),
                    static_cast<std::uint32_t>(req->limit));
                resp.total_available_count = static_cast<int>(svc.count_balance_guaranteed_swap_instruments());
            }
        } catch (const std::exception& e) {
            resp.success = false; resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void save_balance_guaranteed_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::balance_guaranteed_swap_instrument_service svc(ctx);
        if (auto req = decode<save_balance_guaranteed_swap_instrument_request>(msg)) {
            try {
                svc.save_balance_guaranteed_swap_instrument(req->data);
                reply(nats_, msg, save_balance_guaranteed_swap_instrument_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_balance_guaranteed_swap_instrument_response{.success = false, .message = e.what()});
            }
        }
    }

    void remove_balance_guaranteed_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::balance_guaranteed_swap_instrument_service svc(ctx);
        if (auto req = decode<delete_balance_guaranteed_swap_instrument_request>(msg)) {
            delete_balance_guaranteed_swap_instrument_response resp;
            resp.success = true;
            for (const auto& id : req->ids) {
                try {
                    svc.remove_balance_guaranteed_swap_instrument(id);
                    resp.results.push_back({id, {true, ""}});
                } catch (const std::exception& e) {
                    resp.results.push_back({id, {false, e.what()}});
                    resp.success = false;
                    resp.message = "One or more deletions failed";
                }
            }
            reply(nats_, msg, resp);
        }
    }

    void history_balance_guaranteed_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::balance_guaranteed_swap_instrument_service svc(*ctx_expected);
        if (auto req = decode<get_balance_guaranteed_swap_instrument_history_request>(msg)) {
            try {
                reply(nats_, msg, get_balance_guaranteed_swap_instrument_history_response{
                    .success = true,
                    .history = svc.get_balance_guaranteed_swap_instrument_history(req->id)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_balance_guaranteed_swap_instrument_history_response{
                    .success = false, .message = e.what()});
            }
        }
    }

    // ---- Callable Swap ----

    void list_callable_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::callable_swap_instrument_service svc(*ctx_expected);
        get_callable_swap_instruments_response resp;
        try {
            if (auto req = decode<get_callable_swap_instruments_request>(msg)) {
                resp.instruments = svc.list_callable_swap_instruments(
                    static_cast<std::uint32_t>(req->offset),
                    static_cast<std::uint32_t>(req->limit));
                resp.total_available_count = static_cast<int>(svc.count_callable_swap_instruments());
            }
        } catch (const std::exception& e) {
            resp.success = false; resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void save_callable_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::callable_swap_instrument_service svc(ctx);
        if (auto req = decode<save_callable_swap_instrument_request>(msg)) {
            try {
                svc.save_callable_swap_instrument(req->data);
                reply(nats_, msg, save_callable_swap_instrument_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_callable_swap_instrument_response{.success = false, .message = e.what()});
            }
        }
    }

    void remove_callable_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::callable_swap_instrument_service svc(ctx);
        if (auto req = decode<delete_callable_swap_instrument_request>(msg)) {
            delete_callable_swap_instrument_response resp;
            resp.success = true;
            for (const auto& id : req->ids) {
                try {
                    svc.remove_callable_swap_instrument(id);
                    resp.results.push_back({id, {true, ""}});
                } catch (const std::exception& e) {
                    resp.results.push_back({id, {false, e.what()}});
                    resp.success = false;
                    resp.message = "One or more deletions failed";
                }
            }
            reply(nats_, msg, resp);
        }
    }

    void history_callable_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::callable_swap_instrument_service svc(*ctx_expected);
        if (auto req = decode<get_callable_swap_instrument_history_request>(msg)) {
            try {
                reply(nats_, msg, get_callable_swap_instrument_history_response{
                    .success = true,
                    .history = svc.get_callable_swap_instrument_history(req->id)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_callable_swap_instrument_history_response{
                    .success = false, .message = e.what()});
            }
        }
    }

    // ---- Knock-Out Swap ----

    void list_knock_out_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::knock_out_swap_instrument_service svc(*ctx_expected);
        get_knock_out_swap_instruments_response resp;
        try {
            if (auto req = decode<get_knock_out_swap_instruments_request>(msg)) {
                resp.instruments = svc.list_knock_out_swap_instruments(
                    static_cast<std::uint32_t>(req->offset),
                    static_cast<std::uint32_t>(req->limit));
                resp.total_available_count = static_cast<int>(svc.count_knock_out_swap_instruments());
            }
        } catch (const std::exception& e) {
            resp.success = false; resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void save_knock_out_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::knock_out_swap_instrument_service svc(ctx);
        if (auto req = decode<save_knock_out_swap_instrument_request>(msg)) {
            try {
                svc.save_knock_out_swap_instrument(req->data);
                reply(nats_, msg, save_knock_out_swap_instrument_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_knock_out_swap_instrument_response{.success = false, .message = e.what()});
            }
        }
    }

    void remove_knock_out_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::knock_out_swap_instrument_service svc(ctx);
        if (auto req = decode<delete_knock_out_swap_instrument_request>(msg)) {
            delete_knock_out_swap_instrument_response resp;
            resp.success = true;
            for (const auto& id : req->ids) {
                try {
                    svc.remove_knock_out_swap_instrument(id);
                    resp.results.push_back({id, {true, ""}});
                } catch (const std::exception& e) {
                    resp.results.push_back({id, {false, e.what()}});
                    resp.success = false;
                    resp.message = "One or more deletions failed";
                }
            }
            reply(nats_, msg, resp);
        }
    }

    void history_knock_out_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::knock_out_swap_instrument_service svc(*ctx_expected);
        if (auto req = decode<get_knock_out_swap_instrument_history_request>(msg)) {
            try {
                reply(nats_, msg, get_knock_out_swap_instrument_history_response{
                    .success = true,
                    .history = svc.get_knock_out_swap_instrument_history(req->id)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_knock_out_swap_instrument_history_response{
                    .success = false, .message = e.what()});
            }
        }
    }

    // ---- Inflation Swap ----

    void list_inflation_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::inflation_swap_instrument_service svc(*ctx_expected);
        get_inflation_swap_instruments_response resp;
        try {
            if (auto req = decode<get_inflation_swap_instruments_request>(msg)) {
                resp.instruments = svc.list_inflation_swap_instruments(
                    static_cast<std::uint32_t>(req->offset),
                    static_cast<std::uint32_t>(req->limit));
                resp.total_available_count = static_cast<int>(svc.count_inflation_swap_instruments());
            }
        } catch (const std::exception& e) {
            resp.success = false; resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void save_inflation_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::inflation_swap_instrument_service svc(ctx);
        if (auto req = decode<save_inflation_swap_instrument_request>(msg)) {
            try {
                svc.save_inflation_swap_instrument(req->data);
                reply(nats_, msg, save_inflation_swap_instrument_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_inflation_swap_instrument_response{.success = false, .message = e.what()});
            }
        }
    }

    void remove_inflation_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::inflation_swap_instrument_service svc(ctx);
        if (auto req = decode<delete_inflation_swap_instrument_request>(msg)) {
            delete_inflation_swap_instrument_response resp;
            resp.success = true;
            for (const auto& id : req->ids) {
                try {
                    svc.remove_inflation_swap_instrument(id);
                    resp.results.push_back({id, {true, ""}});
                } catch (const std::exception& e) {
                    resp.results.push_back({id, {false, e.what()}});
                    resp.success = false;
                    resp.message = "One or more deletions failed";
                }
            }
            reply(nats_, msg, resp);
        }
    }

    void history_inflation_swap(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::inflation_swap_instrument_service svc(*ctx_expected);
        if (auto req = decode<get_inflation_swap_instrument_history_request>(msg)) {
            try {
                reply(nats_, msg, get_inflation_swap_instrument_history_response{
                    .success = true,
                    .history = svc.get_inflation_swap_instrument_history(req->id)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_inflation_swap_instrument_history_response{
                    .success = false, .message = e.what()});
            }
        }
    }

    // ---- RPA ----

    void list_rpa(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::rpa_instrument_service svc(*ctx_expected);
        get_rpa_instruments_response resp;
        try {
            if (auto req = decode<get_rpa_instruments_request>(msg)) {
                resp.instruments = svc.list_rpa_instruments(
                    static_cast<std::uint32_t>(req->offset),
                    static_cast<std::uint32_t>(req->limit));
                resp.total_available_count = static_cast<int>(svc.count_rpa_instruments());
            }
        } catch (const std::exception& e) {
            resp.success = false; resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void save_rpa(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::rpa_instrument_service svc(ctx);
        if (auto req = decode<save_rpa_instrument_request>(msg)) {
            try {
                svc.save_rpa_instrument(req->data);
                reply(nats_, msg, save_rpa_instrument_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_rpa_instrument_response{.success = false, .message = e.what()});
            }
        }
    }

    void remove_rpa(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "trading::instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden); return;
        }
        service::rpa_instrument_service svc(ctx);
        if (auto req = decode<delete_rpa_instrument_request>(msg)) {
            delete_rpa_instrument_response resp;
            resp.success = true;
            for (const auto& id : req->ids) {
                try {
                    svc.remove_rpa_instrument(id);
                    resp.results.push_back({id, {true, ""}});
                } catch (const std::exception& e) {
                    resp.results.push_back({id, {false, e.what()}});
                    resp.success = false;
                    resp.message = "One or more deletions failed";
                }
            }
            reply(nats_, msg, resp);
        }
    }

    void history_rpa(ores::nats::message msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::rpa_instrument_service svc(*ctx_expected);
        if (auto req = decode<get_rpa_instrument_history_request>(msg)) {
            try {
                reply(nats_, msg, get_rpa_instrument_history_response{
                    .success = true,
                    .history = svc.get_rpa_instrument_history(req->id)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_rpa_instrument_history_response{
                    .success = false, .message = e.what()});
            }
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::trading::messaging

#endif
