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

#include <chrono>
#include <optional>
#include <span>
#include <unordered_map>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
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
#include "ores.trading.core/service/fra_instrument_service.hpp"
#include "ores.trading.core/service/vanilla_swap_instrument_service.hpp"
#include "ores.trading.core/service/cap_floor_instrument_service.hpp"
#include "ores.trading.core/service/swaption_instrument_service.hpp"
#include "ores.trading.core/service/balance_guaranteed_swap_instrument_service.hpp"
#include "ores.trading.core/service/callable_swap_instrument_service.hpp"
#include "ores.trading.core/service/knock_out_swap_instrument_service.hpp"
#include "ores.trading.core/service/inflation_swap_instrument_service.hpp"
#include "ores.trading.core/service/rpa_instrument_service.hpp"
#include "ores.trading.core/service/fx_forward_instrument_service.hpp"
#include "ores.trading.core/service/fx_vanilla_option_instrument_service.hpp"
#include "ores.trading.core/service/fx_barrier_option_instrument_service.hpp"
#include "ores.trading.core/service/fx_digital_option_instrument_service.hpp"
#include "ores.trading.core/service/fx_asian_forward_instrument_service.hpp"
#include "ores.trading.core/service/fx_accumulator_instrument_service.hpp"
#include "ores.trading.core/service/fx_variance_swap_instrument_service.hpp"
#include "ores.trading.core/service/bond_instrument_service.hpp"
#include "ores.trading.core/service/credit_instrument_service.hpp"
#include "ores.trading.core/service/equity_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_digital_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_barrier_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_asian_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_forward_instrument_service.hpp"
#include "ores.trading.core/service/equity_variance_swap_instrument_service.hpp"
#include "ores.trading.core/service/equity_swap_instrument_service.hpp"
#include "ores.trading.core/service/equity_accumulator_instrument_service.hpp"
#include "ores.trading.core/service/equity_position_instrument_service.hpp"
#include "ores.trading.core/service/commodity_instrument_service.hpp"
#include "ores.trading.core/service/composite_instrument_service.hpp"
#include "ores.trading.core/service/scripted_instrument_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.dq.api/messaging/fsm_protocol.hpp"
#include "ores.trading.core/service/trade_status_service.hpp"
#include "ores.trading.core/export.hpp"

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

class ORES_TRADING_CORE_EXPORT trade_handler {
public:
    trade_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier,
        std::string http_base_url = {})
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)),
          http_base_url_(std::move(http_base_url)) {}

private:
    template<typename Ctx>
    static void populate_instruments_for_trades(
        const Ctx& ctx,
        std::vector<trade_export_item>& items) {
        using ores::trading::domain::product_type;
        using ores::trading::domain::trade_instrument;
        using ores::trading::domain::swap_instrument_data;
        using ores::trading::domain::composite_instrument_data;

        // Phase 1: bucket instrument IDs by (product_type, trade_type)
        std::vector<std::string>
            bond_ids, credit_ids, commodity_ids, scripted_ids, composite_ids,
            fra_ids, vswap_ids, capfloor_ids, swaption_ids,
            bgs_ids, callable_ids, koswap_ids, infl_ids, rpa_ids,
            fxfwd_ids, fxopt_ids, fxbar_ids, fxdig_ids, fxasn_ids, fxacc_ids, fxvar_ids,
            eq_opt_ids, eq_fwd_ids, eq_swp_ids, eq_var_ids, eq_bar_ids,
            eq_asn_ids, eq_dig_ids, eq_acc_ids, eq_pos_ids;

        for (const auto& item : items) {
            const auto& t = item.trade;
            if (!t.classification.instrument_id || t.classification.product_type == product_type::unknown) continue;
            const auto id = boost::uuids::to_string(*t.classification.instrument_id);
            const auto& ttc = t.classification.trade_type;
            switch (t.classification.product_type) {
            case product_type::bond:      bond_ids.push_back(id);      break;
            case product_type::credit:    credit_ids.push_back(id);    break;
            case product_type::commodity: commodity_ids.push_back(id); break;
            case product_type::scripted:  scripted_ids.push_back(id);  break;
            case product_type::composite: composite_ids.push_back(id); break;
            case product_type::swap:
                if      (ttc == "ForwardRateAgreement")             fra_ids.push_back(id);
                else if (ttc == "Swap" || ttc == "CrossCurrencySwap"
                         || ttc == "FlexiSwap")                     vswap_ids.push_back(id);
                else if (ttc == "CapFloor")                          capfloor_ids.push_back(id);
                else if (ttc == "Swaption")                          swaption_ids.push_back(id);
                else if (ttc == "BalanceGuaranteedSwap")             bgs_ids.push_back(id);
                else if (ttc == "CallableSwap")                      callable_ids.push_back(id);
                else if (ttc == "KnockOutSwap")                      koswap_ids.push_back(id);
                else if (ttc == "InflationSwap")                     infl_ids.push_back(id);
                else if (ttc == "RiskParticipationAgreement")        rpa_ids.push_back(id);
                break;
            case product_type::fx:
                if      (ttc == "FxForward" || ttc == "FxSwap")     fxfwd_ids.push_back(id);
                else if (ttc == "FxOption")                          fxopt_ids.push_back(id);
                else if (ttc == "FxBarrierOption"
                         || ttc == "FxGenericBarrierOption"
                         || ttc == "FxDoubleBarrierOption"
                         || ttc == "FxEuropeanBarrierOption"
                         || ttc == "FxKIKOBarrierOption")            fxbar_ids.push_back(id);
                else if (ttc == "FxDigitalOption"
                         || ttc == "FxDigitalBarrierOption"
                         || ttc == "FxTouchOption"
                         || ttc == "FxDoubleTouchOption")            fxdig_ids.push_back(id);
                else if (ttc == "FxAverageForward"
                         || ttc == "FxTaRF")                         fxasn_ids.push_back(id);
                else if (ttc == "FxAccumulator")                     fxacc_ids.push_back(id);
                else if (ttc == "FxVarianceSwap")                    fxvar_ids.push_back(id);
                break;
            case product_type::equity:
                if      (ttc == "EquityOption"
                         || ttc == "EquityCliquetOption"
                         || ttc == "EquityOutperformanceOption")      eq_opt_ids.push_back(id);
                else if (ttc == "EquityForward")                      eq_fwd_ids.push_back(id);
                else if (ttc == "EquitySwap"
                         || ttc == "EquityWorstOfBasketSwap")         eq_swp_ids.push_back(id);
                else if (ttc == "EquityVarianceSwap")                 eq_var_ids.push_back(id);
                else if (ttc == "EquityBarrierOption"
                         || ttc == "EquityDoubleBarrierOption"
                         || ttc == "EquityEuropeanBarrierOption")     eq_bar_ids.push_back(id);
                else if (ttc == "EquityAsianOption")                  eq_asn_ids.push_back(id);
                else if (ttc == "EquityDigitalOption"
                         || ttc == "EquityTouchOption")               eq_dig_ids.push_back(id);
                else if (ttc == "EquityAccumulator"
                         || ttc == "EquityTaRF")                      eq_acc_ids.push_back(id);
                else if (ttc == "EquityPosition")                     eq_pos_ids.push_back(id);
                break;
            case product_type::unknown: break;
            }
        }

        // Phase 2: batch-fetch legs (one call covers all swap types)
        std::unordered_map<std::string, std::vector<ores::trading::domain::swap_leg>> legs_map;
        {
            std::vector<std::string> all_swap;
            for (auto* v : {&fra_ids, &vswap_ids, &capfloor_ids, &swaption_ids,
                            &bgs_ids, &callable_ids, &koswap_ids, &infl_ids, &rpa_ids})
                all_swap.insert(all_swap.end(), v->begin(), v->end());
            if (!all_swap.empty()) {
                service::fra_instrument_service fra_svc(ctx);
                for (auto& leg : fra_svc.get_swap_legs_batch(all_swap))
                    legs_map[boost::uuids::to_string(leg.instrument_id)].push_back(
                        std::move(leg));
            }
        }
        std::unordered_map<std::string, std::vector<ores::trading::domain::composite_leg>>
            comp_legs_map;
        if (!composite_ids.empty()) {
            service::composite_instrument_service comp_svc(ctx);
            for (auto& leg : comp_svc.get_legs_batch(composite_ids))
                comp_legs_map[boost::uuids::to_string(leg.instrument_id)].push_back(
                    std::move(leg));
        }

        // Phase 3: batch-fetch instruments, build lookup map
        std::unordered_map<std::string, trade_instrument> imap;

        auto take_legs = [&](const std::string& id) {
            auto it = legs_map.find(id);
            return it != legs_map.end()
                ? std::move(it->second)
                : std::vector<ores::trading::domain::swap_leg>{};
        };

        // Flat / single-table types
        auto add_flat = [&](auto&& results) {
            for (auto& v : results)
                imap[boost::uuids::to_string(v.instrument_id)] = std::move(v);
        };

        if (!bond_ids.empty()) {
            service::bond_instrument_service svc(ctx);
            add_flat(svc.get_bond_instruments(bond_ids));
        }
        if (!credit_ids.empty()) {
            service::credit_instrument_service svc(ctx);
            add_flat(svc.get_credit_instruments(credit_ids));
        }
        if (!commodity_ids.empty()) {
            service::commodity_instrument_service svc(ctx);
            add_flat(svc.get_commodity_instruments(commodity_ids));
        }
        if (!scripted_ids.empty()) {
            service::scripted_instrument_service svc(ctx);
            add_flat(svc.get_scripted_instruments(scripted_ids));
        }
        if (!composite_ids.empty()) {
            service::composite_instrument_service svc(ctx);
            for (auto& v : svc.get_composite_instruments(composite_ids)) {
                const auto id = boost::uuids::to_string(v.instrument_id);
                composite_instrument_data data;
                data.instrument = std::move(v);
                auto it = comp_legs_map.find(id);
                if (it != comp_legs_map.end()) data.legs = std::move(it->second);
                imap[id] = std::move(data);
            }
        }

        // Rates / swap types (9 sub-types, all share swap_legs table)
        auto add_swap = [&](auto&& results) {
            for (auto& v : results) {
                const auto id = boost::uuids::to_string(v.instrument_id);
                swap_instrument_data data;
                data.instrument = std::move(v);
                data.legs = take_legs(id);
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
                imap[boost::uuids::to_string(v.instrument_id)] =
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

        // Equity types
        auto add_eq = [&](auto&& results) {
            for (auto& v : results)
                imap[boost::uuids::to_string(v.instrument_id)] =
                    ores::trading::domain::equity_instrument_variant{std::move(v)};
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
            if (!t.classification.instrument_id || t.classification.product_type == product_type::unknown) continue;
            const auto id = boost::uuids::to_string(*t.classification.instrument_id);
            if (auto it = imap.find(id); it != imap.end())
                item.instrument = it->second;
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
                std::optional<boost::uuids::uuid> node;
                if (!req->node_id.empty()) {
                    boost::uuids::string_generator gen;
                    node = gen(req->node_id);
                }
                auto trades = svc.list_trades_by_node(offset, limit, node);
                resp.trades = std::move(trades);
                resp.total_available_count =
                    static_cast<int>(svc.count_trades_by_node(node));
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(trade_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
            resp.trades.clear();
            resp.total_available_count = 0;
        }
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
                const auto transitions = fetch_fsm_transitions(extract_bearer(msg));
                svc.save_trades(req->trades, transitions);
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

    void instrument(ores::nats::message msg) {
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
        get_trade_instrument_response resp;
        try {
            if (auto req = decode<get_trade_instrument_request>(msg)) {
                auto trade_opt = svc.find_trade(req->trade_id);
                if (!trade_opt) {
                    resp.success = false;
                    resp.message = "Trade not found: " + req->trade_id;
                } else {
                    std::vector<trade_export_item> items{{.trade = std::move(*trade_opt)}};
                    populate_instruments_for_trades(ctx, items);
                    resp.trade = std::move(items[0].trade);
                    resp.instrument = std::move(items[0].instrument);
                    resp.success = true;
                }
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

                std::optional<boost::uuids::uuid> node;
                if (!req->node_id.empty()) {
                    boost::uuids::string_generator gen;
                    node = gen(req->node_id);
                }
                auto trades = svc.list_trades_by_node(offset, limit, node);
                resp.items.reserve(trades.size());
                for (auto& t : trades)
                    resp.items.push_back({.trade = std::move(t)});
                populate_instruments_for_trades(ctx, resp.items);
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

            // Fetch trades for all requested books in pages and batch-populate instruments.
            service::trade_service svc(ctx);
            std::vector<trade_export_item> all_items;
            constexpr std::uint32_t page_size = 1000;
            for (const auto& bid : req->book_ids) {
                try {
                    boost::uuids::string_generator gen;
                    const auto book_node = std::optional<boost::uuids::uuid>(gen(bid));
                    std::uint32_t offset = 0;
                    while (true) {
                        auto trades = svc.list_trades_by_node(offset, page_size, book_node);
                        const auto n = static_cast<std::uint32_t>(trades.size());
                        if (n == 0) break;
                        std::vector<trade_export_item> page;
                        page.reserve(n);
                        for (auto& t : trades)
                            page.push_back({.trade = std::move(t)});
                        populate_instruments_for_trades(ctx, page);
                        all_items.insert(all_items.end(),
                            std::make_move_iterator(page.begin()),
                            std::make_move_iterator(page.end()));
                        offset += n;
                        if (n < page_size) break;
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
    /**
     * @brief Fetches all FSM transitions from the DQ service via NATS.
     *
     * Results are cached process-wide for 5 minutes; FSM transitions are
     * system-level reference data that change only on schema migrations.
     * The cache avoids an extra NATS round-trip per trade when the ore import
     * handler sends one save_trade message per trade.
     * Throws std::runtime_error on failure; callers must handle or propagate.
     */
    // Extract the raw JWT from an incoming message, preferring the delegated
    // header so the original end-user context propagates to downstream calls.
    static std::string extract_bearer(const ores::nats::message& msg) {
        using namespace ores::nats::headers;
        for (auto hdr : {delegated_authorization, authorization}) {
            const auto it = msg.headers.find(std::string(hdr));
            if (it != msg.headers.end() &&
                    it->second.starts_with(bearer_prefix))
                return std::string(it->second.substr(bearer_prefix.size()));
        }
        return {};
    }

    service::fsm_transition_map fetch_fsm_transitions(std::string_view bearer) {
        using namespace ores::dq::messaging;

        {
            std::lock_guard lock(s_cache_mutex_);
            const auto age = std::chrono::steady_clock::now() - s_cache_fetched_;
            if (!s_cache_.empty() && age < std::chrono::minutes(5))
                return s_cache_;
        }

        service::fsm_transition_map result;
        const get_fsm_transitions_request req{};
        const auto json = rfl::json::write(req);
        const auto* p = reinterpret_cast<const std::byte*>(json.data());
        try {
            using namespace ores::nats::headers;
            std::unordered_map<std::string, std::string> hdrs;
            if (!bearer.empty())
                hdrs[std::string(delegated_authorization)] =
                    std::string(bearer_prefix) + std::string(bearer);
            const auto reply = nats_.request_sync(
                get_fsm_transitions_request::nats_subject,
                std::span<const std::byte>(p, json.size()),
                std::move(hdrs), std::chrono::seconds(2));
            const std::string_view sv(
                reinterpret_cast<const char*>(reply.data.data()),
                reply.data.size());
            const auto resp = rfl::json::read<get_fsm_transitions_response>(sv);
            if (resp && resp->success) {
                for (auto& t : resp->transitions)
                    result[t.id] = std::move(t);
            } else {
                const auto detail = resp
                    ? resp->message
                    : std::string("no response from DQ service");
                throw std::runtime_error(
                    "Failed to retrieve FSM transitions: " + detail);
            }
        } catch (const std::runtime_error&) {
            throw;
        } catch (const std::exception& e) {
            throw std::runtime_error(
                std::string("Failed to retrieve FSM transitions: ") + e.what());
        }

        {
            std::lock_guard lock(s_cache_mutex_);
            s_cache_ = result;
            s_cache_fetched_ = std::chrono::steady_clock::now();
        }
        return result;
    }

    inline static std::mutex s_cache_mutex_;
    inline static service::fsm_transition_map s_cache_;
    inline static std::chrono::steady_clock::time_point s_cache_fetched_{};

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    std::string http_base_url_;
};

} // namespace ores::trading::messaging

#endif
