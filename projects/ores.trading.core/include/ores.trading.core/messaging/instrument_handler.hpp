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
#ifndef ORES_TRADING_MESSAGING_INSTRUMENT_HANDLER_HPP
#define ORES_TRADING_MESSAGING_INSTRUMENT_HANDLER_HPP

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
#include "ores.trading.core/service/fx_instrument_service.hpp"
#include "ores.trading.core/service/bond_instrument_service.hpp"
#include "ores.trading.core/service/credit_instrument_service.hpp"
#include "ores.trading.core/service/equity_instrument_service.hpp"
#include "ores.trading.core/service/commodity_instrument_service.hpp"
#include "ores.trading.core/service/composite_instrument_service.hpp"
#include "ores.trading.core/service/scripted_instrument_service.hpp"

namespace ores::trading::messaging {

namespace {
inline auto& instrument_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.trading.messaging.instrument_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief Routes get_instrument_for_trade to the correct per-type service.
 *
 * The swap case dispatches by trade_type_code to one of the nine rates
 * instrument tables. All other families route by product_type enum.
 */
class instrument_handler {
public:
    instrument_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void get_instrument_for_trade(ores::nats::message msg) {
        BOOST_LOG_SEV(instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        get_instrument_for_trade_response resp;
        try {
            if (auto req = decode<get_instrument_for_trade_request>(msg)) {
                const auto& family = req->product_type;
                const auto& id = req->instrument_id;

                using ores::trading::domain::product_type;
                if (family == product_type::unknown) {
                    resp.success = false;
                    resp.message =
                        "get_instrument_for_trade: product_type is required";
                    BOOST_LOG_SEV(instrument_handler_lg(), warn)
                        << msg.subject << ": " << resp.message;
                    reply(nats_, msg, resp);
                    return;
                }
                switch (family) {
                case product_type::unknown:
                    break; // unreachable, handled above
                case product_type::swap: {
                    const auto& ttc = req->trade_type_code;
                    service::fra_instrument_service fra_svc(ctx);
                    if (ttc == "ForwardRateAgreement") {
                        if (auto r = fra_svc.find_fra_instrument(id)) {
                            swap_export_result ex;
                            ex.instrument = std::move(*r);
                            ex.legs = fra_svc.get_swap_legs(id);
                            resp.instrument = std::move(ex);
                        }
                    } else if (ttc == "Swap" || ttc == "CrossCurrencySwap"
                               || ttc == "FlexiSwap") {
                        service::vanilla_swap_instrument_service svc(ctx);
                        if (auto r = svc.find_vanilla_swap_instrument(id)) {
                            swap_export_result ex;
                            ex.instrument = std::move(*r);
                            ex.legs = fra_svc.get_swap_legs(id);
                            resp.instrument = std::move(ex);
                        }
                    } else if (ttc == "CapFloor") {
                        service::cap_floor_instrument_service svc(ctx);
                        if (auto r = svc.find_cap_floor_instrument(id)) {
                            swap_export_result ex;
                            ex.instrument = std::move(*r);
                            ex.legs = fra_svc.get_swap_legs(id);
                            resp.instrument = std::move(ex);
                        }
                    } else if (ttc == "Swaption") {
                        service::swaption_instrument_service svc(ctx);
                        if (auto r = svc.find_swaption_instrument(id)) {
                            swap_export_result ex;
                            ex.instrument = std::move(*r);
                            ex.legs = fra_svc.get_swap_legs(id);
                            resp.instrument = std::move(ex);
                        }
                    } else if (ttc == "BalanceGuaranteedSwap") {
                        service::balance_guaranteed_swap_instrument_service svc(ctx);
                        if (auto r = svc.find_balance_guaranteed_swap_instrument(id)) {
                            swap_export_result ex;
                            ex.instrument = std::move(*r);
                            ex.legs = fra_svc.get_swap_legs(id);
                            resp.instrument = std::move(ex);
                        }
                    } else if (ttc == "CallableSwap") {
                        service::callable_swap_instrument_service svc(ctx);
                        if (auto r = svc.find_callable_swap_instrument(id)) {
                            swap_export_result ex;
                            ex.instrument = std::move(*r);
                            ex.legs = fra_svc.get_swap_legs(id);
                            resp.instrument = std::move(ex);
                        }
                    } else if (ttc == "KnockOutSwap") {
                        service::knock_out_swap_instrument_service svc(ctx);
                        if (auto r = svc.find_knock_out_swap_instrument(id)) {
                            swap_export_result ex;
                            ex.instrument = std::move(*r);
                            ex.legs = fra_svc.get_swap_legs(id);
                            resp.instrument = std::move(ex);
                        }
                    } else if (ttc == "InflationSwap") {
                        service::inflation_swap_instrument_service svc(ctx);
                        if (auto r = svc.find_inflation_swap_instrument(id)) {
                            swap_export_result ex;
                            ex.instrument = std::move(*r);
                            ex.legs = fra_svc.get_swap_legs(id);
                            resp.instrument = std::move(ex);
                        }
                    } else if (ttc == "RiskParticipationAgreement") {
                        service::rpa_instrument_service svc(ctx);
                        if (auto r = svc.find_rpa_instrument(id)) {
                            swap_export_result ex;
                            ex.instrument = std::move(*r);
                            ex.legs = fra_svc.get_swap_legs(id);
                            resp.instrument = std::move(ex);
                        }
                    } else {
                        resp.success = false;
                        resp.message =
                            "get_instrument_for_trade: unknown swap "
                            "trade_type_code: " + ttc;
                        BOOST_LOG_SEV(instrument_handler_lg(), warn)
                            << msg.subject << ": " << resp.message;
                        reply(nats_, msg, resp);
                        return;
                    }
                    break;
                }
                case product_type::fx: {
                    service::fx_instrument_service svc(ctx);
                    if (auto r = svc.find_fx_instrument(id))
                        resp.instrument = std::move(*r);
                    break;
                }
                case product_type::bond: {
                    service::bond_instrument_service svc(ctx);
                    if (auto r = svc.find_bond_instrument(id))
                        resp.instrument = std::move(*r);
                    break;
                }
                case product_type::credit: {
                    service::credit_instrument_service svc(ctx);
                    if (auto r = svc.find_credit_instrument(id))
                        resp.instrument = std::move(*r);
                    break;
                }
                case product_type::equity: {
                    service::equity_instrument_service svc(ctx);
                    if (auto r = svc.find_equity_instrument(id))
                        resp.instrument = std::move(*r);
                    break;
                }
                case product_type::commodity: {
                    service::commodity_instrument_service svc(ctx);
                    if (auto r = svc.find_commodity_instrument(id))
                        resp.instrument = std::move(*r);
                    break;
                }
                case product_type::composite: {
                    service::composite_instrument_service svc(ctx);
                    if (auto r = svc.find_composite_instrument(id)) {
                        composite_export_result ex;
                        ex.instrument = std::move(*r);
                        ex.legs = svc.get_legs(id);
                        resp.instrument = std::move(ex);
                    }
                    break;
                }
                case product_type::scripted: {
                    service::scripted_instrument_service svc(ctx);
                    if (auto r = svc.find_scripted_instrument(id))
                        resp.instrument = std::move(*r);
                    break;
                }
                }
                resp.success = true;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(instrument_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::trading::messaging

#endif
