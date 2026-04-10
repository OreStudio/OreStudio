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
#ifndef ORES_TRADING_MESSAGING_TYPED_EQUITY_INSTRUMENT_HANDLER_HPP
#define ORES_TRADING_MESSAGING_TYPED_EQUITY_INSTRUMENT_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.trading.core/service/equity_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_digital_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_barrier_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_asian_option_instrument_service.hpp"
#include "ores.trading.core/service/equity_forward_instrument_service.hpp"
#include "ores.trading.core/service/equity_variance_swap_instrument_service.hpp"
#include "ores.trading.core/service/equity_swap_instrument_service.hpp"
#include "ores.trading.core/service/equity_accumulator_instrument_service.hpp"
#include "ores.trading.core/service/equity_position_instrument_service.hpp"

namespace ores::trading::messaging {

namespace {
inline auto& typed_equity_instrument_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.trading.messaging.typed_equity_instrument_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

template<typename Request, typename Response, typename Service, typename SaveFn>
void handle_typed_equity_save(
    ores::nats::service::client& nats,
    ores::nats::message msg,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier,
    SaveFn save_fn) {
    BOOST_LOG_SEV(typed_equity_instrument_handler_lg(), debug)
        << "Handling " << msg.subject;
    auto ctx_expected = ores::service::service::make_request_context(
        ctx, msg, verifier);
    if (!ctx_expected) {
        error_reply(nats, msg, ctx_expected.error());
        return;
    }
    const auto& rctx = *ctx_expected;
    if (!has_permission(rctx, "trading::instruments:write")) {
        error_reply(nats, msg, ores::service::error_code::forbidden);
        return;
    }
    if (auto req = decode<Request>(msg)) {
        try {
            Service svc(rctx);
            (svc.*save_fn)(req->data);
            BOOST_LOG_SEV(typed_equity_instrument_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats, msg, Response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(typed_equity_instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats, msg, Response{.success = false, .message = e.what()});
        }
    } else {
        BOOST_LOG_SEV(typed_equity_instrument_handler_lg(), warn)
            << "Failed to decode: " << msg.subject;
    }
}

class typed_equity_instrument_handler {
public:
    typed_equity_instrument_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void save_option(ores::nats::message msg) {
        using Svc = service::equity_option_instrument_service;
        handle_typed_equity_save<
            save_equity_option_instrument_request,
            save_equity_option_instrument_response,
            Svc>(nats_, std::move(msg), ctx_, verifier_,
                &Svc::save_equity_option_instrument);
    }

    void save_digital_option(ores::nats::message msg) {
        using Svc = service::equity_digital_option_instrument_service;
        handle_typed_equity_save<
            save_equity_digital_option_instrument_request,
            save_equity_digital_option_instrument_response,
            Svc>(nats_, std::move(msg), ctx_, verifier_,
                &Svc::save_equity_digital_option_instrument);
    }

    void save_barrier_option(ores::nats::message msg) {
        using Svc = service::equity_barrier_option_instrument_service;
        handle_typed_equity_save<
            save_equity_barrier_option_instrument_request,
            save_equity_barrier_option_instrument_response,
            Svc>(nats_, std::move(msg), ctx_, verifier_,
                &Svc::save_equity_barrier_option_instrument);
    }

    void save_asian_option(ores::nats::message msg) {
        using Svc = service::equity_asian_option_instrument_service;
        handle_typed_equity_save<
            save_equity_asian_option_instrument_request,
            save_equity_asian_option_instrument_response,
            Svc>(nats_, std::move(msg), ctx_, verifier_,
                &Svc::save_equity_asian_option_instrument);
    }

    void save_forward(ores::nats::message msg) {
        using Svc = service::equity_forward_instrument_service;
        handle_typed_equity_save<
            save_equity_forward_instrument_request,
            save_equity_forward_instrument_response,
            Svc>(nats_, std::move(msg), ctx_, verifier_,
                &Svc::save_equity_forward_instrument);
    }

    void save_variance_swap(ores::nats::message msg) {
        using Svc = service::equity_variance_swap_instrument_service;
        handle_typed_equity_save<
            save_equity_variance_swap_instrument_request,
            save_equity_variance_swap_instrument_response,
            Svc>(nats_, std::move(msg), ctx_, verifier_,
                &Svc::save_equity_variance_swap_instrument);
    }

    void save_swap(ores::nats::message msg) {
        using Svc = service::equity_swap_instrument_service;
        handle_typed_equity_save<
            save_equity_swap_instrument_request,
            save_equity_swap_instrument_response,
            Svc>(nats_, std::move(msg), ctx_, verifier_,
                &Svc::save_equity_swap_instrument);
    }

    void save_accumulator(ores::nats::message msg) {
        using Svc = service::equity_accumulator_instrument_service;
        handle_typed_equity_save<
            save_equity_accumulator_instrument_request,
            save_equity_accumulator_instrument_response,
            Svc>(nats_, std::move(msg), ctx_, verifier_,
                &Svc::save_equity_accumulator_instrument);
    }

    void save_position(ores::nats::message msg) {
        using Svc = service::equity_position_instrument_service;
        handle_typed_equity_save<
            save_equity_position_instrument_request,
            save_equity_position_instrument_response,
            Svc>(nats_, std::move(msg), ctx_, verifier_,
                &Svc::save_equity_position_instrument);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::trading::messaging

#endif
