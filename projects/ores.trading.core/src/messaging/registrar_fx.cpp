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
#include "ores.trading.core/messaging/registrar_detail.hpp"
#include "ores.trading.core/messaging/fx_instrument_handler.hpp"
#include "ores.trading.core/messaging/typed_fx_instrument_handler.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_fx_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = queue_name;

    // FX instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_fx_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            fx_instrument_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_fx_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            fx_instrument_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_fx_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            fx_instrument_handler h(nats, ctx, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_fx_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            fx_instrument_handler h(nats, ctx, verifier);
            h.history(std::move(msg));
        }));

    // Typed FX instruments
    subs.push_back(nats.queue_subscribe(
        std::string(save_fx_forward_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_fx_instrument_handler h(nats, ctx, verifier);
            h.save_forward(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_fx_vanilla_option_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_fx_instrument_handler h(nats, ctx, verifier);
            h.save_vanilla_option(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_fx_barrier_option_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_fx_instrument_handler h(nats, ctx, verifier);
            h.save_barrier_option(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_fx_digital_option_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_fx_instrument_handler h(nats, ctx, verifier);
            h.save_digital_option(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_fx_asian_forward_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_fx_instrument_handler h(nats, ctx, verifier);
            h.save_asian_forward(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_fx_accumulator_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_fx_instrument_handler h(nats, ctx, verifier);
            h.save_accumulator(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_fx_variance_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_fx_instrument_handler h(nats, ctx, verifier);
            h.save_variance_swap(std::move(msg));
        }));

    return subs;
}

}
