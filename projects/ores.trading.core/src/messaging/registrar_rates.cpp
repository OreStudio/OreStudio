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
#include "ores.trading.core/messaging/rates_instrument_handler.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_rates_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.trading.service";

    // FRA instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_fra_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.list_fra(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_fra_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.save_fra(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_fra_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.remove_fra(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_fra_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.history_fra(std::move(msg));
        }));

    // Vanilla swap instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_vanilla_swap_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.list_vanilla_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_vanilla_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.save_vanilla_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_vanilla_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.remove_vanilla_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_vanilla_swap_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.history_vanilla_swap(std::move(msg));
        }));

    // Cap/floor instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_cap_floor_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.list_cap_floor(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_cap_floor_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.save_cap_floor(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_cap_floor_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.remove_cap_floor(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_cap_floor_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.history_cap_floor(std::move(msg));
        }));

    // Swaption instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_swaption_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.list_swaption(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_swaption_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.save_swaption(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_swaption_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.remove_swaption(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_swaption_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.history_swaption(std::move(msg));
        }));

    // Balance guaranteed swap instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_balance_guaranteed_swap_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.list_balance_guaranteed_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_balance_guaranteed_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.save_balance_guaranteed_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_balance_guaranteed_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.remove_balance_guaranteed_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_balance_guaranteed_swap_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.history_balance_guaranteed_swap(std::move(msg));
        }));

    // Callable swap instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_callable_swap_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.list_callable_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_callable_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.save_callable_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_callable_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.remove_callable_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_callable_swap_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.history_callable_swap(std::move(msg));
        }));

    // Knock-out swap instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_knock_out_swap_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.list_knock_out_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_knock_out_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.save_knock_out_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_knock_out_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.remove_knock_out_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_knock_out_swap_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.history_knock_out_swap(std::move(msg));
        }));

    // Inflation swap instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_inflation_swap_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.list_inflation_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_inflation_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.save_inflation_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_inflation_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.remove_inflation_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_inflation_swap_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.history_inflation_swap(std::move(msg));
        }));

    // RPA instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_rpa_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.list_rpa(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_rpa_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.save_rpa(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_rpa_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.remove_rpa(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_rpa_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            rates_instrument_handler h(nats, ctx, verifier);
            h.history_rpa(std::move(msg));
        }));

    return subs;
}

}
