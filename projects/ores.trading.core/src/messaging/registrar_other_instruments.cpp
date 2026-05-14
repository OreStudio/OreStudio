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
#include "ores.trading.core/messaging/bond_instrument_handler.hpp"
#include "ores.trading.core/messaging/credit_instrument_handler.hpp"
#include "ores.trading.core/messaging/typed_equity_instrument_handler.hpp"
#include "ores.trading.core/messaging/commodity_instrument_handler.hpp"
#include "ores.trading.core/messaging/composite_instrument_handler.hpp"
#include "ores.trading.core/messaging/scripted_instrument_handler.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_other_instrument_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = queue_name;

    // Bond instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_bond_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            bond_instrument_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_bond_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            bond_instrument_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_bond_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            bond_instrument_handler h(nats, ctx, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_bond_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            bond_instrument_handler h(nats, ctx, verifier);
            h.history(std::move(msg));
        }));

    // Credit instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_credit_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            credit_instrument_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_credit_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            credit_instrument_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_credit_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            credit_instrument_handler h(nats, ctx, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_credit_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            credit_instrument_handler h(nats, ctx, verifier);
            h.history(std::move(msg));
        }));

    // Typed equity instruments
    subs.push_back(nats.queue_subscribe(
        std::string(save_equity_option_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_equity_instrument_handler h(nats, ctx, verifier);
            h.save_option(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_equity_digital_option_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_equity_instrument_handler h(nats, ctx, verifier);
            h.save_digital_option(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_equity_barrier_option_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_equity_instrument_handler h(nats, ctx, verifier);
            h.save_barrier_option(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_equity_asian_option_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_equity_instrument_handler h(nats, ctx, verifier);
            h.save_asian_option(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_equity_forward_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_equity_instrument_handler h(nats, ctx, verifier);
            h.save_forward(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_equity_variance_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_equity_instrument_handler h(nats, ctx, verifier);
            h.save_variance_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_equity_swap_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_equity_instrument_handler h(nats, ctx, verifier);
            h.save_swap(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_equity_accumulator_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_equity_instrument_handler h(nats, ctx, verifier);
            h.save_accumulator(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_equity_position_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_equity_instrument_handler h(nats, ctx, verifier);
            h.save_position(std::move(msg));
        }));

    // Commodity instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_commodity_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            commodity_instrument_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_commodity_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            commodity_instrument_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_commodity_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            commodity_instrument_handler h(nats, ctx, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_commodity_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            commodity_instrument_handler h(nats, ctx, verifier);
            h.history(std::move(msg));
        }));

    // Composite instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_composite_instrument_legs_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            composite_instrument_handler h(nats, ctx, verifier);
            h.get_legs(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_composite_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            composite_instrument_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_composite_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            composite_instrument_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_composite_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            composite_instrument_handler h(nats, ctx, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_composite_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            composite_instrument_handler h(nats, ctx, verifier);
            h.history(std::move(msg));
        }));

    // Scripted instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_scripted_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            scripted_instrument_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_scripted_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            scripted_instrument_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_scripted_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            scripted_instrument_handler h(nats, ctx, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_scripted_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            scripted_instrument_handler h(nats, ctx, verifier);
            h.history(std::move(msg));
        }));

    return subs;
}

}
