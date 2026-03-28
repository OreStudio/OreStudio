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
#include "ores.trading.core/messaging/registrar.hpp"
#include "ores.trading.core/messaging/trade_handler.hpp"
#include "ores.trading.core/messaging/instrument_ref_handler.hpp"
#include "ores.trading.core/messaging/instrument_handler.hpp"
#include "ores.trading.core/messaging/fx_instrument_handler.hpp"
#include "ores.trading.core/messaging/bond_instrument_handler.hpp"
#include "ores.trading.core/messaging/credit_instrument_handler.hpp"
#include "ores.trading.core/messaging/equity_instrument_handler.hpp"

namespace ores::trading::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.trading.service";

    subs.push_back(nats.queue_subscribe(
        std::string(get_activity_types_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            trade_handler h(nats, ctx, verifier);
            h.list_activity_types(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_trades_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            trade_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_trade_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            trade_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_trade_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            trade_handler h(nats, ctx, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_trade_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            trade_handler h(nats, ctx, verifier);
            h.history(std::move(msg));
        }));

    // Instrument reference data — day count fraction types
    subs.push_back(nats.queue_subscribe(
        std::string(get_day_count_fraction_types_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.list_day_count_fraction_types(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_day_count_fraction_type_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.save_day_count_fraction_type(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_day_count_fraction_type_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.delete_day_count_fraction_type(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_day_count_fraction_type_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.history_day_count_fraction_type(std::move(msg));
        }));

    // Instrument reference data — business day convention types
    subs.push_back(nats.queue_subscribe(
        std::string(get_business_day_convention_types_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.list_business_day_convention_types(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_business_day_convention_type_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.save_business_day_convention_type(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_business_day_convention_type_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.delete_business_day_convention_type(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_business_day_convention_type_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.history_business_day_convention_type(std::move(msg));
        }));

    // Instrument reference data — floating index types
    subs.push_back(nats.queue_subscribe(
        std::string(get_floating_index_types_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.list_floating_index_types(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_floating_index_type_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.save_floating_index_type(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_floating_index_type_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.delete_floating_index_type(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_floating_index_type_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.history_floating_index_type(std::move(msg));
        }));

    // Instrument reference data — payment frequency types
    subs.push_back(nats.queue_subscribe(
        std::string(get_payment_frequency_types_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.list_payment_frequency_types(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_payment_frequency_type_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.save_payment_frequency_type(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_payment_frequency_type_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.delete_payment_frequency_type(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_payment_frequency_type_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.history_payment_frequency_type(std::move(msg));
        }));

    // Instrument reference data — leg types
    subs.push_back(nats.queue_subscribe(
        std::string(get_leg_types_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.list_leg_types(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_leg_type_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.save_leg_type(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_leg_type_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.delete_leg_type(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_leg_type_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_ref_handler h(nats, ctx, verifier);
            h.history_leg_type(std::move(msg));
        }));

    // Instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_handler h(nats, ctx, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_handler h(nats, ctx, verifier);
            h.history(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_swap_legs_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            instrument_handler h(nats, ctx, verifier);
            h.list_legs(std::move(msg));
        }));

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

    // Equity instruments
    subs.push_back(nats.queue_subscribe(
        std::string(get_equity_instruments_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            equity_instrument_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_equity_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            equity_instrument_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_equity_instrument_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            equity_instrument_handler h(nats, ctx, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_equity_instrument_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            equity_instrument_handler h(nats, ctx, verifier);
            h.history(std::move(msg));
        }));

    return subs;
}

}
