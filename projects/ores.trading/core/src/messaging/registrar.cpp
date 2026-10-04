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
#include "ores.history.core/messaging/registrar.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.trading.core/messaging/activity_category_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/activity_category_registrar.hpp"
#include "ores.trading.core/messaging/activity_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/activity_type_registrar.hpp"
#include "ores.trading.core/messaging/amortization_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/amortization_type_registrar.hpp"
#include "ores.trading.core/messaging/ascot_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/average_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/average_type_registrar.hpp"
#include "ores.trading.core/messaging/barrier_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/barrier_type_registrar.hpp"
#include "ores.trading.core/messaging/bond_forward_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_future_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_call_date_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_conversion_target_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_leg_amortization_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_leg_amount_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_leg_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_leg_rate_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_leg_schedule_date_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_leg_schedule_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_leg_amortization_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_leg_amount_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_leg_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_leg_rate_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_option_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_repo_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/bond_trs_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/booking_nature_type_registrar.hpp"
#include "ores.trading.core/messaging/callable_swap_call_date_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/commodity_basket_constituent_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/counterparty_scope_type_registrar.hpp"
#include "ores.trading.core/messaging/entry_channel_type_registrar.hpp"
#include "ores.trading.core/messaging/equity_position_option_underlying_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/exercise_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/exercise_type_registrar.hpp"
#include "ores.trading.core/messaging/fpml_event_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/fpml_event_type_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_exercise_fee_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_payment_date_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_premium_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/instrument_schedule_date_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/instrument_schedule_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/instrument_strike_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/lifecycle_event_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/lifecycle_event_registrar.hpp"
#include "ores.trading.core/messaging/long_short_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/long_short_type_registrar.hpp"
#include "ores.trading.core/messaging/moment_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/moment_type_registrar.hpp"
#include "ores.trading.core/messaging/option_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/option_type_registrar.hpp"
#include "ores.trading.core/messaging/party_role_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/party_role_type_registrar.hpp"
#include "ores.trading.core/messaging/payoff_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/payoff_type_registrar.hpp"
#include "ores.trading.core/messaging/price_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/price_type_registrar.hpp"
#include "ores.trading.core/messaging/registrar.hpp"
#include "ores.trading.core/messaging/registrar_detail.hpp"
#include "ores.trading.core/messaging/return_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/return_type_registrar.hpp"
#include "ores.trading.core/messaging/settlement_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/settlement_type_registrar.hpp"
#include "ores.trading.core/messaging/trade_additional_field_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_additional_field_registrar.hpp"
#include "ores.trading.core/messaging/trade_portfolio_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_portfolio_registrar.hpp"
#include "ores.trading.core/messaging/trade_anchor_registrar.hpp"
#include "ores.trading.core/messaging/trade_booking_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_booking_registrar.hpp"
#include "ores.trading.core/messaging/trade_envelope_additional_field_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_envelope_additional_field_registrar.hpp"
#include "ores.trading.core/messaging/trade_envelope_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_envelope_portfolio_id_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_envelope_portfolio_id_registrar.hpp"
#include "ores.trading.core/messaging/trade_envelope_registrar.hpp"
#include "ores.trading.core/messaging/trade_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_id_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_id_type_registrar.hpp"
#include "ores.trading.core/messaging/trade_identifier_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_identifier_registrar.hpp"
#include "ores.trading.core/messaging/trade_operations_handler.hpp"
#include "ores.trading.core/messaging/trade_party_role_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_party_role_registrar.hpp"
#include "ores.trading.core/messaging/trade_state_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_state_registrar.hpp"
#include "ores.trading.core/messaging/trade_type_history_provider_registrar.hpp"
#include <memory>

namespace ores::trading::messaging {

namespace {

constexpr std::string_view queue_group = "ores.trading.service";

// The dispatch registry is process-static: the history handler's
// subscription outlives this function, so the registry it references
// must outlive the subscription too.
ores::history::service::dispatch_registry& history_registry() {
    static ores::history::service::dispatch_registry instance;
    return instance;
}

}

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             std::string http_base_url) {

    auto subs = detail::register_trade_handlers(nats, ctx, verifier, http_base_url);
    // Capacity for the ~250 subscriptions the masters below return.
    subs.reserve(256);

    const auto append = [&subs](auto vec) {
        subs.insert(
            subs.end(), std::make_move_iterator(vec.begin()), std::make_move_iterator(vec.end()));
    };

    append(detail::register_rates_handlers(nats, ctx, verifier));
    append(detail::register_fx_handlers(nats, ctx, verifier));
    append(detail::register_bond_handlers(nats, ctx, verifier));
    append(detail::register_credit_handlers(nats, ctx, verifier));
    append(detail::register_equity_handlers(nats, ctx, verifier));
    append(detail::register_commodity_handlers(nats, ctx, verifier));
    append(detail::register_composite_handlers(nats, ctx, verifier));
    append(detail::register_scripted_handlers(nats, ctx, verifier));
    append(register_activity_category_handlers(nats, ctx, verifier));
    append(register_activity_type_handlers(nats, ctx, verifier));
    append(register_amortization_type_handlers(nats, ctx, verifier));
    append(register_average_type_handlers(nats, ctx, verifier));
    append(register_barrier_type_handlers(nats, ctx, verifier));
    append(register_booking_nature_type_handlers(nats, ctx, verifier));
    append(register_counterparty_scope_type_handlers(nats, ctx, verifier));
    append(register_entry_channel_type_handlers(nats, ctx, verifier));
    append(register_exercise_type_handlers(nats, ctx, verifier));
    append(register_fpml_event_type_handlers(nats, ctx, verifier));
    append(register_lifecycle_event_handlers(nats, ctx, verifier));
    append(register_long_short_type_handlers(nats, ctx, verifier));
    append(register_moment_type_handlers(nats, ctx, verifier));
    append(register_option_type_handlers(nats, ctx, verifier));
    append(register_party_role_type_handlers(nats, ctx, verifier));
    append(register_payoff_type_handlers(nats, ctx, verifier));
    append(register_price_type_handlers(nats, ctx, verifier));
    append(register_return_type_handlers(nats, ctx, verifier));
    append(register_settlement_type_handlers(nats, ctx, verifier));
    append(register_trade_additional_field_handlers(nats, ctx, verifier));
    append(register_trade_portfolio_handlers(nats, ctx, verifier));
    append(register_trade_anchor_handlers(nats, ctx, verifier));
    {
        // Trade operations (hand-written handler, not codegen).
        auto toh = std::make_shared<trade_operations_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            book_trade_request::nats_subject, queue_group, [toh](ores::nats::message msg) {
                toh->book_trade(std::move(msg));
            }));
    }
    append(register_trade_booking_handlers(nats, ctx, verifier));
    append(register_trade_envelope_additional_field_handlers(nats, ctx, verifier));
    append(register_trade_envelope_handlers(nats, ctx, verifier));
    append(register_trade_envelope_portfolio_id_handlers(nats, ctx, verifier));
    append(register_trade_id_type_handlers(nats, ctx, verifier));
    append(register_trade_identifier_handlers(nats, ctx, verifier));
    append(register_trade_party_role_handlers(nats, ctx, verifier));
    append(register_trade_state_handlers(nats, ctx, verifier));

    auto& hist_registry = history_registry();
    register_activity_category_history_provider(hist_registry);
    register_activity_type_history_provider(hist_registry);
    register_amortization_type_history_provider(hist_registry);
    register_ascot_history_provider(hist_registry);
    register_average_type_history_provider(hist_registry);
    register_barrier_type_history_provider(hist_registry);
    register_bond_forward_history_provider(hist_registry);
    register_bond_future_history_provider(hist_registry);
    register_bond_issue_call_date_history_provider(hist_registry);
    register_bond_issue_conversion_target_history_provider(hist_registry);
    register_bond_issue_history_provider(hist_registry);
    register_bond_issue_leg_amortization_history_provider(hist_registry);
    register_bond_issue_leg_amount_history_provider(hist_registry);
    register_bond_issue_leg_history_provider(hist_registry);
    register_bond_issue_leg_rate_history_provider(hist_registry);
    register_bond_issue_leg_schedule_date_history_provider(hist_registry);
    register_bond_issue_leg_schedule_history_provider(hist_registry);
    register_bond_leg_amortization_history_provider(hist_registry);
    register_bond_leg_amount_history_provider(hist_registry);
    register_bond_leg_history_provider(hist_registry);
    register_bond_leg_rate_history_provider(hist_registry);
    register_bond_option_history_provider(hist_registry);
    register_bond_repo_history_provider(hist_registry);
    register_bond_trs_history_provider(hist_registry);
    register_callable_swap_call_date_history_provider(hist_registry);
    register_commodity_basket_constituent_history_provider(hist_registry);
    register_equity_position_option_underlying_history_provider(hist_registry);
    register_exercise_type_history_provider(hist_registry);
    register_fpml_event_type_history_provider(hist_registry);
    register_instrument_option_exercise_fee_history_provider(hist_registry);
    register_instrument_option_history_provider(hist_registry);
    register_instrument_option_payment_date_history_provider(hist_registry);
    register_instrument_option_premium_history_provider(hist_registry);
    register_instrument_schedule_date_history_provider(hist_registry);
    register_instrument_schedule_history_provider(hist_registry);
    register_instrument_strike_history_provider(hist_registry);
    register_lifecycle_event_history_provider(hist_registry);
    register_long_short_type_history_provider(hist_registry);
    register_moment_type_history_provider(hist_registry);
    register_option_type_history_provider(hist_registry);
    register_party_role_type_history_provider(hist_registry);
    register_payoff_type_history_provider(hist_registry);
    register_price_type_history_provider(hist_registry);
    register_return_type_history_provider(hist_registry);
    register_settlement_type_history_provider(hist_registry);
    register_trade_additional_field_history_provider(hist_registry);
    register_trade_portfolio_history_provider(hist_registry);
    register_trade_booking_history_provider(hist_registry);
    register_trade_envelope_additional_field_history_provider(hist_registry);
    register_trade_envelope_history_provider(hist_registry);
    register_trade_envelope_portfolio_id_history_provider(hist_registry);
    register_trade_history_provider(hist_registry);
    register_trade_id_type_history_provider(hist_registry);
    register_trade_identifier_history_provider(hist_registry);
    register_trade_party_role_history_provider(hist_registry);
    register_trade_state_history_provider(hist_registry);
    register_trade_type_history_provider(hist_registry);
    subs.push_back(ores::history::messaging::register_history_handlers(
        nats, hist_registry, "trading", queue_group, ctx, verifier));

    return subs;
}

}
