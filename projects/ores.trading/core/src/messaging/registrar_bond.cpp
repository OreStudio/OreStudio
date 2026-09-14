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
#include "ores.trading.core/messaging/ascot_registrar.hpp"
#include "ores.trading.core/messaging/bond_forward_registrar.hpp"
#include "ores.trading.core/messaging/bond_future_delivery_basket_registrar.hpp"
#include "ores.trading.core/messaging/bond_future_registrar.hpp"
#include "ores.trading.core/messaging/bond_instrument_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_call_date_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_conversion_target_registrar.hpp"
#include "ores.trading.core/messaging/bond_issue_registrar.hpp"
#include "ores.trading.core/messaging/bond_leg_amortization_registrar.hpp"
#include "ores.trading.core/messaging/bond_leg_amount_registrar.hpp"
#include "ores.trading.core/messaging/bond_leg_rate_registrar.hpp"
#include "ores.trading.core/messaging/bond_leg_registrar.hpp"
#include "ores.trading.core/messaging/bond_option_registrar.hpp"
#include "ores.trading.core/messaging/bond_repo_registrar.hpp"
#include "ores.trading.core/messaging/bond_trs_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_exercise_fee_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_payment_date_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_premium_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_registrar.hpp"
#include "ores.trading.core/messaging/instrument_schedule_date_registrar.hpp"
#include "ores.trading.core/messaging/instrument_schedule_registrar.hpp"
#include "ores.trading.core/messaging/instrument_strike_registrar.hpp"
#include "ores.trading.core/messaging/registrar_detail.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_bond_handlers(ores::nats::service::client& nats,
                       ores::database::context ctx,
                       std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    auto bond_instrument_subs = register_bond_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_instrument_subs.begin()),
                std::make_move_iterator(bond_instrument_subs.end()));

    auto bond_issue_subs = register_bond_issue_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_issue_subs.begin()),
                std::make_move_iterator(bond_issue_subs.end()));

    auto bond_issue_call_date_subs = register_bond_issue_call_date_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_issue_call_date_subs.begin()),
                std::make_move_iterator(bond_issue_call_date_subs.end()));

    auto bond_issue_conversion_target_subs =
        register_bond_issue_conversion_target_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_issue_conversion_target_subs.begin()),
                std::make_move_iterator(bond_issue_conversion_target_subs.end()));

    auto bond_leg_subs = register_bond_leg_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_leg_subs.begin()),
                std::make_move_iterator(bond_leg_subs.end()));

    auto bond_leg_amount_subs = register_bond_leg_amount_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_leg_amount_subs.begin()),
                std::make_move_iterator(bond_leg_amount_subs.end()));

    auto bond_leg_rate_subs = register_bond_leg_rate_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_leg_rate_subs.begin()),
                std::make_move_iterator(bond_leg_rate_subs.end()));

    auto bond_leg_amortization_subs = register_bond_leg_amortization_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_leg_amortization_subs.begin()),
                std::make_move_iterator(bond_leg_amortization_subs.end()));

    auto instrument_schedule_subs = register_instrument_schedule_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_schedule_subs.begin()),
                std::make_move_iterator(instrument_schedule_subs.end()));

    auto instrument_schedule_date_subs =
        register_instrument_schedule_date_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_schedule_date_subs.begin()),
                std::make_move_iterator(instrument_schedule_date_subs.end()));

    auto bond_option_subs = register_bond_option_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_option_subs.begin()),
                std::make_move_iterator(bond_option_subs.end()));

    auto instrument_option_subs = register_instrument_option_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_option_subs.begin()),
                std::make_move_iterator(instrument_option_subs.end()));

    auto instrument_option_premium_subs =
        register_instrument_option_premium_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_option_premium_subs.begin()),
                std::make_move_iterator(instrument_option_premium_subs.end()));

    auto instrument_option_exercise_fee_subs =
        register_instrument_option_exercise_fee_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_option_exercise_fee_subs.begin()),
                std::make_move_iterator(instrument_option_exercise_fee_subs.end()));

    auto instrument_option_payment_date_subs =
        register_instrument_option_payment_date_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_option_payment_date_subs.begin()),
                std::make_move_iterator(instrument_option_payment_date_subs.end()));

    auto instrument_strike_subs = register_instrument_strike_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_strike_subs.begin()),
                std::make_move_iterator(instrument_strike_subs.end()));

    auto bond_forward_subs = register_bond_forward_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_forward_subs.begin()),
                std::make_move_iterator(bond_forward_subs.end()));

    auto bond_future_subs = register_bond_future_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_future_subs.begin()),
                std::make_move_iterator(bond_future_subs.end()));

    auto bond_future_delivery_basket_subs =
        register_bond_future_delivery_basket_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_future_delivery_basket_subs.begin()),
                std::make_move_iterator(bond_future_delivery_basket_subs.end()));

    auto bond_trs_subs = register_bond_trs_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_trs_subs.begin()),
                std::make_move_iterator(bond_trs_subs.end()));

    auto bond_repo_subs = register_bond_repo_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(bond_repo_subs.begin()),
                std::make_move_iterator(bond_repo_subs.end()));

    auto ascot_subs = register_ascot_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(ascot_subs.begin()),
                std::make_move_iterator(ascot_subs.end()));

    return subs;
}

}
