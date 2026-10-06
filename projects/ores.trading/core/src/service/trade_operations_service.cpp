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
#include "ores.trading.core/service/trade_operations_service.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.trading.core/repository/trade_activity_repository.hpp"
#include "ores.trading.core/repository/trade_booking_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/repository/trade_state_repository.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string>

namespace ores::trading::service {

using namespace ores::logging;

trade_operations_service::trade_operations_service(context ctx)
    : ctx_(std::move(ctx)) {}

messaging::book_trade_response
trade_operations_service::book_trade(const messaging::book_trade_request& request) {
    using ores::database::repository::unit_of_work;
    using ores::service::messaging::stamp;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition;
    using ores::utility::domain::precondition_kind;

    auto anchor = request.anchor;
    auto booking = request.booking;
    stamp(anchor, ctx_);
    stamp(booking, ctx_);
    BOOST_LOG_SEV(lg(), debug) << "Booking trade " << anchor.id;

    messaging::book_trade_response response;

    // One transaction writes the anchor, the activity, the booking and the
    // state, so a failure in any of them leaves none of them. A read made
    // through the transaction joins it without ending it.
    unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();

    repository::trade_repository trades;
    if (!trades.read_latest(ctx, boost::uuids::to_string(anchor.id)).empty()) {
        response.result.outcome = outcome::conflict;
        response.result.code = "already_exists";
        response.result.message =
            "Trade " + boost::uuids::to_string(anchor.id) + " is already booked.";
        return response;
    }

    domain::trade_activity activity;
    activity.id = boost::uuids::random_generator()();
    activity.activity_type_code = request.activity_type_code;
    activity.actor = booking.modified_by;
    activity.occurred_at = booking.execution_timestamp.value_or(std::chrono::system_clock::now());
    activity.comment = booking.change_commentary;
    stamp(activity, ctx_);
    activity.party_id = anchor.party_id;

    booking.trade_id = anchor.id;
    booking.trade_activity_id = activity.id;
    booking.party_id = anchor.party_id;
    booking.counterparty_id = anchor.counterparty_id;
    booking.version = 0;

    domain::trade_state state;
    state.trade_id = anchor.id;
    state.trade_activity_id = activity.id;
    state.party_id = anchor.party_id;
    state.version = 0;
    state.modified_by = booking.modified_by;
    state.performed_by = booking.modified_by;
    state.change_reason_code = booking.change_reason_code;
    state.change_commentary = booking.change_commentary;
    stamp(state, ctx_);
    state.party_id = anchor.party_id;
    state.status_id = boost::uuids::uuid{};

    const precondition must_not_exist{precondition_kind::must_not_exist, std::nullopt};
    trades.write(ctx, anchor, must_not_exist);
    repository::trade_activity_repository{}.write(ctx, activity, must_not_exist);
    repository::trade_booking_repository{}.write(ctx, booking, must_not_exist);
    repository::trade_state_repository{}.write(ctx, state, must_not_exist);

    uow.commit();

    response.activity_id = activity.id;
    return response;
}

}
