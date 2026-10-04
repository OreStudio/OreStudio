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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <stdexcept>
#include <string>

namespace ores::trading::service {

using namespace ores::logging;

namespace {

std::string optional_uuid(const std::optional<boost::uuids::uuid>& v) {
    return v ? boost::uuids::to_string(*v) : std::string();
}

}

trade_operations_service::trade_operations_service(context ctx)
    : ctx_(std::move(ctx)) {}

messaging::book_trade_response
trade_operations_service::book_trade(const messaging::book_trade_request& request) {
    using ores::database::repository::execute_parameterized_string_query;
    using ores::platform::time::datetime;
    using ores::service::messaging::stamp;
    using ores::utility::domain::outcome;

    auto anchor = request.anchor;
    auto booking = request.booking;
    stamp(anchor, ctx_);
    stamp(booking, ctx_);
    BOOST_LOG_SEV(lg(), debug) << "Booking trade " << anchor.id;

    const auto booked = execute_parameterized_string_query(
        ctx_,
        "SELECT ores_trading_book_trade_fn($1::uuid, $2::uuid, nullif($3, '')::uuid, $4, $5, "
        "$6, $7, $8::uuid, nullif($9, '')::uuid, $10::date, nullif($11, '')::timestamptz, "
        "$12, $13, $14, $15)::text",
        {boost::uuids::to_string(anchor.id),
         boost::uuids::to_string(anchor.party_id),
         optional_uuid(anchor.counterparty_id),
         anchor.trade_type,
         std::string(domain::to_string(anchor.counterparty_scope)),
         std::string(domain::to_string(anchor.booking_nature)),
         std::string(domain::to_string(anchor.entry_channel)),
         boost::uuids::to_string(booking.book_id),
         optional_uuid(booking.netting_set_id),
         datetime::to_iso8601_date(booking.trade_date),
         booking.execution_timestamp ? datetime::to_iso8601_utc(*booking.execution_timestamp) :
                                       std::string(),
         request.activity_type_code,
         booking.modified_by,
         booking.change_reason_code,
         booking.change_commentary},
        lg(),
        "Booking a trade");

    if (booked.size() != 1 || (booked.front() != "true" && booked.front() != "false"))
        throw std::runtime_error("Booking trade " + boost::uuids::to_string(anchor.id) +
                                 " returned no outcome.");

    messaging::book_trade_response response;
    if (booked.front() == "false") {
        response.result.outcome = outcome::conflict;
        response.result.code = "already_exists";
        response.result.message =
            "Trade " + boost::uuids::to_string(anchor.id) + " is already booked.";
    }
    return response;
}

}
