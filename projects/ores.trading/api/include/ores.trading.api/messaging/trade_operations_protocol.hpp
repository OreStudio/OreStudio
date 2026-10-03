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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_MESSAGING_TRADE_OPERATIONS_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_OPERATIONS_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_anchor.hpp"
#include "ores.trading.api/domain/trade_booking.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <string>

namespace ores::trading::messaging {

/**
 * @brief Books a trade: its anchor, its booking and its first state.
 *
 * The three rows are written in one transaction. The booking's trade id,
 * party and counterparty are taken from the anchor, so the caller states
 * them once.
 */
struct book_trade_request {
    using response_type = struct book_trade_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.book";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The trade's immutable facts.
     */
    ores::trading::domain::trade_anchor anchor;
    /**
     * @brief Where the trade is booked. Its trade id, party and counterparty
     * are replaced by the anchor's.
     */
    ores::trading::domain::trade_booking booking;
    /**
     * @brief The activity that books the trade, which names the transition
     * that starts its state: new_booking for a live trade, draft_capture for a
     * draft.
     */
    std::string activity_type_code;
};

/**
 * @brief The outcome of booking a trade.
 */
struct book_trade_response {
    /**
     * @brief Outcome of the operation: conflict with code already_exists when
     * the trade id is already booked.
     */
    ores::utility::domain::result result;
};

}

#endif
