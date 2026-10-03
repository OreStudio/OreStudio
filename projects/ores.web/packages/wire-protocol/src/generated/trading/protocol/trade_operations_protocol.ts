/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { TradeAnchor } from '../domain/trade_anchor.js';
import type { TradeBooking } from '../domain/trade_booking.js';
import type { Result } from '../../../utility/protocol.js';

/**
 * @brief Books a trade: its anchor, its booking and its first state.
 *
 * The three rows are written in one transaction. The booking's trade id,
 * party and counterparty are taken from the anchor, so the caller states
 * them once.
 */
export interface BookTradeRequest {
    /**
     * @brief The trade's immutable facts.
     */
    anchor: TradeAnchor;
    /**
     * @brief Where the trade is booked. Its trade id, party and counterparty
     * are replaced by the anchor's.
     */
    booking: TradeBooking;
    /**
     * @brief The activity that books the trade, which names the transition
     * that starts its state: new_booking for a live trade, draft_capture for a
     * draft.
     */
    activity_type_code: string;
}

/**
 * @brief The outcome of booking a trade.
 */
export interface BookTradeResponse {
    /**
     * @brief Outcome of the operation: conflict with code already_exists when
     * the trade id is already booked.
     */
    result: Result;
}

export const subjects = {
    book_trade_request: 'trading.v1.trades.book',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    book_trade_request: true,
} as const;
