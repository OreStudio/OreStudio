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
import type { Trade } from '../domain/trade.js';
import type { TradeBooking } from '../domain/trade_booking.js';
import type { InstrumentBatch } from '../../../generated/trading/instrument_batch.js';
import type { Result } from '../../../utility/protocol.js';
import type { TradeEnvelopeData } from '../../../trading/payload.js';

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
    anchor: Trade;
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
    /**
     * @brief The activity that booked the trade; absent when the booking was
     * refused.
     */
    activity_id: string | null;
}

/**
 * @brief One trade's anchor, its ORE identifier and its envelope, as an
 * export writes it.
 *
 * The trade's instrument is not here. The export carries it in the batch's
 * typed array for the table the trade type routes to, and every element of
 * that array names the trade id this item states, so a reader joins the two
 * without an element tag.
 */
export interface TradeExportItem {
    /**
     * @brief The trade's immutable facts.
     */
    anchor: Trade;
    /**
     * @brief The trade's ORE identifier, or its id when it has none.
     */
    ore_id: string;
    /**
     * @brief The names the trade's source used: counterparty, netting set, portfolios and additional fields. Absent when the trade has none.
     */
    envelope: TradeEnvelopeData | null;
}

/**
 * @brief Exports the trades under a taxonomy node.
 *
 * The node is a book, a portfolio or a business unit, resolved to its books on
 * the server; the trades are those booked in them.
 */
export interface ExportPortfolioRequest {
    /**
     * @brief The book, portfolio or business unit to export.
     */
    node_id: string;
    /**
     * @brief The first trade to export, in trade id order.
     */
    offset: number;
    /**
     * @brief The most trades to export.
     */
    limit: number;
}

/**
 * @brief The exported trades.
 */
export interface ExportPortfolioResponse {
    /**
     * @brief Whether the export ran.
     */
    success: boolean;
    /**
     * @brief Why the export failed, when it did.
     */
    message: string;
    /**
     * @brief The trades, in trade id order.
     */
    items: TradeExportItem[];
    /**
     * @brief The trades' instruments, one typed array per instrument entity and
     * per child table, every element keyed by its trade id.
     *
     * An instrument is not a field of the item above: a std::variant cannot cross
     * the wire, because reflect-cpp names no alternative, and an encoded payload
     * would be an untyped field. So the tag sits on the container: a reader takes
     * the trade's type from the anchor, routes it with instrument_table_for, and
     * joins the array that table names by the trade id.
     */
    instruments: InstrumentBatch;
}

/**
 * @brief Exports the trades booked in a set of books to object storage.
 *
 * The handler serialises the export items to MsgPack, compresses them with
 * gzip and uploads them, so a report run passes a storage key rather than the
 * trades through NATS.
 */
export interface ExportTradesToStorageRequest {
    /**
     * @brief The books whose trades to export.
     */
    book_ids: string[];
    /**
     * @brief The target bucket: the platform bucket, ores.
     */
    storage_bucket: string;
    /**
     * @brief The target key, such as reporting/runs/{instance_id}/trades.msgpack.
     */
    storage_key: string;
}

/**
 * @brief The outcome of an export to storage.
 */
export interface ExportTradesToStorageResponse {
    /**
     * @brief Whether the export ran.
     */
    success: boolean;
    /**
     * @brief Why the export failed, when it did.
     */
    message: string;
    /**
     * @brief How many trades were written.
     */
    trade_count: number;
    /**
     * @brief The key written, echoed from the request.
     */
    storage_key: string;
}

export const subjects = {
    book_trade_request: 'trading.v1.ops.book_trade',
    export_portfolio_request: 'trading.v1.ops.export_portfolio',
    export_trades_to_storage_request: 'trading.v1.ops.export_trades_to_storage',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    book_trade_request: true,
    export_portfolio_request: true,
    export_trades_to_storage_request: true,
} as const;
