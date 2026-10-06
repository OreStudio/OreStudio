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

#include "ores.trading.api/domain/instrument_payload.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_booking.hpp"
#include "ores.trading.api/domain/trade_envelope_data.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <optional>
#include <string>
#include <vector>

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
    ores::trading::domain::trade anchor;
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
    /**
     * @brief The activity that booked the trade; absent when the booking was
     * refused.
     */
    std::optional<boost::uuids::uuid> activity_id;
};

/**
 * @brief One trade with its resolved instrument and envelope, as an export
 * writes it.
 *
 * The instrument is carried as a payload rather than as a trade_instrument
 * variant: reflect-cpp cannot name the active alternative of that variant
 * (see instrument_payload). The payload's type is empty when the trade has no
 * instrument.
 */
struct trade_export_item {
    /**
     * @brief The trade's immutable facts.
     */
    ores::trading::domain::trade anchor;
    /**
     * @brief The trade's ORE identifier, or its id when it has none.
     */
    std::string ore_id;
    /**
     * @brief The trade's instrument, encoded.
     */
    ores::trading::domain::instrument_payload instrument;
    /**
     * @brief The names the trade's source used: counterparty, netting set, portfolios and
     * additional fields. Absent when the trade has none.
     */
    std::optional<ores::trading::domain::trade_envelope_data> envelope;
};

/**
 * @brief Exports the trades under a taxonomy node.
 *
 * The node is a book, a portfolio or a business unit, resolved to its books on
 * the server; the trades are those booked in them.
 */
struct export_portfolio_request {
    using response_type = struct export_portfolio_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.portfolio.export";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The book, portfolio or business unit to export.
     */
    std::string node_id;
    /**
     * @brief The first trade to export, in trade id order.
     */
    int offset = 0;
    /**
     * @brief The most trades to export.
     */
    int limit = 10000;
};

/**
 * @brief The exported trades.
 */
struct export_portfolio_response {
    /**
     * @brief Whether the export ran.
     */
    bool success = false;
    /**
     * @brief Why the export failed, when it did.
     */
    std::string message;
    /**
     * @brief The trades, in trade id order.
     */
    std::vector<trade_export_item> items;
};

/**
 * @brief Exports the trades booked in a set of books to object storage.
 *
 * The handler serialises the export items to MsgPack, compresses them with
 * gzip and uploads them, so a report run passes a storage key rather than the
 * trades through NATS.
 */
struct export_trades_to_storage_request {
    using response_type = struct export_trades_to_storage_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.export-to-storage";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The books whose trades to export.
     */
    std::vector<std::string> book_ids;
    /**
     * @brief The target bucket: the platform bucket, ores.
     */
    std::string storage_bucket;
    /**
     * @brief The target key, such as reporting/runs/{instance_id}/trades.msgpack.
     */
    std::string storage_key;
};

/**
 * @brief The outcome of an export to storage.
 */
struct export_trades_to_storage_response {
    /**
     * @brief Whether the export ran.
     */
    bool success = false;
    /**
     * @brief Why the export failed, when it did.
     */
    std::string message;
    /**
     * @brief How many trades were written.
     */
    int trade_count = 0;
    /**
     * @brief The key written, echoed from the request.
     */
    std::string storage_key;
};

}

#endif
