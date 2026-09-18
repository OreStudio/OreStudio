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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_PROTOCOL_HPP

#include "ores.trading.api/domain/activity_type.hpp"
#include "ores.trading.api/domain/instrument_payload.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_envelope_data.hpp"
#include "ores.trading.api/domain/trade_instrument.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct get_trades_request {
    using response_type = struct get_trades_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    std::string node_id;
};

struct get_trades_response {
    std::vector<ores::trading::domain::trade> trades;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_trade_request {
    using response_type = struct save_trade_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.save";
    std::vector<ores::trading::domain::trade> trades;

    static save_trade_request from(std::vector<ores::trading::domain::trade> v) {
        return {.trades = std::move(v)};
    }
};

struct save_trade_response {
    bool success = false;
    std::string message;
};

struct delete_trade_request {
    using response_type = struct delete_trade_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.delete";
    std::vector<std::string> ids;
};

struct delete_trade_response {
    bool success = false;
    std::string message;
};

struct get_trade_history_request {
    using response_type = struct get_trade_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.history";
    std::string id;
};

struct get_trade_history_response {
    std::vector<ores::trading::domain::trade> history;
    bool success = false;
    std::string message;
};

struct get_activity_types_request {
    using response_type = struct get_activity_types_response;
    static constexpr std::string_view nats_subject = "trading.v1.activity_types.list";
};

struct get_activity_types_response {
    std::vector<ores::trading::domain::activity_type> activity_types;
};

/**
 * @brief One trade plus its resolved instrument data.
 *
 * The instrument is carried as a payload rather than as a trade_instrument
 * variant. reflect-cpp cannot name the active alternative of that variant:
 * untagged, the first alternative std::monostate parses from any payload and
 * wins; tagged, rfl::AddTagsToVariants exceeds the fold limits on macOS and
 * MSVC. See instrument_payload. The payload's type is empty when the trade has
 * no linked instrument or the product_type is unrecognised.
 *
 * The envelope holds the trade-level data the product tables do not: the
 * counterparty name, the netting set id, the portfolio id labels and the
 * document's additional fields. It is absent when the trade has none.
 */
struct trade_export_item {
    ores::trading::domain::trade trade;
    ores::trading::domain::instrument_payload instrument;
    std::optional<ores::trading::domain::trade_envelope_data> envelope;
};

struct get_trade_instrument_request {
    using response_type = struct get_trade_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.instrument";
    std::string trade_id;
};

struct get_trade_instrument_response {
    bool success = false;
    std::string message;
    ores::trading::domain::trade trade;
    ores::trading::domain::trade_instrument instrument;
};


/**
 * @brief Request to export all trades (and instruments) under a taxonomy node.
 *
 * @p node_id is resolved by the server to the book-id set just like
 * get_trades_request; typical callers supply a portfolio id (to export the
 * whole portfolio subtree) or a book id (to export a single book).
 */
struct export_portfolio_request {
    using response_type = struct export_portfolio_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.portfolio.export";
    std::string node_id;
    int offset = 0;
    int limit = 10000;
};

struct export_portfolio_response {
    bool success = false;
    std::string message;
    std::vector<trade_export_item> items;
};

/**
 * @brief Exports trades for the given book IDs to object storage.
 *
 * The handler resolves trade IDs via ores_trading_get_trade_ids_by_books_fn,
 * loads full trade_export_items, serialises to MsgPack, compresses with gzip,
 * and uploads to storage. Returns the storage key and trade count.
 *
 * Used by the report execution workflow to offload large trade data sets
 * to storage instead of passing them through NATS.
 */
struct export_trades_to_storage_request {
    using response_type = struct export_trades_to_storage_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.export-to-storage";

    std::vector<std::string> book_ids;
    std::string storage_bucket; ///< Target bucket (e.g. "report-data")
    std::string storage_key;    ///< Target key (e.g. "{instance_id}/trades.msgpack")
};

struct export_trades_to_storage_response {
    bool success = false;
    std::string message;
    int trade_count = 0;
    std::string storage_key; ///< Echoed back for confirmation
};
}

#endif
