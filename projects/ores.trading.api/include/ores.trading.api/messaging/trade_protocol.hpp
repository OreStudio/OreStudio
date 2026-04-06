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
#ifndef ORES_TRADING_MESSAGING_TRADE_PROTOCOL_HPP
#define ORES_TRADING_MESSAGING_TRADE_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.trading.api/domain/activity_type.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::trading::messaging {

struct get_activity_types_request {
    using response_type = struct get_activity_types_response;
    static constexpr std::string_view nats_subject = "trading.v1.activity_types.list";
};

struct get_activity_types_response {
    std::vector<ores::trading::domain::activity_type> activity_types;
};

struct get_trades_request {
    using response_type = struct get_trades_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.list";
    int offset = 0;
    int limit = 100;
    std::string book_id;
};

struct get_trades_response {
    std::vector<ores::trading::domain::trade> trades;
    int total_available_count = 0;
};

struct save_trade_request {
    using response_type = struct save_trade_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.save";
    std::vector<ores::trading::domain::trade> trades;

    static save_trade_request from(std::vector<ores::trading::domain::trade> trades) {
        return { .trades = std::move(trades) };
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
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::trade> versions;
};

// ---- Portfolio export ----

/**
 * @brief One trade plus its resolved instrument data for export.
 *
 * The instrument field is monostate when the trade has no linked instrument
 * or the product_type is unrecognised.
 */
struct trade_export_item {
    ores::trading::domain::trade trade;
    instrument_export_result instrument;
};

/**
 * @brief Request to export all trades (and instruments) for a portfolio.
 *
 * Supply exactly one of portfolio_id or book_id.
 */
struct export_portfolio_request {
    using response_type = struct export_portfolio_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trades.portfolio.export";
    std::string portfolio_id;
    std::string book_id;
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
    static constexpr std::string_view nats_subject =
        "trading.v1.trades.export-to-storage";

    std::vector<std::string> book_ids;
    std::string storage_bucket;  ///< Target bucket (e.g. "report-data")
    std::string storage_key;     ///< Target key (e.g. "{instance_id}/trades.msgpack")
};

struct export_trades_to_storage_response {
    bool success = false;
    std::string message;
    int trade_count = 0;
    std::string storage_key;    ///< Echoed back for confirmation
};

}

#endif
