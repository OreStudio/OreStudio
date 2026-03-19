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
#include "ores.trading/domain/activity_type.hpp"
#include "ores.trading/domain/trade.hpp"

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

}

#endif
