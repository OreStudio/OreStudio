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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_PARTY_ROLE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_PARTY_ROLE_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_party_role.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct get_trade_party_roles_request {
    using response_type = struct get_trade_party_roles_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_trade_party_roles_response {
    std::vector<ores::trading::domain::trade_party_role> roles;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_trade_party_role_request {
    using response_type = struct save_trade_party_role_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles.save";
    ores::trading::domain::trade_party_role data;

    static save_trade_party_role_request from(ores::trading::domain::trade_party_role v) {
        return {.data = std::move(v)};
    }
};

struct save_trade_party_role_response {
    bool success = false;
    std::string message;
};

struct delete_trade_party_role_request {
    using response_type = struct delete_trade_party_role_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles.delete";
    std::vector<std::string> ids;
};

struct delete_trade_party_role_response {
    bool success = false;
    std::string message;
};

struct get_trade_party_role_history_request {
    using response_type = struct get_trade_party_role_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_party_roles.history";
    std::string id;
};

struct get_trade_party_role_history_response {
    std::vector<ores::trading::domain::trade_party_role> history;
    bool success = false;
    std::string message;
};

}

#endif
