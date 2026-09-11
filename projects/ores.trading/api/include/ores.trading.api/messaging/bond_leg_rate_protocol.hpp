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
#ifndef ORES_TRADING_API_MESSAGING_BOND_LEG_RATE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_LEG_RATE_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_leg_rate.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct get_bond_leg_rates_request {
    using response_type = struct get_bond_leg_rates_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_bond_leg_rates_response {
    std::vector<ores::trading::domain::bond_leg_rate> bond_leg_rates;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_bond_leg_rate_request {
    using response_type = struct save_bond_leg_rate_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates.save";
    ores::trading::domain::bond_leg_rate data;

    static save_bond_leg_rate_request from(ores::trading::domain::bond_leg_rate v) {
        return {.data = std::move(v)};
    }
};

struct save_bond_leg_rate_response {
    bool success = false;
    std::string message;
};

struct delete_bond_leg_rate_request {
    using response_type = struct delete_bond_leg_rate_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates.delete";
    std::vector<std::string> ids;
    std::vector<std::string> leg_roles;
    std::vector<std::string> leg_numbers;
};

struct delete_bond_leg_rate_response {
    bool success = false;
    std::string message;
};

struct get_bond_leg_rate_history_request {
    using response_type = struct get_bond_leg_rate_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_rates.history";
    std::string instrument_id;
    std::string leg_role;
    std::string leg_number;
};

struct get_bond_leg_rate_history_response {
    std::vector<ores::trading::domain::bond_leg_rate> history;
    bool success = false;
    std::string message;
};

}

#endif
