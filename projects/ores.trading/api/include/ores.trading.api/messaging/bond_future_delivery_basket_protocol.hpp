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
#ifndef ORES_TRADING_API_MESSAGING_BOND_FUTURE_DELIVERY_BASKET_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_FUTURE_DELIVERY_BASKET_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_future_delivery_basket.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct get_bond_future_delivery_baskets_request {
    using response_type = struct get_bond_future_delivery_baskets_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_future_delivery_baskets.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_bond_future_delivery_baskets_response {
    std::vector<ores::trading::domain::bond_future_delivery_basket> delivery_basket_ids;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_bond_future_delivery_basket_request {
    using response_type = struct save_bond_future_delivery_basket_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_future_delivery_baskets.save";
    ores::trading::domain::bond_future_delivery_basket data;

    static save_bond_future_delivery_basket_request
    from(ores::trading::domain::bond_future_delivery_basket v) {
        return {.data = std::move(v)};
    }
};

struct save_bond_future_delivery_basket_response {
    bool success = false;
    std::string message;
};

struct delete_bond_future_delivery_basket_request {
    using response_type = struct delete_bond_future_delivery_basket_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_future_delivery_baskets.delete";
    std::vector<std::string> ids;
    std::vector<std::string> sequence_numbers;
};

struct delete_bond_future_delivery_basket_response {
    bool success = false;
    std::string message;
};

struct get_bond_future_delivery_basket_history_request {
    using response_type = struct get_bond_future_delivery_basket_history_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_future_delivery_baskets.history";
    std::string instrument_id;
    std::string sequence_number;
};

struct get_bond_future_delivery_basket_history_response {
    std::vector<ores::trading::domain::bond_future_delivery_basket> history;
    bool success = false;
    std::string message;
};

}

#endif
