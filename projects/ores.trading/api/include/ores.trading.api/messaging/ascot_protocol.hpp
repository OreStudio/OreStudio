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
#ifndef ORES_TRADING_API_MESSAGING_ASCOT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_ASCOT_PROTOCOL_HPP

#include "ores.trading.api/domain/ascot.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct get_ascots_request {
    using response_type = struct get_ascots_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_ascots_response {
    std::vector<ores::trading::domain::ascot> ascots;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_ascot_request {
    using response_type = struct save_ascot_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.save";
    ores::trading::domain::ascot data;

    static save_ascot_request from(ores::trading::domain::ascot v) {
        return {.data = std::move(v)};
    }
};

struct save_ascot_response {
    bool success = false;
    std::string message;
};

struct delete_ascot_request {
    using response_type = struct delete_ascot_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.delete";
    std::vector<std::string> ids;
};

struct delete_ascot_response {
    bool success = false;
    std::string message;
};

struct get_ascot_history_request {
    using response_type = struct get_ascot_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.ascots.history";
    std::string instrument_id;
};

struct get_ascot_history_response {
    std::vector<ores::trading::domain::ascot> history;
    bool success = false;
    std::string message;
};

}

#endif
