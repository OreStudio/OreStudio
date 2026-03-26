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
#ifndef ORES_TRADING_MESSAGING_DAY_COUNT_FRACTION_TYPE_PROTOCOL_HPP
#define ORES_TRADING_MESSAGING_DAY_COUNT_FRACTION_TYPE_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.trading.api/domain/day_count_fraction_type.hpp"

namespace ores::trading::messaging {

struct get_day_count_fraction_types_request {
    using response_type = struct get_day_count_fraction_types_response;
    static constexpr std::string_view nats_subject = "trading.v1.day-count-fraction-types.list";
    int offset = 0;
    int limit = 100;
};

struct get_day_count_fraction_types_response {
    std::vector<ores::trading::domain::day_count_fraction_type> types;
    int total_available_count = 0;
};

struct save_day_count_fraction_type_request {
    using response_type = struct save_day_count_fraction_type_response;
    static constexpr std::string_view nats_subject = "trading.v1.day-count-fraction-types.save";
    ores::trading::domain::day_count_fraction_type data;
};

struct save_day_count_fraction_type_response {
    bool success = false;
    std::string message;
};

struct delete_day_count_fraction_type_request {
    using response_type = struct delete_day_count_fraction_type_response;
    static constexpr std::string_view nats_subject = "trading.v1.day-count-fraction-types.delete";
    std::vector<std::string> codes;
};

struct delete_day_count_fraction_type_response {
    bool success = false;
    std::string message;
};

struct get_day_count_fraction_type_history_request {
    using response_type = struct get_day_count_fraction_type_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.day-count-fraction-types.history";
    std::string code;
};

struct get_day_count_fraction_type_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::trading::domain::day_count_fraction_type> history;
};

}

#endif
