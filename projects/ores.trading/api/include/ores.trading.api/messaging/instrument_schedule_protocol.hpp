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
#ifndef ORES_TRADING_API_MESSAGING_INSTRUMENT_SCHEDULE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_INSTRUMENT_SCHEDULE_PROTOCOL_HPP

#include "ores.trading.api/domain/instrument_schedule.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct get_instrument_schedules_request {
    using response_type = struct get_instrument_schedules_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_instrument_schedules_response {
    std::vector<ores::trading::domain::instrument_schedule> instrument_schedules;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_instrument_schedule_request {
    using response_type = struct save_instrument_schedule_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.save";
    ores::trading::domain::instrument_schedule data;

    static save_instrument_schedule_request from(ores::trading::domain::instrument_schedule v) {
        return {.data = std::move(v)};
    }
};

struct save_instrument_schedule_response {
    bool success = false;
    std::string message;
};

struct delete_instrument_schedule_request {
    using response_type = struct delete_instrument_schedule_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.delete";
    std::vector<std::string> ids;
    std::vector<std::string> owner_roles;
    std::vector<std::string> owner_numbers;
    std::vector<std::string> schedule_roles;
    std::vector<std::string> sequence_numbers;
};

struct delete_instrument_schedule_response {
    bool success = false;
    std::string message;
};

struct get_instrument_schedule_history_request {
    using response_type = struct get_instrument_schedule_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.history";
    std::string instrument_id;
    std::string owner_role;
    std::string owner_number;
    std::string schedule_role;
    std::string sequence_number;
};

struct get_instrument_schedule_history_response {
    std::vector<ores::trading::domain::instrument_schedule> history;
    bool success = false;
    std::string message;
};

}

#endif
