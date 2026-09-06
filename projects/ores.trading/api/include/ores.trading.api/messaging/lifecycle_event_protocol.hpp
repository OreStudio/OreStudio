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
#ifndef ORES_TRADING_API_MESSAGING_LIFECYCLE_EVENT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_LIFECYCLE_EVENT_PROTOCOL_HPP

#include "ores.trading.api/domain/lifecycle_event.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct get_lifecycle_events_request {
    using response_type = struct get_lifecycle_events_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_lifecycle_events_response {
    std::vector<ores::trading::domain::lifecycle_event> events;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_lifecycle_event_request {
    using response_type = struct save_lifecycle_event_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events.save";
    ores::trading::domain::lifecycle_event data;

    static save_lifecycle_event_request from(ores::trading::domain::lifecycle_event v) {
        return {.data = std::move(v)};
    }
};

struct save_lifecycle_event_response {
    bool success = false;
    std::string message;
};

struct delete_lifecycle_event_request {
    using response_type = struct delete_lifecycle_event_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events.delete";
    std::vector<std::string> codes;
};

struct delete_lifecycle_event_response {
    bool success = false;
    std::string message;
};

struct get_lifecycle_event_history_request {
    using response_type = struct get_lifecycle_event_history_response;
    static constexpr std::string_view nats_subject = "trading.v1.lifecycle_events.history";
    std::string code;
};

struct get_lifecycle_event_history_response {
    std::vector<ores::trading::domain::lifecycle_event> history;
    bool success = false;
    std::string message;
};

}

#endif
