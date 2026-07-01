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
#ifndef ORES_MARKETDATA_API_MESSAGING_FEED_BINDING_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_FEED_BINDING_PROTOCOL_HPP

#include "ores.marketdata.api/domain/feed_binding.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct get_feed_bindings_request {
    using response_type = struct get_feed_bindings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_feed_bindings_response {
    std::vector<ores::marketdata::domain::feed_binding> feed_bindings;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_feed_binding_request {
    using response_type = struct save_feed_binding_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings.save";
    ores::marketdata::domain::feed_binding data;

    static save_feed_binding_request from(ores::marketdata::domain::feed_binding v) {
        return {.data = std::move(v)};
    }
};

struct save_feed_binding_response {
    bool success = false;
    std::string message;
};

struct delete_feed_binding_request {
    using response_type = struct delete_feed_binding_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings.delete";
    std::vector<std::string> ids;
};

struct delete_feed_binding_response {
    bool success = false;
    std::string message;
};

struct get_feed_binding_history_request {
    using response_type = struct get_feed_binding_history_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_bindings.history";
    std::string id;
};

struct get_feed_binding_history_response {
    std::vector<ores::marketdata::domain::feed_binding> history;
    bool success = false;
    std::string message;
};

}

#endif
