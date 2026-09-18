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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ORE_API_MESSAGING_SERIES_KEY_SHAPE_PROTOCOL_HPP
#define ORES_ORE_API_MESSAGING_SERIES_KEY_SHAPE_PROTOCOL_HPP

#include "ores.ore.api/domain/series_key_shape.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::ore::messaging {

struct get_series_key_shapes_request {
    using response_type = struct get_series_key_shapes_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_series_key_shapes_response {
    std::vector<ores::ore::domain::series_key_shape> shapes;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_series_key_shape_request {
    using response_type = struct save_series_key_shape_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes.save";
    ores::ore::domain::series_key_shape data;

    static save_series_key_shape_request from(ores::ore::domain::series_key_shape v) {
        return {.data = std::move(v)};
    }
};

struct save_series_key_shape_response {
    bool success = false;
    std::string message;
};

struct delete_series_key_shape_request {
    using response_type = struct delete_series_key_shape_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes.delete";
    std::vector<std::string> series_types;
};

struct delete_series_key_shape_response {
    bool success = false;
    std::string message;
};

struct get_series_key_shape_history_request {
    using response_type = struct get_series_key_shape_history_response;
    static constexpr std::string_view nats_subject = "ore.v1.series_key_shapes.history";
    std::string series_type;
};

struct get_series_key_shape_history_response {
    std::vector<ores::ore::domain::series_key_shape> history;
    bool success = false;
    std::string message;
};

}

#endif
