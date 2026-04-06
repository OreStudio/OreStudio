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
#ifndef ORES_ANALYTICS_MESSAGING_PRICING_ENGINE_TYPE_PROTOCOL_HPP
#define ORES_ANALYTICS_MESSAGING_PRICING_ENGINE_TYPE_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.analytics.api/domain/pricing_engine_type.hpp"

namespace ores::analytics::messaging {

struct get_pricing_engine_types_request {
    using response_type = struct get_pricing_engine_types_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_engine_types.list";
};

struct get_pricing_engine_types_response {
    std::vector<ores::analytics::domain::pricing_engine_type> types;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_pricing_engine_type_request {
    using response_type = struct save_pricing_engine_type_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_engine_types.save";
    ores::analytics::domain::pricing_engine_type data;
};

struct save_pricing_engine_type_response {
    bool success = false;
    std::string message;
};

struct delete_pricing_engine_type_request {
    using response_type = struct delete_pricing_engine_type_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_engine_types.delete";
    std::vector<std::string> codes;
};

struct delete_pricing_engine_type_response {
    bool success = false;
    std::string message;
};

struct get_pricing_engine_type_history_request {
    using response_type = struct get_pricing_engine_type_history_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_engine_types.history";
    std::string code;
};

struct get_pricing_engine_type_history_response {
    std::vector<ores::analytics::domain::pricing_engine_type> types;
    bool success = false;
    std::string message;
};

}

#endif
