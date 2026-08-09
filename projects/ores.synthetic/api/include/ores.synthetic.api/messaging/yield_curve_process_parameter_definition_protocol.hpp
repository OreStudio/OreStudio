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
#ifndef ORES_SYNTHETIC_API_MESSAGING_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_PROTOCOL_HPP

#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

struct get_yield_curve_process_parameter_definitions_request {
    using response_type = struct get_yield_curve_process_parameter_definitions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_yield_curve_process_parameter_definitions_response {
    std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition>
        parameter_definitions;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_yield_curve_process_parameter_definition_request {
    using response_type = struct save_yield_curve_process_parameter_definition_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions.save";
    ores::synthetic::domain::yield_curve_process_parameter_definition data;

    static save_yield_curve_process_parameter_definition_request
    from(ores::synthetic::domain::yield_curve_process_parameter_definition v) {
        return {.data = std::move(v)};
    }
};

struct save_yield_curve_process_parameter_definition_response {
    bool success = false;
    std::string message;
};

struct delete_yield_curve_process_parameter_definition_request {
    using response_type = struct delete_yield_curve_process_parameter_definition_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions.delete";
    std::vector<std::string> ids;
};

struct delete_yield_curve_process_parameter_definition_response {
    bool success = false;
    std::string message;
};

struct get_yield_curve_process_parameter_definition_history_request {
    using response_type = struct get_yield_curve_process_parameter_definition_history_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions.history";
    std::string id;
};

struct get_yield_curve_process_parameter_definition_history_response {
    std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition> history;
    bool success = false;
    std::string message;
};

}

#endif
