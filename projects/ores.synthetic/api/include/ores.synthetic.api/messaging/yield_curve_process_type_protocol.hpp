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
#ifndef ORES_SYNTHETIC_API_MESSAGING_YIELD_CURVE_PROCESS_TYPE_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_YIELD_CURVE_PROCESS_TYPE_PROTOCOL_HPP

#include "ores.synthetic.api/domain/yield_curve_process_type.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

struct get_yield_curve_process_types_request {
    using response_type = struct get_yield_curve_process_types_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.yield_curve_process_types.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_yield_curve_process_types_response {
    std::vector<ores::synthetic::domain::yield_curve_process_type> process_types;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_yield_curve_process_type_request {
    using response_type = struct save_yield_curve_process_type_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.yield_curve_process_types.save";
    ores::synthetic::domain::yield_curve_process_type data;

    static save_yield_curve_process_type_request
    from(ores::synthetic::domain::yield_curve_process_type v) {
        return {.data = std::move(v)};
    }
};

struct save_yield_curve_process_type_response {
    bool success = false;
    std::string message;
};

struct delete_yield_curve_process_type_request {
    using response_type = struct delete_yield_curve_process_type_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_types.delete";
    std::vector<std::string> codes;
};

struct delete_yield_curve_process_type_response {
    bool success = false;
    std::string message;
};

struct get_yield_curve_process_type_history_request {
    using response_type = struct get_yield_curve_process_type_history_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_types.history";
    std::string code;
};

struct get_yield_curve_process_type_history_response {
    std::vector<ores::synthetic::domain::yield_curve_process_type> history;
    bool success = false;
    std::string message;
};

}

#endif
