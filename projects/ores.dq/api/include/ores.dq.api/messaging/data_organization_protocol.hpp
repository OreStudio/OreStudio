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
#ifndef ORES_DQ_API_MESSAGING_DATA_ORGANIZATION_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_DATA_ORGANIZATION_PROTOCOL_HPP

#include "ores.dq.api/domain/methodology.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::dq::messaging {


// =============================================================================
// Methodology Protocol
// =============================================================================

struct get_methodologies_request {
    using response_type = struct get_methodologies_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.list";
    int offset = 0;
    int limit = 100;
};

struct get_methodologies_response {
    std::vector<ores::dq::domain::methodology> methodologies;
    int total_available_count = 0;
};

struct save_methodology_request {
    using response_type = struct save_methodology_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.save";
    ores::dq::domain::methodology data;
};

struct save_methodology_response {
    bool success = false;
    std::string message;
};

struct delete_methodology_request {
    using response_type = struct delete_methodology_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.delete";
    std::vector<std::string> codes;
};

struct delete_methodology_response {
    bool success = false;
    std::string message;
};

struct get_methodology_history_request {
    using response_type = struct get_methodology_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.history";
    std::string code;
};

struct get_methodology_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::methodology> history;
};

}

#endif
