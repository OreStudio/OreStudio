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
#ifndef ORES_DQ_API_MESSAGING_ARTEFACT_TYPE_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_ARTEFACT_TYPE_PROTOCOL_HPP

#include "ores.dq.api/domain/artefact_type.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct get_artefact_types_request {
    using response_type = struct get_artefact_types_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_artefact_types_response {
    std::vector<ores::dq::domain::artefact_type> types;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_artefact_type_request {
    using response_type = struct save_artefact_type_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types.save";
    ores::dq::domain::artefact_type data;

    static save_artefact_type_request from(ores::dq::domain::artefact_type v) {
        return {.data = std::move(v)};
    }
};

struct save_artefact_type_response {
    bool success = false;
    std::string message;
};

struct delete_artefact_type_request {
    using response_type = struct delete_artefact_type_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types.delete";
    std::vector<std::string> codes;
};

struct delete_artefact_type_response {
    bool success = false;
    std::string message;
};

struct get_artefact_type_history_request {
    using response_type = struct get_artefact_type_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.artefact_types.history";
    std::string code;
};

struct get_artefact_type_history_response {
    std::vector<ores::dq::domain::artefact_type> history;
    bool success = false;
    std::string message;
};

}

#endif
