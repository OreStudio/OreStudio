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
#ifndef ORES_MARKETDATA_API_MESSAGING_TENOR_CONVENTION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_TENOR_CONVENTION_PROTOCOL_HPP

#include "ores.marketdata.api/domain/tenor_convention.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct get_tenor_conventions_request {
    using response_type = struct get_tenor_conventions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.tenor_conventions.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_tenor_conventions_response {
    std::vector<ores::marketdata::domain::tenor_convention> conventions;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_tenor_convention_request {
    using response_type = struct save_tenor_convention_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.tenor_conventions.save";
    ores::marketdata::domain::tenor_convention data;

    static save_tenor_convention_request from(ores::marketdata::domain::tenor_convention v) {
        return {.data = std::move(v)};
    }
};

struct save_tenor_convention_response {
    bool success = false;
    std::string message;
};

struct delete_tenor_convention_request {
    using response_type = struct delete_tenor_convention_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.tenor_conventions.delete";
    std::vector<std::string> codes;
};

struct delete_tenor_convention_response {
    bool success = false;
    std::string message;
};

struct get_tenor_convention_history_request {
    using response_type = struct get_tenor_convention_history_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.tenor_conventions.history";
    std::string code;
};

struct get_tenor_convention_history_response {
    std::vector<ores::marketdata::domain::tenor_convention> history;
    bool success = false;
    std::string message;
};

}

#endif
