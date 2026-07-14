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
#ifndef ORES_REFDATA_API_MESSAGING_INSTRUMENT_CODE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_INSTRUMENT_CODE_PROTOCOL_HPP

#include "ores.refdata.api/domain/instrument_code.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct get_instrument_codes_request {
    using response_type = struct get_instrument_codes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.instrument_codes.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_instrument_codes_response {
    std::vector<ores::refdata::domain::instrument_code> instruments;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_instrument_code_request {
    using response_type = struct save_instrument_code_response;
    static constexpr std::string_view nats_subject = "refdata.v1.instrument_codes.save";
    ores::refdata::domain::instrument_code data;

    static save_instrument_code_request from(ores::refdata::domain::instrument_code v) {
        return {.data = std::move(v)};
    }
};

struct save_instrument_code_response {
    bool success = false;
    std::string message;
};

struct delete_instrument_code_request {
    using response_type = struct delete_instrument_code_response;
    static constexpr std::string_view nats_subject = "refdata.v1.instrument_codes.delete";
    std::vector<std::string> codes;
};

struct delete_instrument_code_response {
    bool success = false;
    std::string message;
};

struct get_instrument_code_history_request {
    using response_type = struct get_instrument_code_history_response;
    static constexpr std::string_view nats_subject = "refdata.v1.instrument_codes.history";
    std::string code;
};

struct get_instrument_code_history_response {
    std::vector<ores::refdata::domain::instrument_code> history;
    bool success = false;
    std::string message;
};

}

#endif
