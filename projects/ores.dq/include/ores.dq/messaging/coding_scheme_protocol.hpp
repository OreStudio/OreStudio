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
#ifndef ORES_DQ_MESSAGING_CODING_SCHEME_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_CODING_SCHEME_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.dq/domain/coding_scheme.hpp"
#include "ores.dq/domain/coding_scheme_authority_type.hpp"

namespace ores::dq::messaging {

// =============================================================================
// Coding Scheme Authority Type Protocol
// =============================================================================

struct get_coding_scheme_authority_types_request {
    using response_type = struct get_coding_scheme_authority_types_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.coding-scheme-authority-types.list";
    int offset = 0;
    int limit = 100;
};

struct get_coding_scheme_authority_types_response {
    std::vector<ores::dq::domain::coding_scheme_authority_type>
        coding_scheme_authority_types;
    int total_available_count = 0;
};

struct save_coding_scheme_authority_type_request {
    using response_type = struct save_coding_scheme_authority_type_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.coding-scheme-authority-types.save";
    ores::dq::domain::coding_scheme_authority_type data;
};

struct save_coding_scheme_authority_type_response {
    bool success = false;
    std::string message;
};

struct delete_coding_scheme_authority_type_request {
    using response_type = struct delete_coding_scheme_authority_type_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.coding-scheme-authority-types.delete";
    std::vector<std::string> types;
};

struct delete_coding_scheme_authority_type_response {
    bool success = false;
    std::string message;
};

struct get_coding_scheme_authority_type_history_request {
    using response_type = struct get_coding_scheme_authority_type_history_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.coding-scheme-authority-types.history";
    std::string type;
};

struct get_coding_scheme_authority_type_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::coding_scheme_authority_type> history;
};

// =============================================================================
// Coding Scheme Protocol
// =============================================================================

struct get_coding_schemes_request {
    using response_type = struct get_coding_schemes_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.coding-schemes.list";
    int offset = 0;
    int limit = 100;
};

struct get_coding_schemes_response {
    std::vector<ores::dq::domain::coding_scheme> coding_schemes;
    int total_available_count = 0;
};

struct save_coding_scheme_request {
    using response_type = struct save_coding_scheme_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.coding-schemes.save";
    ores::dq::domain::coding_scheme data;
};

struct save_coding_scheme_response {
    bool success = false;
    std::string message;
};

struct delete_coding_scheme_request {
    using response_type = struct delete_coding_scheme_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.coding-schemes.delete";
    std::vector<std::string> codes;
};

struct delete_coding_scheme_response {
    bool success = false;
    std::string message;
};

struct get_coding_scheme_history_request {
    using response_type = struct get_coding_scheme_history_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.coding-schemes.history";
    std::string code;
};

struct get_coding_scheme_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::coding_scheme> history;
};

}

#endif
