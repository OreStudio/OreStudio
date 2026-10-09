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
#ifndef ORES_IAM_API_MESSAGING_GEO_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_GEO_OPERATIONS_PROTOCOL_HPP

#include <string>

namespace ores::iam::messaging {

/**
 * @brief Resolve one address to the country code it came from.
 *
 * The search is the caller's tenant's ranges. An address the tenant's ranges
 * do not cover is not found, which is an answer rather than an error: a
 * private address never resolves.
 */
struct lookup_country_request {
    using response_type = struct lookup_country_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.lookup_country";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string address;
};

struct lookup_country_response {
    std::string country_code;
    bool found = false;
    bool success = false;
    std::string message;
};

}

#endif
