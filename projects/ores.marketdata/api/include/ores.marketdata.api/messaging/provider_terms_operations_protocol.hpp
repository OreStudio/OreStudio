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
#ifndef ORES_MARKETDATA_API_MESSAGING_PROVIDER_TERMS_OPERATIONS_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_PROVIDER_TERMS_OPERATIONS_PROTOCOL_HPP

#include <chrono>
#include <optional>
#include <string>

namespace ores::marketdata::messaging {

/**
 * @brief Accepts the terms of a provider as they stand.
 *
 * The caller names the provider and nothing else: what is accepted is the terms
 * the service holds, by whom is the session's account, and when is the service's
 * clock. Terms that change afterwards no longer match, so they must be accepted
 * again.
 */
struct accept_provider_terms_request {
    using response_type = struct accept_provider_terms_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.accept_provider_terms";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The provider whose terms are accepted.
     */
    std::string provider_code;
};

struct accept_provider_terms_response {
    /**
     * @brief Whether the acceptance was recorded.
     */
    bool success = false;
    /**
     * @brief Why it was not, or what was recorded.
     */
    std::string message;
    /**
     * @brief The account recorded as accepting.
     */
    std::string accepted_by;
    /**
     * @brief When the acceptance was recorded.
     */
    std::optional<std::chrono::system_clock::time_point> accepted_at;
};

}

#endif
