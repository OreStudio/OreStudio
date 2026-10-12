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
#ifndef ORES_ANALYTICS_API_MESSAGING_PUBLISH_OPERATION_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_PUBLISH_OPERATION_PROTOCOL_HPP

#include <string_view>

namespace ores::analytics::messaging {

/**
 * @brief The workflow step that publishes a DQ-cleared pricing engines document.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is ores_analytics_publish_pricing_engines_from_dq_fn.
 */
struct publish_pricing_engines_from_dq_request {
    static constexpr std::string_view nats_subject =
        "analytics.v1.ops.publish_pricing_engines_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The workflow step that publishes a DQ-cleared today's market document.
 *
 * The same trigger shape as publish_pricing_engines_from_dq_request. The SQL function
 * the handler calls is ores_analytics_publish_todays_market_from_dq_fn.
 */
struct publish_todays_market_from_dq_request {
    static constexpr std::string_view nats_subject =
        "analytics.v1.ops.publish_todays_market_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

}

#endif
